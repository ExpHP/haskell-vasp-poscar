{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

-- WARNING: NOT WELL-TESTED

module Data.Vasp.Poscar(
        Poscar(..),
        CoordSystem(..),
        ScaleLine(..),
        fromText,
        toText,
    ) where

import           "base" GHC.Generics
import           "base" Data.Foldable
import           "base" Data.Char
import           "base" Control.Monad
import           "base" Data.Void
import           "vector" Data.Vector(Vector, (!))
import qualified "vector" Data.Vector as Vector
import           "text" Data.Text(Text)
import qualified "text" Data.Text as Text
import qualified "text" Data.Text.Read as Text.Read
import           "mtl" Control.Monad.Identity(Identity(..))

type Matrix a = Vector (Vector a)
data CoordSystem = Cartesian | Direct deriving (Generic, Eq, Show, Read)
data ScaleLine = Volume Double | Scale Double deriving (Generic, Eq, Show, Read)

-- | A representation of a VASP POSCAR file.
--
-- This data structure is intended to have 1-1 feature-correspondence with
-- POSCAR files, and as such is not a very convenient form for actually
-- manipulating a crystal structure.
data Poscar = Poscar
                { comment :: Text
                , scale :: ScaleLine
                , lattice :: Matrix Double            -- 3 x 3, rows are vectors
                , positions :: Matrix Double          -- n x 3
                , velocities :: Maybe (Matrix Double) -- n x 3
                , dynamics :: Maybe (Matrix Bool)     -- n x 3
                , groupLabels :: Maybe (Vector Text)  -- m
                , groupCounts :: Vector Int           -- m
                , coords :: CoordSystem
                , predictorCorrector :: Maybe Void -- TODO
                } deriving (Generic, Eq, Show, Read)

truncateFrom :: Text -> Text -> Text
truncateFrom c = head . Text.splitOn c

fromText :: Text -> Poscar
fromText = nastyParse

toText :: Poscar -> Text
toText = toTextImpl


-- "nasty" parsers due to the nasty user experience...
nastyWordToDouble :: Text -> Double
nastyWordToDouble s = let Right (x, "") = Text.Read.double s in x
nastyWordToInt :: (Num b)=> Text -> b
nastyWordToInt s = fromIntegral $ let Right (x, "") = Text.Read.decimal s in x
nastyOnlyWord :: Text -> Text
nastyOnlyWord s = let [w] = Text.words s in w

parseDynamicFlag :: Text -> Bool
parseDynamicFlag "t" = True
parseDynamicFlag "T" = True
parseDynamicFlag _   = False

-- wherein I use the identity monad for name shadowing
-- (I'm expecting hate mail for this)
--
-- also, yes yes, I know, "who on earth would write parsing code in haskell and NOT use
--                         parser combinators!?", etc. etc.
nastyParse :: Text -> Poscar
nastyParse text = runIdentity $ do
    -- (deleting trailing newlines simplifies detection of velocity and... that other thing)
    text            <- pure $ Text.dropWhileEnd (== '\n') text
    lines           <- pure $ Text.lines text
    (comment:lines) <- pure lines

    lines           <- pure $ truncateFrom "#" . truncateFrom "!" <$> lines

    (scale:lines) <- pure lines
    scale <- pure $ case (nastyWordToDouble $ nastyOnlyWord scale) of
                x | x < 0 -> Volume (-x)
                x | x > 0 -> Scale x
                _ -> error "zero scale factor"

    (lattice, lines) <- pure $ splitAt 3 lines
    lattice <- pure . Vector.fromList $
               Vector.fromList . fmap nastyWordToDouble . Text.words <$> lattice

    (line:lines) <- pure lines
    line <- pure $ Text.words line
    (groupCounts, groupLabels, lines) <- pure $
        case Text.Read.decimal (head line) of
            Right (_, "") ->
                ( Vector.fromList $ nastyWordToInt <$> line
                , Nothing
                , lines
                )
            Left _ ->
                ( Vector.fromList $ nastyWordToInt <$> Text.words (head lines)
                , Just $ Vector.fromList $ line
                , tail lines
                )

    (line:_) <- pure lines
    (hasDynamics, lines) <- pure $ case toUpper (Text.head line) of
        'S' -> (True, tail lines)
        _   -> (False, lines)

    (line:lines) <- pure lines
    coords <- pure $ case toUpper (Text.head line) of
        'C' -> Cartesian
        'K' -> Cartesian
        _   -> Direct

    meaningfulCount <- pure $
        case (hasDynamics, length (Text.words (head lines))) of
            (True,  7) -> 6;   (True,  6) -> 6
            (False, 4) -> 3;   (False, 3) -> 3
            (_,n) -> error ("your poscar seems to have " ++ show n
                            ++ " words per line... what is this!?")

    let natoms = sum groupCounts

    let listsToMat = Vector.fromList . fmap Vector.fromList
    (something, lines) <- pure $ splitAt natoms lines
    something <- pure $ take meaningfulCount . Text.words <$> something
    positions <- pure $ listsToMat $ fmap nastyWordToDouble . take 3 <$> something
    dynamics <- pure $ case hasDynamics of
        False -> Nothing
        True  -> Just $ listsToMat $ fmap parseDynamicFlag . drop 3 <$> something

    -- optional velocities (preceded by a single blank line if present)
    -- (since trailing empty lines were truncated, the presence of any remaining lines
    --  means there is velocity)
    (velocities, lines) <- pure $ case lines of
        [] -> (Nothing, lines)

        ((Text.words -> []):lines) -> runIdentity $ do
            (velocities, lines) <- pure $ splitAt natoms lines
            velocities <- pure $ listsToMat $
                          fmap nastyWordToDouble . take 3 . Text.words <$> velocities
            pure (Just velocities, lines)

        (nonblank:_) -> error ("expected blank line at " ++ show nonblank)

    (predictorCorrector, lines) <- pure $ case lines of
        [] -> (Nothing, lines)
        ((Text.words -> []):lines) -> error "predictor corrector not supported" -- TODO
        (nonblank:_) -> error ("expected blank line at " ++ show nonblank)

    let [] = lines

    pure $ Poscar comment scale lattice positions velocities
                  dynamics groupLabels groupCounts coords predictorCorrector

fmtShowable :: (Show b)=> b -> Text
fmtShowable = Text.pack . show

fmtDouble :: Double -> Text
fmtDouble = fmtShowable

fmtInt :: Int -> Text
fmtInt = fmtShowable

fmtFlag :: Bool -> Text
fmtFlag False = "T"
fmtFlag True  = "F"

fmtScale :: ScaleLine -> Text
fmtScale (Volume v) = fmtDouble (-v)
fmtScale (Scale  v) = fmtDouble v

fmtCoords :: CoordSystem -> Text
fmtCoords Cartesian = "Cartesian"
fmtCoords Direct    = "Direct"

toTextImpl :: Poscar -> Text
toTextImpl poscar@Poscar
                { comment, scale, lattice, positions
                , velocities, dynamics, groupLabels, groupCounts
                , coords, predictorCorrector
                } =
    Text.unlines $ concat
        [ [comment]
        , [fmtScale scale]
        , matLines fmtDouble lattice
        , toList groupLabels >>= vecLines id
        , vecLines fmtInt groupCounts
        , toList dynamics >>= const ["Selective Dynamics"]
        , [fmtCoords coords]
        -- append word lists, THEN do unwords to get spacing right
        , fmap Text.unwords $ zipWith (++) plines dlines
        , toList velocities >>= const [""]
        , toList velocities >>= matLines fmtDouble
        , toList predictorCorrector >>= const [""]
        , toList predictorCorrector >>= error "predictor corrector not supported"
        ] where
            unmat = fmap toList . toList
            vecLines fmt vec = matLines fmt $ Vector.fromList [vec]
            matLines fmt mat = Text.unwords . fmap fmt <$> unmat mat
            plines = unmat $ fmap fmtDouble <$> positions
            dlines = case dynamics of
                Nothing -> cycle [[]]
                Just ds -> unmat $ fmap fmtFlag <$> ds
