{-# LANGUAGE RecordWildCards #-}
-- | Pairwise alignment with affine gap penalties and multi-sequence alignment. Forked from Data.Align.
module Affine
  (
  -- * Global and local alignment
  align
  , AlignConfig
  , alignConfig
  , Step
  , Trace, traceScore, trace
  , debugAlign, debugStrAlign
  -- * Multi-sequence alignment
  , centerStar
  , MultiStep, center, others, stepOfAll
  , MultiTrace, centerIndex, otherIndices, allIndices, multiTrace
  , debugMultiAlign
  ) where

import Control.Monad.Trans.State.Strict (evalState, gets, modify)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import qualified Data.Vector.Generic as G

-- | Either an unmatched item or a match.
type Step a = Either (Either a a) (a, a)

stepLeft :: a -> Either (Either a b1) b2
stepLeft = Left . Left

stepRight :: b1 -> Either (Either a b1) b2
stepRight = Left . Right

stepBoth :: a1 -> b -> Either a2 (a1, b)
stepBoth a b = Right (a,b)

-- | The result of the alignment.
data Trace a s = Trace
  { traceScore :: s
  , trace :: [Step a]
  }

instance (Show a, Show s) => Show (Trace a s) where
  show (Trace s t) = "Trace(score = " ++ show s ++ ", steps = " ++ show t ++ ")"

tappend :: Num s => Trace a s -> (s, Step a) -> Trace a s
Trace a b `tappend` (y,z) = Trace (a+y) (z:b)

data AffineTrace a s = AffineTrace {
    at_max :: Trace a s
  , at_left_gap :: Trace a s
  , at_right_gap :: Trace a s
  }

data AlignConfig a s = AlignConfig
  { acPairScore :: a -> a -> s
  , acGapOpen :: s
  , acGapExtension :: s
  }

alignConfig :: (a -> a -> s)   -- ^ Scoring function.
            -> s               -- ^ Gap opening score.
            -> s               -- ^ Gap extension score.
            -> AlignConfig a s
alignConfig = AlignConfig

-- | Aligns two sequences.
-- 
-- Char-based alignment
-- >>> :{
-- let tr = align (alignConfig (\a b -> if a == b then 1 else (-1)) 
--                             (-1) (-0.25)) 
--                (Data.Vector.fromList "circumambulate") 
--                (Data.Vector.fromList "perambulatory")
-- in do
--    print $ traceScore tr
--    putStrLn . debugAlign . trace $ tr
-- :}
--
-- -0.5
-- circumambulate--
-- per---ambulatory
--
-- String-based alignment
-- >>> :{
-- let tr = align (alignConfig (\a b -> if a == b then 1 else (-1)) 
--                             (-1) (-0.25)) 
--                (Data.Vector.fromList ["kra","ya","ṇā","ddha","ra","ṇā","tyā","cñā","yāḥ"]) 
--                (Data.Vector.fromList ["bha","ra","ṇā","da","pa","ha","ra","ṇā","tyā","cña","yā"])
-- in do
--    print $ traceScore tr
--    putStrLn . debugStrAlign . trace $ tr
-- :}
--
-- -3.25
-- |kra|ya|ṇā|ddha|--|--|ra|ṇā|tyā|cñā|yāḥ|
-- |bha|ra|ṇā|da  |pa|ha|ra|ṇā|tyā|cñā|yā |
align :: (G.Vector v a, Num s, Ord s)
  => AlignConfig a s
  -> v a  -- ^ Left sequence.
  -> v a  -- ^ Right sequence.
  -> Trace a s
align AlignConfig{..} as bs =
  let p = (lastIndex as, lastIndex bs)
  in revTrace . at_max $ evalState (go p) M.empty
  where
  revTrace (Trace s t) = Trace s (reverse t)
  lastIndex v = G.length v - 1
  --
  go p = do
    res <- gets $ M.lookup p
    case res of
        Just r -> return r
        Nothing -> do
            newRes <- pgo p
            modify (M.insert p newRes)
            return newRes
  --
  pgo (i,j)
    | i == (-1) || j == (-1) = return $
      if i == j then AffineTrace (Trace 0 []) (Trace 0 []) (Trace 0 [])
      else if i == (-1)
           then skipInit j stepRight bs
           else skipInit i stepLeft as
    | otherwise = do
      let a = as G.! i
          b = bs G.! j
      diag  <- go (i-1,j-1)
      let diag_max = (at_max diag) `tappend` (acPairScore a b, stepBoth a b)
      
      a_gaps <- go (i-1,  j)
      let a_gap1 = (at_max a_gaps) `tappend` (acGapOpen + acGapExtension, stepLeft a)
      let a_gap2 = (at_left_gap) a_gaps `tappend` (acGapExtension, stepLeft a)
      let a_gap_max = L.maximumBy (comparing traceScore) [a_gap1, a_gap2]
      
      b_gaps <- go (  i,j-1)
      let b_gap1 = (at_max b_gaps) `tappend` (acGapOpen + acGapExtension, stepRight b)
      let b_gap2 = (at_right_gap b_gaps) `tappend` (acGapExtension, stepRight b)
      let b_gap_max = L.maximumBy (comparing traceScore) [b_gap1, b_gap2]
      
      let maxi = L.maximumBy (comparing traceScore) [diag_max, a_gap_max, b_gap_max]
      return $ AffineTrace maxi a_gap_max b_gap_max
  --
  skipInit idx stepFun xs =
    let score = acGapOpen + acGapExtension * fromIntegral (idx+1)
        tr = reverse [stepFun (xs G.! xi) | xi <- [0..idx]]
    in AffineTrace (Trace score tr) (Trace score tr) (Trace score tr)

-- | Utility for displaying a Char-based alignment.
debugAlign :: [Step Char] -> String
debugAlign = go [] []
  where
  go as bs [] = reverse as ++ "\n" ++ reverse bs
  go as bs (t:ts) = case t of
    Left (Left c)  -> go (c:as) ('-':bs) ts
    Left (Right c) -> go ('-':as) (c:bs) ts
    Right (c, d)   -> go (c:as) (d:bs) ts

-- | Utility for displaying a String-based alignment.
debugStrAlign :: [Step String] -> String
debugStrAlign = go id id
    where
    go as bs [] = as "|\n" ++ bs "|"
    go as bs (t:ts) = case t of
        Left (Left c)   -> go (as . ("|"++) . (c ++)) 
                              (bs . ("|"++) . (take (length c) (repeat '-') ++)) ts
        Left (Right c)  -> go (as . ("|"++) . (take (length c) (repeat '-') ++)) 
                              (bs . ("|"++) . (c ++)) ts
        Right (c,d)     -> go (as . ("|"++) . (c ++) . (filldc ++)) 
                              (bs . ("|"++) . (d ++) . (fillcd ++)) ts
            where
            fill n = take n $ repeat ' '
            filldc = fill (length d - length c)
            fillcd = fill (length c - length d)

-- | A step in a multi-sequence alignment.
data MultiStep a = MultiStep
  { center :: Maybe a    -- ^ 'Nothing' means gap insertion.
  , others :: [Maybe a]  -- ^ Parallel to 'otherIndices'.
  }

-- | The result of a multi-sequence alignment.
data MultiTrace i a s = MultiTrace
  { centerIndex :: i
  , otherIndices :: [i]
  , multiTrace :: [MultiStep a]
  }

-- | The center step followed by other steps.
stepOfAll :: MultiStep a -> [Maybe a]
stepOfAll MultiStep{..} = center:others

-- | The center index followed by other indices.
allIndices :: MultiTrace i a s -> [i]
allIndices MultiTrace{..} = centerIndex:otherIndices

-- | Align multiple sequences using the Center Star method. See, for example, section 14.6.2, "A bounded-error approximation method for SP alignment" in Gusfield 1997: <https://doi.org/10.1017/CBO9780511574931>.
-- This algorithm uses an affine gap penalty model. See section 12.6 "Convex gap weights" in Gusfield 1997.
-- Assumes the list of sequences to have length > 1, and the indices to be unique.
centerStar :: (G.Vector v a, Num s, Ord s, Ord i)
  => AlignConfig a s
  -> [(i, v a)]  -- TODO use internal indices rather to make uniqueness sure
  -> MultiTrace i a s
centerStar conf vs =
  let (firstPair:rest) = centerPairs
      initialTrace = MultiTrace
        { centerIndex = fst . fst $ firstPair
        , otherIndices = [snd . fst $ firstPair]
        , multiTrace = initialSteps . trace . snd $ firstPair
        }
  in foldl mergePair initialTrace rest
  where
  initialSteps = go []
    where
    go acc [] = reverse acc
    go acc (s:xs) = go (conv s []:acc) xs
  --
  conv s rest = case s of
      Right (c, d) -> MultiStep (Just c) (Just d:rest)
      Left (Left c) -> MultiStep (Just c) (Nothing:rest)
      Left (Right d) -> MultiStep Nothing (Just d:rest)
  --
  mergePair MultiTrace{..} ((_,j), tr) = MultiTrace
    { centerIndex = centerIndex
    , otherIndices = j:otherIndices
    , multiTrace = mergeSteps multiTrace (trace tr)
    }
    where
    mergeSteps mss' = go [] mss'
      where
      noOthers = map (const Nothing) . others . head $ mss'
      --
      go acc [] [] = reverse acc
      go acc (MultiStep{..}:mss) [] =
        go (MultiStep center (Nothing:others):acc) mss []
      go acc [] (s:ss) = go (conv s noOthers:acc) [] ss
      go acc (m@MultiStep{..}:mss) (s:ss) = case (center, s) of
        (Nothing, Left (Right d)) ->
          go (MultiStep center (Just d:others):acc) mss ss
        (Nothing, _) ->
          go (MultiStep center (Nothing:others):acc) mss (s:ss)
        (Just _, Right (_, d)) ->
          go (MultiStep center (Just d:others):acc) mss ss
        (Just _, Left (Left _)) ->
          go (MultiStep center (Nothing:others):acc) mss ss
        (Just _, Left (Right d)) ->
          go (MultiStep Nothing (Just d:noOthers):acc) (m:mss) ss
  --
  centerPairs
    = snd  -- drop cache
    . L.maximumBy (comparing fst)
    . map (\g -> (starSum g, g))  -- cache scores
    . L.groupBy ((==) `on` (fst . fst))
    . L.sortBy (comparing fst)
    $ pairAligns
    where
    pairAligns = do
      ((i,v):rest) <- L.tails vs
      (j,w) <- rest
      let tr = align conf v w
      [((i,j), tr), ((j,i), flipLR tr)]
      where
        flipLR tr = tr { trace = map go . trace $ tr }
          where
            go (Left (Left a)) = Left (Right a)
            go (Left (Right a)) = Left (Left a)
            go (Right (c,d)) = Right (d,c)    
    --
    starSum = sum . map (traceScore . snd)

-- | Renders a char-based multi-alignment result to a string.
debugMultiAlign :: [MultiStep Char] -> String
debugMultiAlign =
  unlines . map (map charOrDash) . L.transpose . map stepOfAll
  where
  charOrDash = fromMaybe '-'
