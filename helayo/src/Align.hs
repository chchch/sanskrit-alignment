{-# LANGUAGE RecordWildCards #-}
-- | Collection of functions for global, local and multi-sequence alignment.
module Align
  (
  -- * Global and local alignment
    align
  , AlignConfig
  , alignConfig
  , Step
  , Trace, traceScore, trace
  -- * Align streams using sliding windows
  , windowedAlign
  -- * Multi-sequence alignment
  , centerStar
  , MultiStep, center, others, stepOfAll
  , MultiTrace, centerIndex, otherIndices, allIndices, multiTrace
  -- * Debugging and demonstration
  , debugAlign
  , debugMultiAlign
  ) where

import Control.Monad.Trans.State.Strict
import Data.Function (fix, on)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Ord
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import Control.Parallel.Strategies

data AlignConfig a s = AlignConfig
  { acPairScore :: a -> a -> s
  , ac_initial_gap_penalty :: s
  , ac_gap_penalty :: s
  }

-- | Configures the scores used when aligning.
-- The gap scores should be negative in order to be penalties.
alignConfig :: (a -> a -> s)  -- ^ Scoring function.
            -> s              -- ^ Initial gap score.
            -> s              -- ^ Gap score.
            -> AlignConfig a s
alignConfig = AlignConfig

-- | Configuration for local alignment.
localAlignConfig
  :: Num s
  => (a -> a -> s)  -- ^ Scoring function.
  -> s              -- ^ Gap score.
  -> AlignConfig a s
localAlignConfig f = alignConfig f 0

-- | Either an unmatched item or a match.
type Step a = Either (Either a a) (a, a)

stepLeft = Left . Left
stepRight = Left . Right
stepBoth a b = Right (a,b)

isMatch :: Step a -> Bool
isMatch (Right _) = True
isMatch _ = False

isLeft :: Step a -> Bool
isLeft (Left (Left _)) = True
isLeft _ = False

isRight :: Step a -> Bool
isRight (Left (Right _)) = True
isRight _ = False

-- | The result of the alignment.
data Trace a s = Trace
  { traceScore :: s
  , trace :: [Step a]
  }

instance (Show a, Show s) => Show (Trace a s) where
  show (Trace s t) = "Trace(score = " ++ show s ++ ", steps = " ++ show t ++ ")"

mt `tappend` (Trace z (t:_)) =
    fmap (\(Trace s ts) -> Trace (s+z) (t:ts)) mt

-- | Utility for displaying a Char-based alignment.
debugAlign :: [Step Char] -> String
debugAlign = go [] []
  where
  go as bs [] = reverse as ++ "\n" ++ reverse bs
  go as bs (t:ts) = case t of
    Left (Left c)  -> go (c:as) ('-':bs) ts
    Left (Right c) -> go ('-':as) (c:bs) ts
    Right (c, d)   -> go (c:as) (d:bs) ts

-- | Aligns two sequences.
--
-- >>> :{
-- let tr = align
--            (alignConfig (\a b -> if a == b then 1 else (-0.25 :: Double)) 
--                         (-0.5) (-1))
--            (Data.Vector.fromList "dopple")
--            (Data.Vector.fromList "applied")
-- in do
--    print $ traceScore tr
--    putStrLn . debugAlign . trace $ tr
-- :}
-- 1.25
-- doppl-e-
-- -applied
align :: (G.Vector v a, Num s, Ord s)
  => AlignConfig a s
  -> v a  -- ^ Left sequence.
  -> v a  -- ^ Right sequence.
  -> Trace a s
align AlignConfig{..} as bs =
  let p = (lastIndex as, lastIndex bs)
  in revTrace $ evalState (go p) M.empty
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
      if i == j then Trace 0 []
      else if i == (-1)
           then skipInit j stepRight bs
           else skipInit i stepLeft as
    | otherwise = do
      let a = as G.! i
          b = bs G.! j
      diag  <- go (i-1,j-1) `tappend` Trace (acPairScore a b) [stepBoth a b]
      a_gap <- go (i-1,  j) `tappend` Trace ac_gap_penalty [stepLeft a]
      b_gap <- go (  i,j-1) `tappend` Trace ac_gap_penalty [stepRight b]
      return $ L.maximumBy (comparing traceScore) [diag, a_gap, b_gap]
  --
  skipInit idx stepFun xs =
    let score = ac_initial_gap_penalty * fromIntegral (idx+1)
        tr = reverse [stepFun (xs G.! xi) | xi <- [0..idx]]
    in Trace score tr

-- | Aligns long streams by performing alignment on windowed sections.
windowedAlign :: (Num s, Eq s, Ord s)
  => AlignConfig a s
  -> Int  -- ^ Window size.
  -> [a]  -- ^ Left stream.
  -> [a]  -- ^ Right stream.
  -> [Step a]  -- ^ Alignment result.
windowedAlign cfg w as bs = go as bs []
  where
  go as [] rs = concat . reverse $ (map stepLeft as):rs
  go [] bs rs = concat . reverse $ (map stepRight bs):rs
  go as bs rs =
    let ahs = take w as
        bhs = take w bs
        tr0 = trace $ align cfg (V.fromList ahs) (V.fromList bhs)
        tr = let matched = dropTrailingMismatches tr0
                 len = length matched
             in if len <= 1
                then matched
                -- Give opportunity for recovery given new inputs in next go.
                -- TODO this needs some experimenting with (or even option)
                else take (len `div` 2) matched
    in if null tr
       then let (a:ax) = as
                (b:bx) = bs
            -- Degenerate case, mimic some progress to avoid getting stuck.
            in go ax bx ([stepLeft a, stepRight b]:rs)
       else let ac = countMatchOr isLeft tr
                bc = countMatchOr isRight tr
            in go (drop ac as) (drop bc bs) (tr:rs)
  --
  dropTrailingMismatches =
    reverse . dropWhile (not . isMatch) . reverse
  --
  countMatchOr f = length . filter (\s -> isMatch s || f s)

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

-- | Renders a char-based multi-alignment result to a string.
debugMultiAlign :: [MultiStep Char] -> String
debugMultiAlign =
  unlines . map (map charOrDash) . L.transpose . map stepOfAll
  where
  charOrDash = fromMaybe '-'

-- | Align multiple sequences using the Center Star heuristic method by
-- Chin, Ho, Lam, Wong and Chan (2003).
-- <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.90.7448&rep=rep1&type=pdf>. 
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
    mergeSteps mss = go [] mss
      where
      noOthers = map (const Nothing) . others . head $ mss
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
    
{-
    pairAligns:: (G.Vector v a, Num s, Ord s, Ord i)
                => AlignConfig a s
                -> [(i, v a)]
                -> [((i,i),Trace a s)]
    pairAligns conf vs = concat (map gotr xys `using` parListChunk il rseq)
        where
        is = V.fromList vs
        il = (length is) - 1
        xys = [(x,y) | x <- [0..il], y <- [x..il]]
        gotr (x,y) = [((jj,kk), tr),((kk,jj), flipLR tr)]
            where 
            j = is V.! x
            k = is V.! y
            jj = fst j
            kk = fst k
            tr = align conf (snd j) (snd k)
            flipLR tr = tr { trace = map go . trace $ tr }
                where
                go (Left (Left a)) = Left (Right a)
                go (Left (Right a)) = Left (Left a)
                go (Right (c,d)) = Right (d,c)
-}
{-
    pairAligns conf vs = concat $ map concat (parMap rseq go ts)
        where
        ts = L.tails vs
        go [] = []
        go [x] = []
        go (i:js) = map go2 js
            where
            go2 j = [((fst i,fst j),tr),((fst j, fst i), flipLR tr)]
                where
                tr = align conf (snd i) (snd j)
                flipLR tr = tr { trace = map go3 . trace $ tr }
                    where
                    go3 (Left (Left a)) = Left (Right a)
                    go3 (Left (Right a)) = Left (Left a)
                    go3 (Right (c,d)) = Right (d,c)
-}  
    --
    starSum = sum . map (traceScore . snd)
