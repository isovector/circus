{-# LANGUAGE NoMonomorphismRestriction #-}

module Lib where

import Control.Arrow
import           Data.Function (on)
import           Data.Graph.Inductive (Gr, match)
import qualified Data.Graph.Inductive as G
import           Data.Graph.Inductive.Graph hiding (Path, (&))
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Ord (comparing)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Diagrams.Backend.SVG (B(..), renderSVG)
import           Diagrams.Prelude hiding (Context)
import Graphics.SVGFonts (lin2)
import Graphics.SVGFonts.Text
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (join)
import Data.Typeable
import Control.Arrow (first)
import Physics.ForceLayout
import Data.Maybe (maybeToList)


lin2' = unsafePerformIO lin2
{-# NOINLINE lin2' #-}


(.&) :: Gr a b -> Context a b -> Gr a b
(.&) = flip (G.&)

graph :: Gr String ()
graph = G.empty
     .& ([], 0, "A:n", [])
     .& ([], 1, "D", [])
     .& ([((), 0)], 2, "copy", [])
     .& ([((), 1)], 3, "split", [])
     .& ([((), 2), ((), 3)], 4, "and", [])
     .& ([((), 2), ((), 3)], 5, "and", [])
     .& ([((), 4)], 6, ":n", [])
     .& ([((), 5)], 7, ":n", [])


sourceNodes :: Gr v e -> [Node]
sourceNodes g = go (nodes g)
  where
    go [] = []
    go (n : ns) =
      case match n g of
        (Just ([], _, _, _), _) -> n : go ns
        _ -> go ns

rank :: Ord v => Gr v e -> Map Node Int
rank g = go (Seq.fromList $ fmap (0,) $ sourceNodes g) g
  where
    go Seq.Empty _ = mempty
    go ((d, n) Seq.:<| s) g =
      case match n g of
        (Just ([], _, _, out), g') ->
          M.singleton n d
            <> go (s <> Seq.fromList (fmap ((d + 1,) . snd) out)) g'
        (_, _) -> go s g


ranksOf :: Map Node Int -> [[Node]]
ranksOf = fmap (fmap fst) . groupBy ((==) `on` snd) . sortOn snd . M.assocs


text' d s = (strokeP $ textSVG' (TextOpts lin2' INSIDE_H KERN False d d) s)
          # lw none # fc black

stateLabel = text' 6
arrowLabel txt size = text' size txt

state  = circle 4 # fc silver
fState = circle 3.7 # fc lightblue <> state

points = map p2 [ (0, 12), (12, 16), (24, 12), (24, 21), (36, 16), (48, 12)
                , (48, 21), (12, 0), (7, 7), (24, 4), (36, 0), (46, 0)]


data Component a = Component
  { c_node :: Node
  , c_rank :: Int
  , c_label :: String
  , c_edges :: [Node]
  , c_pos :: Point V2 a
  }
  deriving stock Functor

drawTheD :: Component a -> (Point V2 a, Diagram B)
drawTheD c = (c_pos c, (stateLabel (c_label c) <> state) # named (c_node c))

mkD
    :: (v -> String)
    -> Gr v e
    -> [[Node]]
    -> [Component Int]
mkD sho g ranked = do
  (rank, ns) <- zip [0..] ranked
  (y, n) <- zip [0..] ns
  case match n g of
    (Just (_, _, v, es), _) ->
      -- TODO(sandy): stupid diagrams, upside down y coord
      pure (Component n rank (sho v) (fmap snd es) $ P $ V2 rank (-y))
    _ -> mempty

componentEdges :: Component a -> [(Node, Node)]
componentEdges c = fmap ((c_node c), ) $ c_edges c


ds = mkD id graph $ ranksOf $ rank graph

pairsOf :: [a] -> [(a, a)]
pairsOf cs = do
  (p1 : ps) <- tails $ cs
  p2 <- ps
  pure (p1, p2)

e :: Floating a => [Component a] -> Ensemble V2 a
e cs = Ensemble
  [ (componentEdges =<< cs, hookeForce 0.05 40)
  , (allPairs, coulombForce 3)
  ] particleMap
  where
    allPairs    = do
      (p1 : ps) <- tails $ cs
      p2 <- ps
      pure (c_node p1, c_node p2)
    particleMap = M.fromList $ do
      c <- cs
      pure (c_node c, initParticle $ c_pos c)

unassemble :: Map Node (Component a) -> Ensemble V2 a -> [Component a]
unassemble m e = do
  (n, p) <- M.assocs $ _particles e
  c <- maybeToList $ M.lookup n m
  pure $ c { c_pos = _pos p }

float :: (Floating a, Ord a) => [Component a] -> [Component a]
float cs =
  let m = M.fromList $ fmap (c_node &&& id) cs
   in unassemble m $ forceLayout def  $ e cs


states = position $ fmap drawTheD $ float $ fmap (fmap (fromIntegral . (* 20))) ds

shaft  = arc xDir (-1/6 @@ turn)
shaft' = arc xDir (-2.7/5 @@ turn)
line = trailFromOffsets [unitX]

arrowStyle1 = (with  & arrowHead  .~ spike & headLength .~ normal
                     & arrowShaft .~ shaft)

arrowStyle2 = (with  & arrowHead   .~ spike
                     & arrowShaft  .~ shaft' & arrowTail .~ lineTail
                     & tailTexture .~ solid black & lengths .~ normal)

arrowStyle3 = (with  & arrowHead  .~ spike  & headLength .~ normal
                     & arrowShaft .~ line)

edgify g = flip foldMap (G.edges g) $ \case
  (src, dst) -> Endo $ connectOutside' arrowStyle1 src dst

example = appEndo (edgify graph) states
  -- # connectOutside' arrowStyle1 "1" "2"
  -- # connectOutside' arrowStyle3 "1" "4"
  -- # connectPerim'   arrowStyle2 "2" "2" (4/12 @@ turn) (2/12 @@ turn)
  -- # connectOutside' arrowStyle1 "2" "3"
  -- # connectPerim'   arrowStyle2 "3" "3" (4/12 @@ turn) (2/12 @@ turn)
  -- # connectOutside' arrowStyle1 "4" "5"
  -- # connectPerim'   arrowStyle2 "5" "5" (1/12 @@ turn) (-1/12 @@ turn)


main :: IO ()
main = renderSVG "/tmp/yo.svg" (dims (V2 @Double 500 500)) example

