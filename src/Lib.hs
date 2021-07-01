{-# LANGUAGE NoMonomorphismRestriction #-}

module Lib where

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


mkD
    :: ( Floating n
       , Ord n
       , Typeable n
       , RealFloat n
       , Renderable (Path V2 n) b
       , Read n
       )
    => (v -> String)
    -> Gr v e
    -> [[Node]]
    -> [(Point V2 Int, QDiagram b V2 n Any)]
mkD sho g ranked = do
  (rank, ns) <- zip [0..] ranked
  (y, n) <- zip [0..] ns
  case match n g of
    (Just (_, _, v, es), _) ->
      -- TODO(sandy): stupid diagrams, upside down y coord
      pure (P $ V2 rank (-y), (stateLabel (sho v) <> state) # named n)
    _ -> mempty


ds = mkD id graph $ ranksOf $ rank graph

states = position $ fmap (first $ fmap (fromIntegral . (* 20))) ds

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
main = renderSVG "/tmp/yo.svg" (dims (V2 @Float 500 500)) example

