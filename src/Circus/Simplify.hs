module Circus.Simplify (simplify) where

import           Circus.Types
import           Control.Arrow
import           Data.List
import qualified Data.Map as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as S

------------------------------------------------------------------------------
-- | Gather sets of input and output ports for the given cell.
cellPorts :: Cell -> (Set PortName, Set PortName)
cellPorts c =
  let ports = M.assocs $ cellPortDirections c
      (ip, op) = fmap fst *** fmap fst $ partition ((== Input) . snd) ports
   in (S.fromList ip, S.fromList op)


------------------------------------------------------------------------------
-- | Gather input and output bits for the given cell.
ioBits :: Module -> (Set Bit, Set Bit)
ioBits m = flip foldMap (moduleCells m) $ \c ->
  let (ip, op) = cellPorts c
      ib = foldMap (fromMaybe [] . flip M.lookup (cellConnections c)) $ ip
      ob = foldMap (fromMaybe [] . flip M.lookup (cellConnections c)) $ op
      mod_ports = fmap (portDirection &&& portBits) $ M.elems $ modulePorts m
      (iports, oports) = (snd =<<) *** (snd =<<) $ partition ((== Input) . fst) mod_ports
   in (S.fromList $ ib <> oports, S.fromList $ ob <> iports)


------------------------------------------------------------------------------
-- | Delete any cells which output only bits in the @to_kill@ set.
pruneCellsOutput :: Set Bit -> Module -> Module
pruneCellsOutput to_kill m = m
  { moduleCells = M.filter (not . should_kill) $ moduleCells m
  }
  where
    should_kill :: Cell -> Bool
    should_kill c =
      let (_, op) = cellPorts c
       in case S.toList op of
            [pn] ->
              let bits = fromMaybe [] $ M.lookup pn $ cellConnections c
               in all (flip S.member to_kill) bits
            _ -> False


------------------------------------------------------------------------------
-- | Recursively delete cells that output only bits which are unused in the
-- circuit.
simplify :: Module -> Module
simplify m =
  let (ib, ob) = ioBits m
      to_kill = ob S.\\ ib
      m' = pruneCellsOutput to_kill m
   in case m == m' of
        True -> m
        False -> simplify m'

