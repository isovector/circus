{-# LANGUAGE DeriveGeneric #-}

module DSL where

import           Control.Monad.State.Class
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import           GHC.Generics
import           Generics.SYB hiding (Generic)
import           Yosys


data GraphState = GraphState
  { gs_next_port :: Bit
  , gs_module    :: Module
  }
  deriving stock (Generic)


freshBit :: MonadState GraphState m => m Bit
freshBit = do
  p <- gets gs_next_port
  modify $ \gs ->
    gs { gs_next_port = gs_next_port gs + 1 }
  pure p


addCell :: MonadState GraphState m => Cell -> m ()
addCell c = do
  uniq <- freshBit
  let name = CellName $ T.pack $ show $ getBit uniq
  modify' $ \gs ->
    gs { gs_module = gs_module gs
                  <> Module mempty (M.singleton name c)
       }


unifyBitsPure :: Data a => Map Bit Bit -> a -> a
unifyBitsPure rep  = everywhere $ mkT $ \case
  b | Just b' <- M.lookup b rep -> b'
    | otherwise -> b


unifyBits :: MonadState GraphState m => Map Bit Bit -> m ()
unifyBits rep =
  modify' $ \gs -> gs
    { gs_module = unifyBitsPure rep $ gs_module gs
    }

