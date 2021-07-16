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

instance Semigroup GraphState where
  GraphState b1 m1 <> GraphState b2 m2
    = GraphState
    { gs_next_port = b1 + b2
    , gs_module = m1 <> m2
    }

instance Monoid GraphState where
  mempty = GraphState
    { gs_next_port = Bit 0
    , gs_module = mempty
    }


------------------------------------------------------------------------------
-- | Synthesize a fresh 'Bit', suitable for connecting 'Cell's
-- together.
freshBit :: MonadState GraphState m => m Bit
freshBit = do
  p <- gets gs_next_port
  modify $ \gs ->
    gs { gs_next_port = gs_next_port gs + 1 }
  pure p


------------------------------------------------------------------------------
-- | Add a 'Cell' to the 'Module' under construction.
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


------------------------------------------------------------------------------
-- | Given a mapping from source 'Bit's to target 'Bit's, replace
-- all occurences of the source bits in the 'Module' with the target bits.
--
-- This function allows you to call 'addCell' as you go, and create
-- feedback loops later without needing to know about them in
-- advance.
unifyBits :: MonadState GraphState m => Map Bit Bit -> m ()
unifyBits rep =
  modify' $ \gs -> gs
    { gs_module = unifyBitsPure rep $ gs_module gs
    }

