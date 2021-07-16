{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Yosys where

import           Control.Applicative (empty)
import           Control.Arrow
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types (toJSONKeyText)
import           Data.Char
import           Data.Data (Data)
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics (Generic)


------------------------------------------------------------------------------
-- | A collection of modules.
data Schema = Schema
  { schemaModules :: Map ModuleName Module
  }
  deriving stock (Eq, Show, Data)


data Module = Module
  { -- | Inputs and outputs
    modulePorts :: Map PortName Port
    -- | Components
  , moduleCells :: Map CellName Cell
  }
  deriving stock (Eq, Show, Data)

instance Semigroup Module where
  (<>) (Module p1 c1) (Module p2 c2) = Module
    { modulePorts = p1 <> p2
    , moduleCells = c1 <> c2
    }

instance Monoid Module where
  mempty = Module {modulePorts = mempty, moduleCells = mempty}

newtype ModuleName = ModuleName { getModuleName :: Text }
  deriving stock (Data)
  deriving newtype (Eq, Ord, Show, IsString, ToJSONKey, FromJSONKey, FromJSON, ToJSON)

newtype PortName = PortName { getPortName :: Text }
  deriving stock (Data)
  deriving newtype (Eq, Ord, Show, IsString, ToJSONKey, FromJSONKey, FromJSON, ToJSON)

newtype CellName = CellName { getCellName :: Text }
  deriving stock (Data)
  deriving newtype (Eq, Ord, Show, IsString, ToJSONKey, FromJSONKey, FromJSON, ToJSON)



data Port = Port
  { -- | Whether this port is an input or an output
    portDirection :: Direction
    -- | The individual wires connected to this port. They are numbered in the
    -- same order they are described here.
  , portBits :: [Bit]
  }
  deriving stock (Eq, Show, Data)

------------------------------------------------------------------------------
-- | A single wire. Bits are defined implicitly by a unique ID. Every component
-- that references the bit will be connected with a common node.
newtype Bit = Bit { getBit :: Int }
  deriving stock (Eq, Ord, Show, Data)
  deriving newtype (Num, ToJSON, FromJSON)


data Cell = Cell
  { -- | The symbol to use when drawing this cell.
    cellType :: CellType
  , cellParameters :: Map Parameter Int
  , cellAttributes :: Map Text Value
    -- | Which ports are inputs and outputs.
  , cellPortDirections :: Map PortName Direction
    -- | What are the ports connected to? Each port may connect to several
    -- bits, but make sure you set the 'Width' cell 'Parameter' if this is the
    -- case.
  , cellConnections :: Map PortName [Bit]
  }
  deriving stock (Eq, Show, Data)


data Parameter
  = -- | How many bits wide is the given 'Port'?
    Width PortName
    -- | Is the given 'Port' signed?
  | Signed PortName
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (FromJSON, ToJSON)

instance ToJSONKey Parameter where
  toJSONKey = toJSONKeyText $ \case
    Width pn -> getPortName pn <> "_WIDTH"
    Signed pn -> getPortName pn <> "_SIGNED"
  toJSONKeyList = error "toJSONKeyList called on Parameter"

instance FromJSONKey Parameter where
  fromJSONKey = undefined


data Direction
  = Input
  | Output
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data)

instance ToJSON Direction where
  toJSON Input = String "input"
  toJSON Output = String "output"

instance FromJSON Direction where
  parseJSON = withText "Direction" $ \case
    "input" ->  pure Input
    "output" -> pure Output
    _ -> empty


------------------------------------------------------------------------------
-- | Master list of cells, and their associated names is available here:
--
-- https://raw.githubusercontent.com/nturley/netlistsvg/master/lib/default.svg?sanitize=true
data CellType = CellGeneric Text
  deriving stock (Eq, Ord, Show, Data)


pattern CellMux :: CellType
pattern CellMux = CellGeneric "$mux"

pattern CellMuxBus :: CellType
pattern CellMuxBus = CellGeneric "$mux-bus"

pattern CellTribuf :: CellType
pattern CellTribuf = CellGeneric "$tribuf"

pattern CellAnd :: CellType
pattern CellAnd = CellGeneric "$and"

pattern CellOr :: CellType
pattern CellOr = CellGeneric "$or"

pattern CellNand :: CellType
pattern CellNand = CellGeneric "$nand"

pattern CellNor :: CellType
pattern CellNor = CellGeneric "$nor"

pattern CellXor :: CellType
pattern CellXor = CellGeneric "$xor"

pattern CellXnor :: CellType
pattern CellXnor = CellGeneric "$xnor"

pattern CellNot :: CellType
pattern CellNot = CellGeneric "$not"

pattern CellAdd :: CellType
pattern CellAdd = CellGeneric "$add"

pattern CellEq :: CellType
pattern CellEq = CellGeneric "$eq"

pattern CellDff :: CellType
pattern CellDff = CellGeneric "$dff"

pattern CellDffn :: CellType
pattern CellDffn = CellGeneric "$dffn-bus"

pattern CellLt :: CellType
pattern CellLt = CellGeneric "$lt"

pattern CellGe :: CellType
pattern CellGe = CellGeneric "$ge"

pattern CellConstant :: CellType
pattern CellConstant = CellGeneric "$_constant_"

-- TODO(sandy): use real port names


instance ToJSON CellType where
  toJSON (CellGeneric t) = String t

instance FromJSON CellType where
  parseJSON = withText "CellType" $ pure . CellGeneric

$(deriveJSON
    defaultOptions
      { fieldLabelModifier = camelTo2 '_' . drop 4
      , constructorTagModifier = map toLower
      }
      ''Cell
 )

$(deriveJSON
    defaultOptions
      { fieldLabelModifier = camelTo2 '_' . drop 4
      , constructorTagModifier = map toLower
      }
      ''Port
 )

$(deriveJSON
    defaultOptions
      { fieldLabelModifier = camelTo2 '_' . drop 6
      , constructorTagModifier = map toLower
      }
      ''Module
 )

$(deriveJSON
    defaultOptions
      { fieldLabelModifier = camelTo2 '_' . drop 6
      , constructorTagModifier = map toLower
      }
      ''Schema
 )


mkMonoidalBinaryOp :: CellType -> PortName -> PortName -> PortName -> [Bit] -> [Bit] -> [Bit] -> Cell
mkMonoidalBinaryOp cell in1p in2p outp in1 in2 out =
  Cell
    cell
    (M.fromList
      [ (Width in1p, length in1)
      , (Width in2p, length in2)
      , (Width outp, length out)
      ])
    mempty
    (M.fromList
      [ (in1p, Input)
      , (in2p, Input)
      , (outp, Output)
      ])
    (M.fromList
      [ (in1p, in1)
      , (in2p, in2)
      , (outp, out)
      ])


renderModule :: Module -> IO ()
renderModule
  = writeFile "/tmp/test.json"
  . read
  . show
  . encode
  . Schema
  . M.singleton "module"


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


------------------------------------------------------------------------------
-- | Helper function for constructing 'Cell's.
mkCell
    :: CellType
    -> M.Map T.Text Value  -- ^ Attributes
    -> M.Map PortName (Direction, [Bit])
    -> Cell
mkCell ty as m =
  Cell
    ty
    (M.fromList $ fmap (\(pn, bs) -> (Width pn, length bs)) $ M.toList $ fmap snd m)
    as
    (fmap fst m)
    (fmap snd m)

