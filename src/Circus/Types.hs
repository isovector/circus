{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Circus.Types where

import           Control.Applicative (empty)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types (toJSONKeyText)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Char
import           Data.Data (Data)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics (Generic)


------------------------------------------------------------------------------
-- | A collection of modules.
newtype Schema = Schema
  { schemaModules :: Map ModuleName Module
  }
  deriving stock (Eq, Show, Data)
  deriving newtype (Semigroup, Monoid)


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


renderModuleBS :: Module -> ByteString
renderModuleBS
  = encode
  . Schema
  . M.singleton "module"


renderModuleString :: Module -> String
renderModuleString
  = BS.unpack
  . renderModuleBS


------------------------------------------------------------------------------
-- | Helper function for constructing 'Cell's.
mkCell
    :: CellType
    -> M.Map PortName (Direction, [Bit])
    -> Cell
mkCell = flip mkCell' mempty


------------------------------------------------------------------------------
-- | Helper function for constructing 'Cell's with explicit attributes.
mkCell'
    :: CellType
    -> M.Map T.Text Value  -- ^ Attributes
    -> M.Map PortName (Direction, [Bit])
    -> Cell
mkCell' ty as m =
  Cell
    ty
    (M.fromList $ fmap (\(pn, bs) -> (Width pn, length bs)) $ M.toList $ fmap snd m)
    as
    (fmap fst m)
    (fmap snd m)

