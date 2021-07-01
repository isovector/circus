{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Yosys where

import           Control.Applicative (empty)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types (toJSONKeyText)
import           Data.Char
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics (Generic)


------------------------------------------------------------------------------
-- | A collection of modules.
data Schema = Schema
  { schemaModules :: Map ModuleName Module
  }


data Module = Module
  { -- | Inputs and outputs
    modulePorts :: Map PortName Port
    -- | Components
  , moduleCells :: Map CellName Cell
  }

instance Semigroup Module where
  (<>) (Module p1 c1) (Module p2 c2) = Module
    { modulePorts = p1 <> p2
    , moduleCells = c1 <> c2
    }

instance Monoid Module where
  mempty = Module {modulePorts = mempty, moduleCells = mempty}

newtype ModuleName = ModuleName { getModuleName :: Text }
  deriving newtype (Eq, Ord, IsString, ToJSONKey, FromJSONKey, FromJSON, ToJSON)

newtype PortName = PortName { getPortName :: Text }
  deriving newtype (Eq, Ord, IsString, ToJSONKey, FromJSONKey, FromJSON, ToJSON)

newtype CellName = CellName { getCellName :: Text }
  deriving newtype (Eq, Ord, IsString, ToJSONKey, FromJSONKey, FromJSON, ToJSON)


data Port = Port
  { -- | Whether this port is an input or an output
    portDirection :: Direction
    -- | The individual wires connected to this port. They are numbered in the
    -- same order they are described here.
  , portBits :: [Bit]
  }

------------------------------------------------------------------------------
-- | A single wire. Bits are defined implicitly by a unique ID. Every component
-- that references the bit will be connected with a common node.
newtype Bit = Bit { getBit :: Int }
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


data Parameter
  = -- | How many bits wide is the given 'Port'?
    Width PortName
    -- | Is the given 'Port' signed?
  | Signed PortName
  deriving stock (Eq, Ord, Generic)
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


pattern CellMux :: CellType
pattern CellMux = CellGeneric "$mux"

pattern CellMuxBus :: CellType
pattern CellMuxBus = CellGeneric "$mux-bus"

pattern CellTribuf :: CellType
pattern CellTribuf = CellGeneric "$tribuf"

pattern CellAnd :: CellType
pattern CellAnd = CellGeneric "$and"

pattern CellOr :: CellType
pattern CellNand :: CellType

pattern CellNand = CellGeneric "$nand"
pattern CellOr = CellGeneric "$or"

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
pattern CellDff = CellGeneric "dff"

pattern CellDffn :: CellType
pattern CellDffn = CellGeneric "dffn-bus"

pattern CellLt :: CellType
pattern CellLt = CellGeneric "$lt"

pattern CellGe :: CellType
pattern CellGe = CellGeneric "$ge"

pattern CellInputExt :: CellType
pattern CellInputExt = CellGeneric "$_inputExt_"

pattern CellJoin :: CellType
pattern CellJoin = CellGeneric "$_join_"

pattern CellSplit :: CellType
pattern CellSplit = CellGeneric "$_split_"

pattern CellOutputExt :: CellType
pattern CellOutputExt = CellGeneric "$_outputExt_"

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


mkMonoidalBinaryOp :: CellType -> PortName -> PortName -> PortName -> [Bit] -> [Bit] -> Bit -> Cell
mkMonoidalBinaryOp cell in1p in2p outp in1 in2 out =
  Cell
    cell
    (M.fromList
      [ (Width in1p, length in1)
      , (Width in2p, length in2)
      , (Width outp, 1)
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
      , (outp, [out])
      ])

mkAnd = mkMonoidalBinaryOp CellAnd "A" "B" "C"
mkOr = mkMonoidalBinaryOp CellOr "A" "B" "C"
mkXor = mkMonoidalBinaryOp CellXor "A" "B" "C"

mkConstant :: String -> [Bit] -> Cell
mkConstant str out =
  Cell
  CellConstant
    (M.fromList
      [ (Width "Y", length out )
      ]
    )
    (M.singleton "output" "hey")
    (M.fromList
      [ ("Y", Output)
      ])
    (M.fromList
      [ ("Y", out)
      ])


-- mkTestCell :: CellType -> Bit -> Bit -> Bit -> Cell
-- mkTestCell ty in1 in2 out =
--   Cell
--     ty
--     (M.fromList
--       [ (Width "A", 2)
--       , (Width "B", 1)
--       , (Width "X", 1)
--       ])
--     mempty
--     (M.fromList
--       [ ("A", Input)
--       , ("B", Input)
--       , ("Y", Output)
--       ])
--     (M.fromList
--       [ ("A", [in1, in1])
--       , ("B", [in2])
--       , ("Y", [out])
--       ])

-- schema :: Schema
-- schema = Schema $ M.fromList
--   [ ( "test"
--     , Module
--         (M.fromList
--           [ ("in1", Port Input [0])
--           , ("in2", Port Input [1])
--           , ("out", Port Output [2])
--           ]
--         )
--         (M.fromList
--           [ ("c1" , mkTestCell (CellGeneric "hmm") 0 0 3)
--           , ("c2" , mkTestCell CellAnd 0 1 4)
--           , ("c3" , mkTestCell CellOr 3 4 5)
--           , ("c4" , mkTestCell CellAnd 5 6 2)
--           , ("const" , mkConstant "hello" [6])
--           ])
--     )
--   ]

-- main :: IO ()
-- main = writeFile "/tmp/test.json" $ read $ show $ encode schema

