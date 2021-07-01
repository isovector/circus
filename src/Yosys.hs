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


data Schema = Schema
  { schemaModules :: Map ModuleName Module
  }

data Module = Module
  { modulePorts :: Map PortName Port
  , moduleCells :: Map CellName Cell
  }

newtype ModuleName = ModuleName { getModuleName :: Text }
  deriving newtype (Eq, Ord, IsString, ToJSONKey, FromJSONKey, FromJSON, ToJSON)

newtype PortName = PortName { getPortName :: Text }
  deriving newtype (Eq, Ord, IsString, ToJSONKey, FromJSONKey, FromJSON, ToJSON)

newtype CellName = CellName { getCellName :: Text }
  deriving newtype (Eq, Ord, IsString, ToJSONKey, FromJSONKey, FromJSON, ToJSON)


data Port = Port
  { portDirection :: Direction
  , portBits :: [Bit]
  }

newtype Bit = Bit { getBit :: Int }
  deriving newtype (Num, ToJSON, FromJSON)


data Cell = Cell
  { cellType :: CellType
  , cellParameters :: Map Parameter Int
  , cellPortDirections :: Map PortName Direction
  , cellConnections :: Map PortName [Bit]
  }


data Parameter
  = Width PortName
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

pattern CellReduceNor :: CellType
pattern CellReduceNor = CellGeneric "$nor"

pattern CellReduceXor :: CellType
pattern CellReduceXor = CellGeneric "$xor"

pattern CellReduceNxor :: CellType
pattern CellReduceNxor = CellGeneric "$xnor"

pattern CellNot :: CellType
pattern CellNot = CellGeneric "$not"

pattern CellAdd :: CellType
pattern CellAdd = CellGeneric "$add"

pattern CellDff :: CellType
pattern CellEq :: CellType

pattern CellEq = CellGeneric "$eq"
pattern CellLt :: CellType

pattern CellDff = CellGeneric "dff"
pattern CellDffn :: CellType

pattern CellDffn = CellGeneric "dffn-bus"
pattern CellLt = CellGeneric "$lt"

pattern CellGe :: CellType
pattern CellGe = CellGeneric "$ge"

pattern CellInputExt :: CellType
pattern CellInputExt = CellGeneric "$_inputExt_"

pattern CellJoin :: CellType
pattern CellOutputExt :: CellType

pattern CellOutputExt = CellGeneric "$_outputExt_"
pattern CellJoin = CellGeneric "$_join_"

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


mkTestCell :: CellType -> Bit -> Bit -> Bit -> Cell
mkTestCell ty in1 in2 out =
  Cell
    ty
    (M.fromList
      [ (Width "A", 2)
      , (Width "B", 1)
      , (Width "X", 1)
      ])
    (M.fromList
      [ ("A", Input)
      , ("B", Input)
      , ("Y", Output)
      ])
    (M.fromList
      [ ("A", [in1, in1])
      , ("B", [in2])
      , ("Y", [out])
      ])

schema :: Schema
schema = Schema $ M.fromList
  [ ( "test"
    , Module
        (M.fromList
          [ ("in1", Port Input [0])
          , ("in2", Port Input [1])
          , ("out", Port Output [2])
          ]
        )
        (M.fromList
          [ ("c1" , mkTestCell (CellGeneric "hmm") 0 0 3)
          , ("c2" , mkTestCell CellAnd 0 1 4)
          , ("c3" , mkTestCell CellOr 3 4 2)
          ])
    )
  ]

blah :: String
blah = read $ show $ encode schema




