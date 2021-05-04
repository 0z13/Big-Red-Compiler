{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module CFG where
import Control.Applicative
import qualified Data.HashMap.Internal as M
import System.IO
import qualified Data.ByteString.Lazy as B 
import GHC.Generics
import qualified Data.Vector as V
import Data.Either
import Data.Maybe
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), (.:), (.=), object, eitherDecode, (.:?))
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Text (Text)

-- I need a list of blocks
-- Then I need a hashmap that associates blocks with other blocks.
-- So i need a function f : [blocks] -> [(blocks, blocks)] 

data Args = Args
  { name :: Text
  , argtype :: Text
  } deriving (Show, Eq, Ord)

data Functions = Functions
  { name :: Text
  , argtype :: Maybe Text
  , args :: Maybe [Args]
  , instrs :: [Instrs]
  } deriving (Show, Eq, Ord)

data Instrs = Instrs
  { argtype :: Maybe Text
  , op :: Text
  , value :: Maybe Int
  , dest :: Maybe Text
  } deriving (Show, Eq, Ord)

data Model = Model
  { functions :: [Functions]
  } deriving (Show, Eq, Ord)

instance FromJSON Args where
  parseJSON (Object v) = do
    name <- v .: "name"
    argtype <- v .: "type"
    pure $ Args{..}
  parseJSON invalid = do
    prependFailure "parsing Args failed, "
      (typeMismatch "Object" invalid)

instance FromJSON Functions where
  parseJSON (Object v) = do
    name <- v .: "name"
    argtype <- v .:? "type"
    args <- v .:? "args"
    instrs <- v .: "instrs"
    pure $ Functions{..}
  parseJSON invalid = do
    prependFailure "parsing Functions failed, "
      (typeMismatch "Object" invalid)

instance FromJSON Instrs where
  parseJSON (Object v) = do
    argtype <- v .:? "type"
    op <- v .: "op"
    value <- v .:? "value" 
    dest <- v .:? "dest"
    pure $ Instrs{..}
  parseJSON invalid = do
    prependFailure "parsing Instrs failed, "
      (typeMismatch "Object" invalid)

instance FromJSON Model where
  parseJSON (Object v) = do
    functions <- v .: "functions"
    pure $ Model{..}
  parseJSON invalid = do
    prependFailure "parsing Model failed, "
      (typeMismatch "Object" invalid)


-- Todo:
-- Make sure this parses semi-correctly 
-- ideally in accorance to the spec of bril.
-- grab the instrs and create blocks
-- grab the blocks and create the cfg
-- this is how we chill 

main1 = do
  inp <- B.getContents 
  let json = eitherDecode inp :: Either String Model
  case json of 
    (Left s) -> print "yoyO errorz"
    (Right obj) -> print $ functions obj

  

