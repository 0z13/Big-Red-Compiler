module CFG where

import Data.Aeson
import System.IO
import qualified Data.ByteString.Lazy as B 
x :: Maybe [Integer]
x = decode "[1,2,3]"

main1 = do
  inp <- B.getContents 
  let res = decode inp :: Maybe Object 
  case res of
    (Just s) -> putStr (show s)
    Nothing  -> B.putStr ""
