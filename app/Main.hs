{-# LANGUAGE OverloadedStrings #-}
module Main where


import GHC.IO.Encoding (getLocaleEncoding)
import Data.Csv
import Data.Text
import qualified Data.Text.Encoding as T
import System.IO
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.Char8 as LB

data Stuff = Stuff { stuff :: Text }
    deriving (Eq, Show)

instance FromNamedRecord Stuff
    where parseNamedRecord v = Stuff <$> fmap T.decodeLatin1 (v .: "Donnée")

csvFile = "MyFile.CSV"

main :: IO ()
main = do 
    getLocaleEncoding >>= print
    handle <- openFile csvFile ReadMode
    encoding <- hGetEncoding handle
    csvData <- System.IO.hGetContents handle
    print encoding
    Prelude.putStrLn "----"
    Prelude.putStrLn csvData
    Prelude.putStrLn "----"
    let bsData = LB.pack csvData
    print $ ((decodeByName bsData) :: Either String (Header, V.Vector Stuff))
    hClose handle
