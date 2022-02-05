import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString, findByExtension)
import System.IO (readFile)
import Reflex.Dom.TH
import System.FilePath (takeBaseName, replaceExtension)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as L
import Text.Megaparsec

main :: IO ()
main = defaultMain =<< goldenTests

parseTemplate :: String -> LBS.ByteString -> LBS.ByteString
parseTemplate  path = L.pack . show . runParser template  path .  L.unpack
  

goldenTests :: IO TestTree
goldenTests = do
  htmlFiles <- findByExtension [".html"] "."
  return $ testGroup "Html Template Parser golden tests"
    [ goldenVsString
        (takeBaseName htmlFile) 
        astFile
        (parseTemplate htmlFile  <$> LBS.readFile htmlFile) 
    | htmlFile <- htmlFiles
    , let astFile  = replaceExtension htmlFile ".ast"
    ]
