module Libs.Object.ObjLoader where

import Text.Parsec (runParser)
import Text.Parsec.Char (string, digit, spaces)
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (many1, optionMaybe)
import Control.Applicative ((<*), (*>), (<|>))
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V

import Libs.Vector (Vector3(..))
import Libs.Object.Triangle (Triangle, makeTriangle)
import Libs.Material.Material (Material(..))
import Libs.Material.Diffuse (Diffuse(..))

-- | Parse a string that contains several digital chars
digitsP :: Parser String
digitsP = many1 digit

-- | Parse an unsigned int with digitsP
uintP :: Parser Int
uintP = read <$> digitsP

-- | Parse an basic number expressed by either an int (e.g. 123, -123) or a basic
--   float (e.g. 123.0, -123.0)
numP :: Parser Float
numP = read <$> do
  signM <- optionMaybe $ string "-"
  num <- digitsP
  floatM <- optionMaybe $ (++) <$> string "." <*> digitsP
  return $ (fromMaybe "" signM) ++ num ++ (fromMaybe "" floatM)

-- | Parse Vector3 with number
vectorP :: Parser a -> Parser (Vector3 a)
vectorP p = Vector3
  <$> p <* spaces
  <*> p <* spaces
  <*> p <* spaces

-- | ObjLoader parser currently support 2 types of section. First is Vertex section
--   starting with 'v', and second is Mapping section starting with 'f'
data ObjSection = Vertex (Vector3 Float)
                | Mapping (Vector3 Int)
                deriving Show

vertexP :: Parser ObjSection
vertexP = Vertex <$> (string "v" *> spaces *> vectorP numP)

mappingP :: Parser ObjSection
mappingP = Mapping <$> (string "f" *> spaces *> vectorP uintP)

sectionP :: Parser ObjSection
sectionP = vertexP <|> mappingP

loadObj :: FilePath -> IO [Triangle]
loadObj path = do
  ls <- readFile path
  let result = runParser (many1 sectionP) () path ls
  case result of
    Left parseE -> print parseE >> return []
    Right sections -> do
      let (vs, ms) = _reduceSections sections
      return $ _makeTriangles (V.fromList vs) ms

_reduceSections :: [ObjSection] -> ([Vector3 Float], [Vector3 Int])
_reduceSections [] = ([], [])
_reduceSections ((Vertex v):xs)  = (v:vs, ms) where (vs, ms) = _reduceSections xs
_reduceSections ((Mapping m):xs) = (vs, m:ms) where (vs, ms) = _reduceSections xs

_makeTriangles :: V.Vector (Vector3 Float) -> [Vector3 Int] -> [Triangle]
_makeTriangles _ [] = []
_makeTriangles vs ((Vector3 i1 i2 i3):ms) =
  makeTriangle a b c Nothing m : _makeTriangles vs ms where
    a = vs V.! (i1 - 1)
    b = vs V.! (i2 - 1)
    c = vs V.! (i3 - 1)
    -- TODO Here we use a dummy material, next version we will move material section
    -- into Object
    m = Material $ Diffuse (Vector3 0 0 0) (Vector3 0 0 0)
