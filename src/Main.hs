module Main where 

import Parser.Expression
import Text.Megaparsec

main = getContents >>= parseTest expression