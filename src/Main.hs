module Main where 

import Parser.Expression
import Parser.Statement
import Text.Megaparsec

main = getContents >>= parseTest (statement <* eof)