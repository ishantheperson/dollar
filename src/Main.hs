module Main where 

import Parser.Function
import Text.Megaparsec

main = getContents >>= parseTest ((many functionDef) <* eof)