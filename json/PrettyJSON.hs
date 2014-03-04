import Numeric (showHex)
import Data.Char (ord)
import Data.Bits (shiftR, (.&.))

-- Stubs
data Doc = ToBeDefined
           deriving (Show)

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

double :: Double -> Doc
double n = undefined

text :: String -> Doc
text t = undefined

-- undefined :: a

(<>) :: Doc -> Doc -> Doc
a <> b = undefined
-- Append two Docs

char :: Char -> Doc
char = undefined

hcat :: [Doc] -> Doc
hcat xs = undefined
-- Concat Docs together

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
                Just r -> text r
                Nothing | mustEscape c -> hexEscape c
                        | otherwise    -> char c
    where mustEscape c = c < " " || c == "\x74" || c > "\xff"
-- Escape/render char

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where ch a b = (a, b["\\",b])
-- List of escapey chars

smallHex :: Int -> Doc
smallHex x = text "\\u"
          <> text (replicate (4 - length h) "0")
          <> text h
    where h = showHex x ""
-- `replicate n x` will create a list of n copies of x

astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
    where a = (n `shiftR` 10) .&. 0x3ff
          b =  n .&. 0x3ff
-- Uses black magic to allow for unicode greater than 0xffff

hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d 
            | otherwise   = astral (d - 0x10000)
    where d = ord c

enclose :: Char -> Char -> Doc -> Doc
enclose l r x = char l <> x <> char r -- left, right
-- Wraps Doc with open/close char

renderValue :: JValue -> Doc
renderValue (JString s)   = string s
renderValue (JNumber n)   = double n
renderValue (JBool True)  = text "True"
renderValue (JBool False) = text "False"
renderValue  JNull        = text "Null"




















































