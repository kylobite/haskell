import Numeric (showHex)
import Data.Char (ord)
import Data.Bits (shiftR, (.&.))

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

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series o c item = enclose o c -- open, close
                . fsep . punctuate (char ",") . map item

renderJValue :: JValue -> Doc
renderJValue (JString s)   = string s
renderJValue (JNumber n)   = double n
renderJValue (JBool True)  = text "True"
renderJValue (JBool False) = text "False"
renderJValue  JNull        = text "Null"
renderJValue (JArray ary)  = series "[" "]" renderJValue ary
renderJValue (JObject obj) = series "{" "}" field obj
    where field (name,val) = string name
                          <> text ": "
                          <> renderJValue val



















































