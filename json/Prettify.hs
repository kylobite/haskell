import SimpleJSON

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
           deriving (Show,Eq)

empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s  = Text s

double :: Double -> Doc
double d = text (show d)

line :: Doc
line = Line

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

(<>) :: Doc -> Doc -> Doc
Empty <> b = b
a <> Empty = a
a <> b     = a `Concat` b
-- Append two Docs

(</>) :: Doc -> Doc -> Doc
a </> b = a <> softline <> b
-- Append two Docs between a newline

softline :: Doc
softline = group line
-- Newline for word wrapping

group :: Doc -> Doc
group x = flatten x `Union` x
-- We need two versions of a Doc for rendering

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten (x `Union`  _) = flatten x
flatten Line           = Char " "
flatten other          = other
-- Replace Line with a space

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty
-- Like concat, but with more Doc

hcat :: [Doc] -> Doc
hcat = fold (<>)
-- Concat Docs together

fsep :: [Doc] -> Doc
fsep = fold (</>)
-- Combines a list of Docs together

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []      = []
punctuate p [d]     = [d]
punctuate p (d:ds)  = (d <> p) : punctuate p ds
-- Append Docs between punctuation