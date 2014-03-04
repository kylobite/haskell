import SimpleJSON

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

fsep :: [Doc] -> Doc
fsep xs = undefined
-- Combines a list of Docs together

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []      = []
punctuate p [d]     = [d]
punctuate p (d:ds)  = (d <> p) : punctuate p ds

