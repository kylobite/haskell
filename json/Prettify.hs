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

compact :: Doc -> String
compact x = transform [x]
    where transform []     = ""
          transform (x:xs) =
            case x of
                Empty        -> transform xs
                Char c       -> c : transform xs
                Text s       -> s ++ transform xs
                Line         -> "\n" : transform xs
                a `Concat` b -> transform (a:b:xs)
                a `Union` _  -> transform (a:xs)
-- Compacts Doc into a String

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
    where best col (x:xs) =
            case x of
                Empty        -> best col xs
                Char c       -> c :  best (col + 1) xs
                Text s       -> s ++ best (col + length s) xs
                Line         -> "\n" : best 0 xs
                a `Concat` b -> best col (a:b:xs)
                a `Union` b  -> nicest col (best (a:xs))
                                           (best (b:xs))
          best _ _ = ""

          nicest col a b | (width - least) `fits` a = a
                         | otherwise                = b
                         where least = min width col
-- True pretty printing

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` ""        = True
w `fits` ("\n":_)  = True
w `fits` (x:xs)    = (w - 1) `fits` xs
-- Does this or that fit better here?



















































