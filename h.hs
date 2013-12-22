kPaildrome :: [a] -> [a]
kPaildrome xs | null xs =   []
kPaildrome []           =   []
kPaildrome (x:xs)       =   let a = (x:xs)
                                b = reverse a
                            in  a ++ b