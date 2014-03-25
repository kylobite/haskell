import System.IO
import System.Directory (renameFile,removeFile)

main :: IO ()
main = do
       copyh  <- openFile "output.txt" ReadMode
       cloneh <- openFile "clone.txt" WriteMode
       mainloop copyh cloneh
       hClose inh
       hClose outh

       renameFile "clone.txt" "remove.txt"
       removeFile "remove.txt"

mainloop :: Handle -> Handle -> IO ()
mainloop inh outh =
    do ineof <- hIsEOF inh
       if ineof
            then return () -- wraps pure value as IO
            else do inpStr <- hGetLine inh
                    hPutStrLn outh inpStr
                    mainloop inh outh