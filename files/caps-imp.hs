import System.IO
import Data.Char (toUpper)

main :: IO ()
main = do
       inh  <- openFile "input.txt"  ReadMode
       outh <- openFile "output.txt" WriteMode
       mainloop inh outh
       hClose inh
       hClose outh

mainloop :: Handle -> Handle -> IO ()
mainloop inh outh =
    do ineof <- hIsEOF inh
       if ineof
            then return () -- wraps pure value as IO
            else do inpStr <- hGetLine inh
                    hPutStrLn outh $ map toUpper inpStr
                    mainloop inh outh

-- Standard In, Out, and Error --
-- Constants: stdin, stdout, stderr
getLine'  = hGetLine stdin
putStrLn' = hPutStrln stdout
print'    = hPrint stdout

-- Seek and Tell --
-- hTell gives current position
-- hSeek moves current position
-- * hTell :: Handle -> IO Integer
-- * hSeek :: Handle -> SeekMode -> Integer -> IO ()
--- SeekMode: 
--- + AbsoluteSeek: precise location in file
--- + RelativeSeek: from current location
--- + SeekFromEnd:  from the end, backwards
-- hIsSeekable tells if Handle can be seeked