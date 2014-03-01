module Main where
    
import SimpleJSON

main = print (JObject [("foo", JNumber 1),("bar", JBool False)])

-- main acts like main() in Java and C