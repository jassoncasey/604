import System

apply f [] = return ()
apply f ( h : tl ) = do f h
                        apply f tl

readFile f = do   handle <- openFile f ReadMode
                  hClose handle

main = do   x <- getArgs 
            apply readFile x
