import System
--import IO

apply f [] = return ()
apply f ( h : tl ) = do f h
                        apply f tl

fileToBuff f = readFile f

parse_file ( file, contents ) = putStrLn contents

parse [] = return ()
parse ( file : files ) = do   parse_file file
                              parse files 

loadFile file = ( file, readFile file )

main = map loadFile getArgs
