module Project2
( batch
) where

-- Process files for project 2
batch :: [String] -> IO ()
batch args = do
  ForM agrs readFile