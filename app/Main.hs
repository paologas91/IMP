module Main where
    
import Parser
import Interpreter

main :: IO ()
main = do
  s <- readFile "source.txt"
  let p = exeParser s
  if snd p == ""
    then do
      let s = emptyState
      let s' = exeCommands s (fst p)
      print p
      print s'
    else do
      print p
      error "Parse failed"
    
  
