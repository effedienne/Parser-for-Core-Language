import System.IO
import Parser
import ParseProg


readF :: IO String
readF = do inh <- openFile "input.txt" ReadMode
           prog <- readloop inh
           hClose inh
           return prog

main :: IO (Program Name)
main = do inp <- readF
          return (comp (parse parseProg inp)) 

comp :: [(Program Name, Name)] -> Program Name
comp [] = error "no parse"
comp [(e,[])] = e
comp [(_,a)] = error ("doesn't use all input "++ a)

readloop inh = do ineof <- hIsEOF inh
                  if ineof
                   then return []
                    else do
                     x <- hGetLine inh
                     xs <- readloop inh
                     return (x ++ xs)
