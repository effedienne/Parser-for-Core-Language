module ParseProg where
import Parser
import Data.Char
import Control.Applicative

parseProg::Parser(Program Name)
parseProg = do p <- parseScDef
               do a <- symbol ";"
                  ps <-  parseProg
                  return (p:ps)
                <|> return [p]

parseScDef::Parser(ScDef Name)
parseScDef = do 
                v  <- parseVar 
                pf <- many parseVar
                symbol "="
                body <- parseExpr 
                return (v,pf,body)

item::Parser Char
item = P(\i-> case i of 
              []     -> []
              (x:xs) -> [(x,xs)] )

sat::(Char -> Bool)-> Parser Char --satisfy
sat f = do
         k <- item          
         if f k
          then return k 
          else empty 

--function is alphanum is provided by Data.Char
alphanum:: Parser Char
alphanum = sat isAlphaNum

char:: Char -> Parser Char
char x = sat (==x) 

lower:: Parser Char
lower = sat isLower

digit:: Parser Char
digit = sat isDigit

space::Parser ()
space = do many (sat isSpace)
           return ()

string::String->Parser String
string [] = return [] 
string (x:xs) = do char x
                   string xs
                   return (x:xs)

ident:: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

token::Parser a -> Parser a 
token p = do space
             value <- p 
             space 
             return value

symbol:: String->Parser String
symbol xs = token (string xs)

parseVar::Parser String
parseVar = do token ident

isVar::Parser String
isVar =  parseVar
           
nat::Parser Int
nat = do xs <- some digit
         return (read xs)

int::Parser Int
int = do token(char '-')
         n <- token nat
         return (-n)
       <|> token nat   

parseAlt::Parser(Alter Name)
parseAlt = do symbol "<"
              n <- int 
              symbol ">"
              aa <- many parseVar
              symbol "->"
              e <- parseExpr
              return(n,aa,e)

parseExpr::Parser(Expr Name) 
parseExpr =      do symbol "case" --case
                    e <- parseExpr
                    symbol "of"
                    k <- parseAlt
                    mm <- many parseAlt
                    return (ECase e  (k:mm))
                  <|> do symbol "letrec" --nonrecurs
                         def <- parseDef
                         dd <- many parseDef  
                         symbol "in" 
                         e <- parseExpr
                         return(ELet Recursive (def:dd) e)
                       <|> do symbol "let" --nonrecurs
                              def <- parseDef 
                              dd <- many parseDef 
                              symbol "in"  
                              e <- parseExpr
                              return(ELet NonRecursive (def:dd) e)
                            <|> do symbol "\\"
                                   a <- parseVar
                                   aa <- many parseVar
                                   symbol "->"
                                   e <- parseExpr
                                   return(Elam (a:aa) e)
                                 <|> do a <-parseAExpr
                                        return a 

parseAExpr::Parser(Expr Name)
parseAExpr = do a <- isVar
                return (EVar a)
              <|> do n <- int 
                     return (ENum n)
                   <|> do symbol "Pack"             
                          n1 <- int
                          n2 <- int
                          return (EConstr n1 n2)
                        <|> do symbol "("
                               e <- parseExpr
                               symbol ")"
                               return(e)       

parseDef::Parser(Def Name) 
parseDef = do v <- parseVar
              symbol "="
              e <-parseExpr
              return (v,e)
