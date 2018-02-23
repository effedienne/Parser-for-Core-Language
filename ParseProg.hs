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
alphanum =  sat isAlphaNum

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

initialIdent::Parser String
initialIdent = do x<-lower
                  return [x]
                <|> do x<-symbol "_"
                       return x

ident:: Parser String
ident = do x<- initialIdent
           xs <- many alphanum
           return (x++xs)

token::Parser a -> Parser a 
token p = do space
             value <- p 
             space 
             return value

symbol:: String->Parser String
symbol xs = token (string xs)

parseVar::Parser String
parseVar = do token ident

nat::Parser Int
nat = do xs <- some digit
         return (read xs)

int::Parser Int
int = do token(char '-')
         n <- token nat
         return (-n)
       <|> token nat   

parseExpr::Parser(Expr Name) 
parseExpr = parseLetrRec
            <|> parseLet
            <|> parseCase
            <|> parseLambda
            <|> parseBinApp

parseBinApp::Parser(Expr Name)
parseBinApp = do t1 <- parseExpr2
                 do symbol "|"
                    t2<-parseBinApp
                    return (EAp(EAp (EVar "|") t1) t2)
                  <|> return t1

parseExpr2::Parser(Expr Name)
parseExpr2 = do t1 <- parseExpr3
                do symbol "&"
                   t2<-parseExpr2
                   return (EAp(EAp (EVar "&") t1) t2)
                 <|> return t1

parseRelop::Parser String
parseRelop = symbol "=="
             <|> symbol"<="
             <|> symbol">="
             <|> symbol ">"
             <|> symbol "<"
             <|> symbol"~="

parseExpr3::Parser(Expr Name)
parseExpr3 = do t1 <- parseExpr4
                do s <- parseRelop
                   t2 <- parseExpr4
                   return (EAp(EAp (EVar s) t1) t2) 
                 <|> return t1

parseExpr4::Parser(Expr Name)
parseExpr4 = do t1 <- parseExpr5
                do symbol "+"
                   t2<-parseExpr4
                   return (EAp(EAp (EVar "+") t1) t2)
                 <|> do symbol "-"
                        t2<-parseExpr5
                        return (EAp(EAp (EVar "-") t1) t2)
                 <|> return t1

parseExpr5::Parser(Expr Name)
parseExpr5 = do t1 <- parseExpr6
                do symbol "*"
                   t2<-parseExpr5
                   return (EAp(EAp (EVar "*") t1) t2)
                 <|> do symbol "/"
                        t2<-parseExpr6
                        return (EAp(EAp (EVar "/") t1) t2)
                 <|> return t1

parseExpr6::Parser(Expr Name) -- EAP o aexpr
parseExpr6 =  do a <- parseAExpr
                 do b <- parseExpr6
                    return (EAp a b)
                  <|> return a

parseCase::Parser (Expr Name)
parseCase = do symbol "case"
               e <- parseExpr
               symbol "of"
               k <- parseAlt
               mm <- many parseAlt
               return (ECase e (k:mm))


parseAlt::Parser(Alter Name)
parseAlt = do symbol "<"
              n <- int 
              symbol ">"
              aa <- many parseVar
              symbol "->"
              e <- parseExpr 
              symbol ";"
              return(n,aa,e)

parseLambda::Parser (Expr Name)
parseLambda = do symbol "\\"
                 a <- parseVar
                 aa <- many parseVar
                 symbol "." --"->"
                 e <- parseExpr
                 return(Elam (a:aa) e)           

parseLetrRec::Parser (Expr Name) 
parseLetrRec = do symbol "letrec" 
                  def <- parseDef 
                  dd <- many parseDef 
                  symbol "in"  
                  e <- parseExpr
                  return(ELet Recursive (def:dd) e)

parseLet::Parser (Expr Name)     --nonrecurs
parseLet = do symbol "let" 
              def <- parseDef 
              dd <- many parseDef 
              symbol "in"  
              e <- parseExpr
              return(ELet NonRecursive (def:dd) e)

parseAExpr::Parser(Expr Name)
parseAExpr =
                   do v<-isVar
                      return (EVar v)
                    <|> do n <- int 
                           return (ENum n)
                    <|> do symbol "Pack"
                           symbol "{"
                           n1 <- int
                           symbol ","
                           n2 <- int
                           symbol "}"
                           return (EConstr n1 n2)
                    <|> do symbol "("
                           e <- parseExpr
                           symbol ")"
                           return e

parseDef::Parser(Def Name) 
parseDef = do v <- parseVar 
              symbol "="
              e <-parseAExpr
              return (v,e)


isVar::Parser String
isVar = do v <- parseVar
           if (isKey v ) 
            then empty
            else return v

isKey::String->Bool
isKey "case"  = True
isKey "let" =True
isKey "of"  = True
isKey "letrec" =True
isKey "in" =True
isKey _ =False
