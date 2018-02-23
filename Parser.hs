module Parser where
import Control.Applicative as CA

type Name = String

type Program a = [ScDef a]

type ScDef a = (Name,[a],Expr a) 

type Def a = (a, Expr a)

type Alter a = (Int, [a], Expr a) 

data IsRec = NonRecursive | Recursive 
  deriving Show

data Expr a =  EVar Name | ENum Int | EConstr Int Int  | EAp (Expr a) (Expr a) | 
               ELet IsRec [Def a] (Expr a) | ECase (Expr a) [Alter a] | Elam [a] (Expr a) 
  deriving Show

newtype Parser a = P(String -> [(a,String)]) 

parse::Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

instance Functor Parser where
-- fmap::(a->b) ->Parser a ->Parser b
  fmap g p = P (\i -> case parse p i of
    []              -> []
    [(v, out)]      -> [(g v, out)])

instance Applicative Parser where
--pure::a->Parser a
  pure v = P(\i -> [(v,i)]) 

--(<*>)::Parser(a->b)->Parser a ->Parser b
  pg <*> px = P(\i -> case parse pg i of
                  []      -> []
                  [(v,s)] -> parse (fmap v px) s )

instance Monad Parser where
--(>>=)::Parser a -> (a->Parser b)-> Parser b
  p >>= fun = P(\i -> case parse p i of
                   []      -> []
                   [(v,s)] -> parse (fun v) s )

instance CA.Alternative Parser where
--empty:: Parser a
  empty = P(\i -> [])

--(<|>):: Parser a -> Parser a -> Parser a
  a <|> b = P(\i-> case parse a i of
                 []      -> parse b i
                 [(v,s)] -> [(v,s)])
