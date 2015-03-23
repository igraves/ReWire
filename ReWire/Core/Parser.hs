-- TODO: Qualified names
-- TODO: do-notation

module ReWire.Core.Parser where

import ReWire.Core.Syntax
import Text.Parsec
import Text.Parsec.Language as L
import qualified Text.Parsec.Token as T
import Data.Char (isUpper,isLower)
import Data.List (nub)
import Data.Foldable (toList)

rwcDef :: T.LanguageDef st
rwcDef = L.haskellDef { T.reservedNames   = ["data","of","let","in","end","def","is","case","bind","vhdl","module","import"],
                        T.reservedOpNames = ["|","\\","->","::","<-"] }

lexer = T.makeTokenParser rwcDef

identifier     = T.identifier lexer
reserved       = T.reserved lexer
operator       = T.operator lexer
reservedOp     = T.reservedOp lexer
charLiteral    = T.charLiteral lexer
stringLiteral  = T.stringLiteral lexer
natural        = T.natural lexer
integer        = T.integer lexer
float          = T.float lexer
naturalOrFloat = T.naturalOrFloat lexer
decimal        = T.decimal lexer
hexadecimal    = T.hexadecimal lexer
octal          = T.octal lexer
symbol         = T.symbol lexer
lexeme         = T.lexeme lexer
whiteSpace     = T.whiteSpace lexer
parens         = T.parens lexer
braces         = T.braces lexer
angles         = T.angles lexer
brackets       = T.brackets lexer
squares        = T.squares lexer
semi           = T.semi lexer
comma          = T.comma lexer
colon          = T.colon lexer
dot            = T.dot lexer
semiSep        = T.semiSep lexer
semiSep1       = T.semiSep1 lexer
commaSep       = T.commaSep lexer
commaSep1      = T.commaSep1 lexer

name           = identifier

con_Unit                = "Prelude.()"
con_Tuple n | n < 2     = error "Parser: can't happen: tycon_Tuple arity < 2"
            | otherwise = "Prelude.(" ++ replicate (n-1) ',' ++ ")"
con_Arrow               = "Prelude.(->)"
mkArrow t1 t2 = TyApp (TyApp (TyVar con_Arrow) t1) t2

varid = lexeme $ try $
        do{ name <- identifier
          ; if isUpper (head name)
             then unexpected ("conid " ++ show name)
             else return name
          }

conid = lexeme $ try $
        do{ name <- identifier
          ; if isLower (head name)
             then unexpected ("varid " ++ show name)
             else return name
          }

import_ = do reserved "import"
             conid

module_ = do reserved "module"
             n    <- conid
             imps <- many import_
             dds  <- many datadecl
             pds  <- many primdefn
             defs <- many defn
             return (Module n dds pds defs)
             
datadecl = do reserved "data"
              i   <- conid
              tvs <- many varid
              reserved "is"
              dcs <- datacon `sepBy` reservedOp "|"
              reserved "end"
              return (Data i tvs dcs)

datacon = do i  <- conid
             ts <- many atype
             return (Ctor i ts)

atype = do tm <- angles ty
           tv <- angles ty
           return (TyComp tm tv)
    <|> do i <- identifier
           return (TyVar i)
    <|> do ts <- parens (ty `sepBy` comma)
           case ts of
             []  -> return (TyVar con_Unit)
             [t] -> return t
             _   -> return (foldl TyApp tc ts) where tc = TyVar (con_Tuple (length ts))
             
btype = do ts <- many1 atype
           return (foldl1 TyApp ts)

ty = do ts <- btype `sepBy1` reservedOp "->"
        return (foldr1 mkArrow ts)

mkPoly :: Ty String -> PolyTy String
mkPoly t = tvs t :-> t
  where tvs = nub . filter isTyVar . toList
        isTyVar = isLower . head

primdefn = do reserved "vhdl"
              i <- varid
              reservedOp "::"
              t <- ty
              reserved "is"
              n <- varid
              return (Prim i (mkPoly t) n)
              
defn = do i <- varid
          mt <- optionMaybe (reservedOp "::" >> ty)
          reserved "is"
          e <- expr
          reserved "end"
          return (Defn i (fmap mkPoly mt) e)

expr = lamexpr
   <|> do es <- many1 aexpr
          return (foldl1 App es)

aexpr = do i <- name
           return (Var i)
    <|> do es <- parens (expr `sepBy` comma)
           case es of
             []  -> return (Var con_Unit)
             [e] -> return e
             _   -> return (foldl App tc es) where tc = Var (con_Tuple (length es))
             
lamexpr = do reservedOp "\\"
             i <- name
             reservedOp "->"
             e <- expr
             return (Lam i e)
      <|> do reserved "case"
             e    <- expr
             reserved "of"
             alts <- braces (alt `sepBy` semi)
             return (Case e alts)
{-      <|> do reserved "bind"
             i  <- varid
             reservedOp "<-"
             ei <- expr
             reserved "in"
             eb <- expr
--             reserved "end"
--             return (RWCBind (mkId i) ei eb)
             return (RWCApp (RWCApp (RWCVar (mkId "bind") tblank) ei) (RWCLam (mkId i) tblank eb))-}

alt = do p <- pat
         reservedOp "->"
         e <- expr
         return (Alt p e)

pat = do i    <- conid
         pats <- many apat
         return (Pat i pats)
  <|> apat

apat = do i <- name
          return (Pat i [])
   <|> do symbol "_"
          return (Pat "_" [])
   <|> do ps <- parens (pat `sepBy` comma)
          case ps of
            []  -> return (Pat con_Unit [])
            [p] -> return p
            _   -> return (Pat (con_Tuple (length ps)) ps)

parse :: String -> Either String (Module String)
parse = parsewithname "<no filename>"

parsewithname :: FilePath -> String -> Either String (Module String)
parsewithname filename guts =
  case runParser (whiteSpace >> module_ >>= \ p -> whiteSpace >> eof >> return p) () filename guts of
    Left e  -> Left (show e)
    Right p -> Right p
            
parsefile :: FilePath -> IO (Either String (Module String))
parsefile fname = do guts <- readFile fname
                     return (parsewithname fname guts)
