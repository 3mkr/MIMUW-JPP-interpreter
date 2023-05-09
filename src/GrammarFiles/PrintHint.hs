-- File generated by the BNF Converter (bnfc 2.9.4.1).

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for PrintHint.

module PrintHint where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified AbsHint

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 False (map ($ "") $ d []) ""
  where
  rend
    :: Int        -- ^ Indentation level.
    -> Bool       -- ^ Pending indentation to be output before next character?
    -> [String]
    -> ShowS
  rend i p = \case
      "["      :ts -> char '[' . rend i False ts
      "("      :ts -> char '(' . rend i False ts
      "{"      :ts -> onNewLine i     p . showChar   '{'  . new (i+1) ts
      "}" : ";":ts -> onNewLine (i-1) p . showString "};" . new (i-1) ts
      "}"      :ts -> onNewLine (i-1) p . showChar   '}'  . new (i-1) ts
      [";"]        -> char ';'
      ";"      :ts -> char ';' . new i ts
      t  : ts@(s:_) | closingOrPunctuation s
                   -> pending . showString t . rend i False ts
      t        :ts -> pending . space t      . rend i False ts
      []           -> id
    where
    -- Output character after pending indentation.
    char :: Char -> ShowS
    char c = pending . showChar c

    -- Output pending indentation.
    pending :: ShowS
    pending = if p then indent i else id

  -- Indentation (spaces) for given indentation level.
  indent :: Int -> ShowS
  indent i = replicateS (2*i) (showChar ' ')

  -- Continue rendering in new line with new indentation.
  new :: Int -> [String] -> ShowS
  new j ts = showChar '\n' . rend j True ts

  -- Make sure we are on a fresh line.
  onNewLine :: Int -> Bool -> ShowS
  onNewLine i p = (if p then id else showChar '\n') . indent i

  -- Separate given string from following text by a space (if needed).
  space :: String -> ShowS
  space t s =
    case (all isSpace t', null spc, null rest) of
      (True , _   , True ) -> []              -- remove trailing space
      (False, _   , True ) -> t'              -- remove trailing space
      (False, True, False) -> t' ++ ' ' : s   -- add space if none
      _                    -> t' ++ s
    where
      t'          = showString t []
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt i = concatD . map (prt i)

instance Print Char where
  prt _ c = doc (showChar '\'' . mkEsc '\'' c . showChar '\'')

instance Print String where
  prt _ = printString

printString :: String -> Doc
printString s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  s -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print AbsHint.Ident where
  prt _ (AbsHint.Ident i) = doc $ showString i
instance Print (AbsHint.Program' a) where
  prt i = \case
    AbsHint.Program _ topdefs -> prPrec i 0 (concatD [prt 0 topdefs])

instance Print (AbsHint.TopDef' a) where
  prt i = \case
    AbsHint.FnDef _ type_ id_ args block -> prPrec i 0 (concatD [prt 0 type_, prt 0 id_, doc (showString "("), prt 0 args, doc (showString ")"), prt 0 block])

instance Print [AbsHint.TopDef' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print (AbsHint.Arg' a) where
  prt i = \case
    AbsHint.Arg _ type_ id_ -> prPrec i 0 (concatD [prt 0 type_, prt 0 id_])

instance Print [AbsHint.Arg' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (AbsHint.Block' a) where
  prt i = \case
    AbsHint.Block _ stmts -> prPrec i 0 (concatD [doc (showString "{"), prt 0 stmts, doc (showString "}")])

instance Print [AbsHint.Stmt' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print (AbsHint.Stmt' a) where
  prt i = \case
    AbsHint.Empty _ -> prPrec i 0 (concatD [doc (showString ";")])
    AbsHint.BStmt _ block -> prPrec i 0 (concatD [prt 0 block])
    AbsHint.Decl _ type_ items -> prPrec i 0 (concatD [prt 0 type_, prt 0 items, doc (showString ";")])
    AbsHint.Ass _ id_ expr -> prPrec i 0 (concatD [prt 0 id_, doc (showString "="), prt 0 expr, doc (showString ";")])
    AbsHint.Incr _ id_ -> prPrec i 0 (concatD [prt 0 id_, doc (showString "++"), doc (showString ";")])
    AbsHint.Decr _ id_ -> prPrec i 0 (concatD [prt 0 id_, doc (showString "--"), doc (showString ";")])
    AbsHint.Ret _ expr -> prPrec i 0 (concatD [doc (showString "return"), prt 0 expr, doc (showString ";")])
    AbsHint.VRet _ -> prPrec i 0 (concatD [doc (showString "return"), doc (showString ";")])
    AbsHint.Cond _ expr block -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 block])
    AbsHint.CondElse _ expr block1 block2 -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 block1, doc (showString "else"), prt 0 block2])
    AbsHint.While _ expr block -> prPrec i 0 (concatD [doc (showString "while"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 block])
    AbsHint.For _ id_ expr1 expr2 block -> prPrec i 0 (concatD [doc (showString "for"), doc (showString "("), prt 0 id_, doc (showString ":="), prt 0 expr1, doc (showString "to"), prt 0 expr2, doc (showString ")"), prt 0 block])
    AbsHint.SExp _ expr -> prPrec i 0 (concatD [prt 0 expr, doc (showString ";")])
    AbsHint.BreakExp _ -> prPrec i 0 (concatD [doc (showString "break"), doc (showString ";")])
    AbsHint.ContExp _ -> prPrec i 0 (concatD [doc (showString "continue"), doc (showString ";")])
    AbsHint.Print _ expr -> prPrec i 0 (concatD [doc (showString "print"), doc (showString "("), prt 0 expr, doc (showString ")")])
    AbsHint.Printf _ expr1 expr2 expr3 str -> prPrec i 0 (concatD [doc (showString "printf"), doc (showString "("), prt 0 expr1, prt 0 expr2, prt 0 expr3, printString str, doc (showString ")")])

instance Print (AbsHint.Item' a) where
  prt i = \case
    AbsHint.NoInit _ id_ -> prPrec i 0 (concatD [prt 0 id_])
    AbsHint.Init _ id_ expr -> prPrec i 0 (concatD [prt 0 id_, doc (showString "="), prt 0 expr])

instance Print [AbsHint.Item' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (AbsHint.Type' a) where
  prt i = \case
    AbsHint.Int _ -> prPrec i 0 (concatD [doc (showString "int")])
    AbsHint.Str _ -> prPrec i 0 (concatD [doc (showString "string")])
    AbsHint.Bool _ -> prPrec i 0 (concatD [doc (showString "boolean")])
    AbsHint.Void _ -> prPrec i 0 (concatD [doc (showString "void")])
    AbsHint.Tuple _ types -> prPrec i 0 (concatD [doc (showString "tuple"), doc (showString "<"), prt 0 types, doc (showString ">")])
    AbsHint.Array _ type_ -> prPrec i 0 (concatD [prt 0 type_, doc (showString "[]")])
    AbsHint.Fun _ type_ types -> prPrec i 0 (concatD [prt 0 type_, doc (showString "("), prt 0 types, doc (showString ")")])

instance Print [AbsHint.Type' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (AbsHint.Expr' a) where
  prt i = \case
    AbsHint.ETuple _ exprs -> prPrec i 6 (concatD [doc (showString "?"), prt 0 exprs, doc (showString "?")])
    AbsHint.EArr _ exprs -> prPrec i 6 (concatD [doc (showString "["), prt 0 exprs, doc (showString "]")])
    AbsHint.EArrIdx _ expr1 expr2 -> prPrec i 6 (concatD [prt 6 expr1, doc (showString "["), prt 0 expr2, doc (showString "]")])
    AbsHint.EVar _ id_ -> prPrec i 6 (concatD [prt 0 id_])
    AbsHint.ELitInt _ n -> prPrec i 6 (concatD [prt 0 n])
    AbsHint.ELitTrue _ -> prPrec i 6 (concatD [doc (showString "true")])
    AbsHint.ELitFalse _ -> prPrec i 6 (concatD [doc (showString "false")])
    AbsHint.EApp _ id_ exprs -> prPrec i 6 (concatD [prt 0 id_, doc (showString "("), prt 0 exprs, doc (showString ")")])
    AbsHint.EString _ str -> prPrec i 6 (concatD [printString str])
    AbsHint.Neg _ expr -> prPrec i 5 (concatD [doc (showString "-"), prt 6 expr])
    AbsHint.Not _ expr -> prPrec i 5 (concatD [doc (showString "!"), prt 6 expr])
    AbsHint.EMul _ expr1 mulop expr2 -> prPrec i 4 (concatD [prt 4 expr1, prt 0 mulop, prt 5 expr2])
    AbsHint.EAdd _ expr1 addop expr2 -> prPrec i 3 (concatD [prt 3 expr1, prt 0 addop, prt 4 expr2])
    AbsHint.ERel _ expr1 relop expr2 -> prPrec i 2 (concatD [prt 2 expr1, prt 0 relop, prt 3 expr2])
    AbsHint.EAnd _ expr1 expr2 -> prPrec i 1 (concatD [prt 2 expr1, doc (showString "&&"), prt 1 expr2])
    AbsHint.EOr _ expr1 expr2 -> prPrec i 0 (concatD [prt 1 expr1, doc (showString "||"), prt 0 expr2])

instance Print [AbsHint.Expr' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (AbsHint.AddOp' a) where
  prt i = \case
    AbsHint.Plus _ -> prPrec i 0 (concatD [doc (showString "+")])
    AbsHint.Minus _ -> prPrec i 0 (concatD [doc (showString "-")])

instance Print (AbsHint.MulOp' a) where
  prt i = \case
    AbsHint.Times _ -> prPrec i 0 (concatD [doc (showString "*")])
    AbsHint.Div _ -> prPrec i 0 (concatD [doc (showString "/")])
    AbsHint.Mod _ -> prPrec i 0 (concatD [doc (showString "%")])

instance Print (AbsHint.RelOp' a) where
  prt i = \case
    AbsHint.LTH _ -> prPrec i 0 (concatD [doc (showString "<")])
    AbsHint.LE _ -> prPrec i 0 (concatD [doc (showString "<=")])
    AbsHint.GTH _ -> prPrec i 0 (concatD [doc (showString ">")])
    AbsHint.GE _ -> prPrec i 0 (concatD [doc (showString ">=")])
    AbsHint.EQU _ -> prPrec i 0 (concatD [doc (showString "==")])
    AbsHint.NE _ -> prPrec i 0 (concatD [doc (showString "!=")])
