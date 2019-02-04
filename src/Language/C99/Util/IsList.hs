{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}

module Language.C99.Util.IsList where

import GHC.Exts

import Language.C99.AST
import Language.C99.Util

error_emptylist name = error $ "Empty " ++ name ++ " is not allowed"

instance IsList TypeQualList where
  type Item TypeQualList = TypeQual
  fromList []     = error_emptylist "TypeQualList"
  fromList (x:xs) = foldl TypeQualCons (TypeQualBase x) xs
  toList = undefined


instance IsList StructDeclnList where
  type Item StructDeclnList = StructDecln
  fromList []     = error_emptylist "StructDeclnList"
  fromList (x:xs) = foldl StructDeclnCons (StructDeclnBase x) xs
  toList = undefined

instance IsList ArgExprList where
  type Item ArgExprList = AssignExpr
  fromList []     = error_emptylist "ArgExprList"
  fromList (x:xs) = foldl ArgExprListCons (ArgExprListBase x) xs
