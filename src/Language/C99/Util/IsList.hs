{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}

module Language.C99.Util.IsList where

import GHC.Exts

import Language.C99.AST

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

instance IsList InitList where
  type Item InitList = Init
  fromList []     = error_emptylist "InitExprList"
  fromList (x:xs) = foldl step base xs where
    base      = InitBase Nothing x
    step ys y = InitCons ys Nothing y

instance IsList BlockItemList where
  type Item BlockItemList = BlockItem
  fromList []     = error_emptylist "BlockItemList"
  fromList (x:xs) = foldl BlockItemCons (BlockItemBase x) xs

instance IsList TransUnit where
  type Item TransUnit = ExtDecln
  fromList []     = error_emptylist "TransUnit"
  fromList (x:xs) = foldl TransUnitCons (TransUnitBase x) xs

instance IsList DeclnList where
  type Item DeclnList = Decln
  fromList []     = error_emptylist "DeclnList"
  fromList (x:xs) = foldl DeclnCons (DeclnBase x) xs
