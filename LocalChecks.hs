
module LocalChecks (phase) where

import AST
import Message

phase :: Namespace -> E Namespace
phase (Namespace name imps defs) = do
  let imps' = mergeImports imps
  mapM_ checkDef defs
  return $ Namespace name imps' defs

mergeImports imps =
  map mergeImportGroup
  $ groupBy (\a b -> iFrom a == iFrom b)
  $ sortBy (\a b -> compare (iFrom a) (iFrom b))

mergeImportGroup imps =
  Import (iFrom $ head imps) $ foldr1 combineImportLists $ map iNames imps)

combineImportLists xs ys = do
  xs <- xs
  ys <- ys
  return $ xs ++ ys

elemHasType = isJust . seType

unless = when . not

checkDef NsValueDef { }    = return ()
checkDef NsTypeAlias { }   = return ()
checkDef def@NsTypeDef { } = do
  let kind  = tdKind def
  let elems = tdElems def
  when (kind `elem` [Struct, Union]) $ do
    -- Check that each element has a type.
    unless (all elemHasType $ elems) $ err 
  when (kind == Struct) $ do
    -- Check that each element has an initial value.
  when (kind == Union) $ do
    -- Check that exactly one element has an initial value.
  when (kind `elem` [Enum, Mask]) $ do
    -- Check that there are no dynamic parameters.
    -- Check that no element has a type.
    -- Check that either all of the elements have initial values, or none of them.
    -- Check that all initial values are integer constants.
    -- Check that each initial value is unique.
  when (kind == Mask) $ do
    -- Check that all initial values are powers of two.
  
