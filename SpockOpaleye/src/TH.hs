{-# LANGUAGE TemplateHaskell #-}
module TH where

import Control.Lens
import Language.Haskell.TH
import Data.Char
import Data.List
import DataTypes

getTypeSegs :: Type -> [Type]
getTypeSegs a@(ConT n) = [a]
getTypeSegs (AppT a b) = (b : getTypeSegs a)
getTypeSegs r = error $ show r

typeToName :: Type -> Name
typeToName (ConT n)  = n

makeAudtableLenses :: TypeQ -> Q [Dec]
makeAudtableLenses tq= do
    a <- tq
    let type_segs = reverse $ getTypeSegs a
    let n = typeToName $ head type_segs
    field_names <- getRecordFields n
    type_params <- getTypeParams n
    concat <$> (sequence $ (mkFunc n type_params type_segs) <$> ((\(x, y) -> (nameBase x, y)) <$> field_names))
    where
      mkFunc :: Name -> [Name] -> [Type] -> (String, Type) -> Q [Dec]
      mkFunc n type_params type_segs (field, typ) = do
        let resolved_type = resolve_type typ type_params type_segs
        fname <- newName (drop 1 $ (toLower <$> field) ++ "A")
        t <- [t|Lens' (AuditM $(tq)) $(return resolved_type)|]
        exp <- mkFunc2 field
        return [SigD fname t, FunD fname $ [Clause [] (NormalB exp) []] ]
        where
          resolve_type :: Type -> [Name] -> [Type] -> Type
          resolve_type t@(ConT _) _ _ = t
          resolve_type (VarT n) tp ts = case (elemIndex n tp) of
            Just idx -> (ts !! (idx + 1))
            _ -> error "Unknown type variable"


getTypeParams :: Name -> Q [Name]
getTypeParams t_name = do
  info <- reify t_name
  case info of
    TyConI (DataD _ _ params _ [RecC _ t] _) -> return $ toName <$> params 
    _ -> error $ "Not a record!" ++ (show info)
    where
      toName (KindedTV n _) = n

getRecordFields :: Name -> Q [(Name, Type)]
getRecordFields t_name = do
  info <- reify t_name
  case info of
    TyConI (DataD _ _ _ _ [RecC _ t] _) -> return $ ext <$> t
    _ -> error $ "Not a record!" ++ (show info)
  where
    ext (a, _, t) = (a, t)

mkFunc2 :: String -> Q Exp
mkFunc2 name = do
  Just _field_name <- lookupValueName name
  fn <- lookupValueName $ "DataTypes." ++ (transformName name)
  case fn of
    Just field_name -> do
      [| lens ($(return  $ VarE _field_name)._data) (\r v -> r {
          _data = _data r & ($(return $ VarE field_name) .~ v),
          _log = ((name ++": " ++ (show $ (_data r ^. $(return $ VarE field_name))) ++ "->" ++ (show v)): (_log r))
        }) |]
    _ -> error $ "field accessor not found for " ++ name
  where
    transformName :: String -> String
    transformName name = toLower <$> (dropWhile isLower (drop 1 name))
