{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module TH where

import Control.Lens
import Language.Haskell.TH
import Data.Char
import Data.List (elemIndex)
import DataTypes
import qualified Data.HashMap.Strict as HM
import Data.Text (pack)

import Data.Aeson (Value(..), ToJSON(..))

getTypeSegs :: Type -> [Type]
getTypeSegs a@(ConT _) = [a]
getTypeSegs (AppT a b) = (b : getTypeSegs a)
getTypeSegs r = error $ show r

typeToName :: Type -> Name
typeToName (ConT n)  = n
typeToName _  = error "Unsupported type constructor"

getType :: Name -> TypeQ
getType tn = do
  info <- reify tn
  case info of
    TyConI (TySynD _ _ tpe) -> return tpe
    _ -> error "Not a type syn"
    
makeAudtableLenses :: Name -> Q [Dec]
makeAudtableLenses tq= do
    a <- getType tq
    let type_segs = reverse $ getTypeSegs a
    let n = typeToName $ head type_segs
    let record_name = nameBase n
    field_names <- getRecordFields n
    type_params <- getTypeParams n
    concat <$> (sequence $ (mkInstanceDef record_name a type_params type_segs) <$> ((\(x, y) -> (nameBase x, y)) <$> field_names))
    where
      mkInstanceDef :: String -> Type -> [Name] -> [Type] -> (String, Type) -> Q [Dec]
      mkInstanceDef rec_name t_type type_params type_segs (field, typ) = do
        let resolved_type = resolve_type typ type_params type_segs
        let fname_rt = (drop (1+(length rec_name)) $ (toLower <$> field))
        Just fname_ap <- lookupValueName fname_rt
        expr <- mkInstanceFunction field
        do
          let tc = "Has" ++ (uc_first fname_rt)
          maybe_c <- lookupTypeName tc
          case maybe_c of
            Just c_name -> do
              aud_t <- [t| Auditable $(return t_type) |]
              return $ [InstanceD Nothing [] (AppT (AppT (ConT c_name) aud_t) resolved_type)  [FunD (fname_ap) $ [Clause [] (NormalB expr) []] ]]
            _ -> error $ "Typeclass " ++ tc ++ " not found"
        where
          uc_first (x:xs) = (toUpper x):xs
          uc_first [] = []
          resolve_type :: Type -> [Name] -> [Type] -> Type
          resolve_type t@(ConT _) _ _ = t
          resolve_type (VarT n) tp ts = case (elemIndex n tp) of
            Just idx -> (ts !! (idx + 1))
            _ -> error "Unknown type variable"
          resolve_type _ _ _ = error "Should be a type variable or a concrete type"

getTypeParams :: Name -> Q [Name]
getTypeParams t_name = do
  info <- reify t_name
  case info of
    TyConI (DataD _ _ params _ [RecC _ _] _) -> return $ toName <$> params 
    _ -> error $ "Not a record!" ++ (show info)
    where
      toName (KindedTV n _) = n
      toName _ = error "Unexpected type variable type"

getRecordFields :: Name -> Q [(Name, Type)]
getRecordFields t_name = do
  info <- reify t_name
  case info of
    TyConI (DataD _ _ _ _ [RecC _ t] _) -> return $ ext <$> t
    _ -> error $ "Not a record!" ++ (show info)
  where
    ext (a, _, t) = (a, t)

makeLog :: (ToJSON a) => Value -> String -> a -> a -> Value
makeLog current_log field_name old new = case current_log of
  Object v -> Object $ HM.insert field_name_txt (getLog old new) v
  _ -> error "Unexpected value in the log field"
  where
    field_name_txt = pack field_name
    getLog ::  (ToJSON v) => v -> v -> Value
    getLog old' new' = Object $ HM.insert "old" (toJSON old') $ HM.insert "new" (toJSON new') $ HM.empty

mkInstanceFunction :: String -> Q Exp
mkInstanceFunction nam = do
  Just _field_name <- lookupValueName nam
  -- TODO remove dependency on "DataTypes" module name or don't hard code it in.
  fn <- lookupValueName $ "DataTypes." ++ (transformName nam)
  case fn of
    Just field_name -> do
      [| lens ($(return  $ VarE _field_name)._data) (\r v -> r {
          _data = _data r & ($(return $ VarE field_name) .~ v),
          _log = makeLog (_log r) nam (_data r ^. $(return $ VarE field_name)) v 
        }) |]
    _ -> error $ "field accessor not found for " ++ nam
  where
    transformName :: String -> String
    transformName na = toLower <$> (dropWhile isLower (drop 1 na))
