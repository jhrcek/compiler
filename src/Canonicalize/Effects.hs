{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Canonicalize.Effects
  ( canonicalize
  , checkPayload
  )
  where

import qualified Data.Foldable as F
import qualified Data.Map as Map

import qualified AST.Canonical as Can
import qualified AST.Valid as Valid
import qualified AST.Module.Name as ModuleName
import qualified AST.Utils.Type as Type
import qualified Canonicalize.Environment as Env
import qualified Canonicalize.Type as Type
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Error
import qualified Reporting.Region as R
import qualified Reporting.Result as Result



-- RESULT


type Result i w a =
  Result.Result i w Error.Error a



-- CANONICALIZE


canonicalize
  :: Env.Env
  -> [A.Located Valid.Decl]
  -> Map.Map N.Name union
  -> Valid.Effects
  -> Result i w Can.Effects
canonicalize env decls unions effects =
  case effects of
    Valid.NoEffects ->
      Result.ok Can.NoEffects

    Valid.Ports ports ->
      do  pairs <- traverse (canonicalizePort env) ports
          return $ Can.Ports (Map.fromList pairs)

    Valid.Manager region manager ->
      let dict = Map.fromList (map toNameRegion decls) in
      Can.Manager
        <$> verifyManager region dict "init"
        <*> verifyManager region dict "onEffects"
        <*> verifyManager region dict "onSelfMsg"
        <*>
          case manager of
            Valid.Cmd cmdType ->
              Can.Cmd
                <$> verifyEffectType cmdType unions
                <*  verifyManager region dict "cmdMap"

            Valid.Sub subType ->
              Can.Sub
                <$> verifyEffectType subType unions
                <*  verifyManager region dict "subMap"

            Valid.Fx cmdType subType ->
              Can.Fx
                <$> verifyEffectType cmdType unions
                <*> verifyEffectType subType unions
                <*  verifyManager region dict "cmdMap"
                <*  verifyManager region dict "subMap"



-- CANONICALIZE PORT


canonicalizePort :: Env.Env -> Valid.Port -> Result i w (N.Name, Can.Port)
canonicalizePort env (Valid.Port (A.At region portName) tipe) =
  do  (Can.Forall freeVars ctipe) <- Type.toAnnotation env tipe
      case Type.deepDealias ctipe of
        Can.TLambda outgoingType (Can.TType home name [Can.TVar _])
          | home == ModuleName.cmd && name == "Cmd" ->
              case checkPayload outgoingType of
                Left (badType, err) ->
                  Result.throw (Error.PortPayloadInvalid region portName badType err)

                Right () ->
                  Result.ok (portName, Can.Outgoing freeVars outgoingType ctipe)

        Can.TLambda (Can.TLambda incomingType (Can.TVar msg1)) (Can.TType home name [Can.TVar msg2])
          | home == ModuleName.sub && name == "Sub" && msg1 == msg2 ->
              case checkPayload incomingType of
                Left (badType, err) ->
                  Result.throw (Error.PortPayloadInvalid region portName badType err)

                Right () ->
                  Result.ok (portName, Can.Incoming freeVars incomingType ctipe)

        _ ->
          Result.throw (Error.PortTypeInvalid region portName ctipe)



-- VERIFY MANAGER


verifyEffectType :: A.Located N.Name -> Map.Map N.Name a -> Result i w N.Name
verifyEffectType (A.At region name) unions =
  if Map.member name unions then
    Result.ok name
  else
    Result.throw (Error.EffectNotFound region name)


toNameRegion :: A.Located Valid.Decl -> (N.Name, R.Region)
toNameRegion (A.At _ (Valid.Decl (A.At region name) _ _ _)) =
  (name, region)


verifyManager :: R.Region -> Map.Map N.Name R.Region -> N.Name -> Result i w R.Region
verifyManager tagRegion decls name =
  case Map.lookup name decls of
    Just region ->
      Result.ok region

    Nothing ->
      Result.throw (Error.EffectFunctionNotFound tagRegion name)



-- CHECK PAYLOAD TYPES


checkPayload :: Can.Type -> Either (Can.Type, Error.InvalidPayload) ()
checkPayload tipe =
  case tipe of
    Can.TAlias _ _ args aliasedType ->
      checkPayload (Type.dealias args aliasedType)

    Can.TType home name args ->
      case args of
        []
          | isJson home name -> Right ()
          | isString home name -> Right ()
          | isIntFloatBool home name -> Right ()

        [arg]
          | isList  home name -> checkPayload arg
          | isMaybe home name -> checkPayload arg
          | isArray home name -> checkPayload arg

        _ ->
          Left (tipe, Error.UnsupportedType name)

    Can.TUnit ->
        Right ()

    Can.TTuple a b maybeC ->
        do  checkPayload a
            checkPayload b
            case maybeC of
              Nothing ->
                Right ()

              Just c ->
                checkPayload c

    Can.TVar name ->
        Left (tipe, Error.TypeVariable name)

    Can.TLambda _ _ ->
        Left (tipe, Error.Function)

    Can.TRecord _ (Just _) ->
        Left (tipe, Error.ExtendedRecord)

    Can.TRecord fields Nothing ->
        F.traverse_ checkPayload fields


isIntFloatBool :: ModuleName.Canonical -> N.Name -> Bool
isIntFloatBool home name =
  home == ModuleName.basics
  &&
  (name == N.int || name == N.float || name == N.bool)


isString :: ModuleName.Canonical -> N.Name -> Bool
isString home name =
  home == ModuleName.string
  &&
  name == N.string


isJson :: ModuleName.Canonical -> N.Name -> Bool
isJson home name =
  home == ModuleName.jsonEncode
  &&
  name == N.value


isList :: ModuleName.Canonical -> N.Name -> Bool
isList home name =
  home == ModuleName.list
  &&
  name == N.list


isMaybe :: ModuleName.Canonical -> N.Name -> Bool
isMaybe home name =
  home == ModuleName.maybe
  &&
  name == N.maybe


isArray :: ModuleName.Canonical -> N.Name -> Bool
isArray home name =
  home == ModuleName.array
  &&
  name == N.array
