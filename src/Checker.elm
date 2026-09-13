module Checker exposing (Param, Property, PropertyTerm, Term(..), Type(..), TypeEnv, show, typecheck)

import Dict exposing (Dict)
import List.Extra as List


type Type
    = Boolean
    | Number
    | Func (List Param) Type
    | Object (List Property)


type alias Param =
    { name : String
    , type_ : Type
    }


type alias Property =
    { name : String
    , type_ : Type
    }


type alias PropertyTerm =
    { name : String
    , term : Term
    }


type Term
    = BooleanLiteral Bool
    | NumberLiteral Float
    | Conditional Term Term Term
    | Addition Term Term
    | Variable String
    | Function (List Param) Term
    | Call Term (List Term)
    | Seq Term Term
    | Const String Term Term
    | RecFunc String (List Param) Type Term Term
    | ObjectNew (List PropertyTerm)
    | ObjectGet Term String


type alias TypeEnv =
    Dict String Type


typecheck : Term -> TypeEnv -> Result String Type
typecheck t env =
    case t of
        BooleanLiteral _ ->
            Ok Boolean

        NumberLiteral _ ->
            Ok Number

        Conditional condition thenBranch elseBranch ->
            expect Boolean condition env
                |> Result.andThen
                    (\_ ->
                        Result.map2 Tuple.pair
                            (typecheck thenBranch env)
                            (typecheck elseBranch env)
                    )
                |> Result.andThen
                    (\( thenType, elseType ) ->
                        if typeEq thenType elseType then
                            Ok thenType

                        else
                            Err "then and else have different types"
                    )

        Addition left right ->
            expect Number left env
                |> Result.andThen (\_ -> expect Number right env)

        Variable name ->
            lookup name env

        Function params body ->
            addParams params env
                |> typecheck body
                |> Result.map (Func params)

        Call func args ->
            typecheck func env
                |> Result.andThen
                    (\funcType ->
                        case funcType of
                            Func params retType ->
                                if List.length params /= List.length args then
                                    Err "wrong number of arguments"

                                else
                                    checkArgs params args env
                                        |> Result.map (\_ -> retType)

                            _ ->
                                Err "function type expected"
                    )

        Seq body rest ->
            typecheck body env
                |> Result.andThen (\_ -> typecheck rest env)

        Const name init rest ->
            typecheck init env
                |> Result.andThen
                    (\initType -> typecheck rest (Dict.insert name initType env))

        RecFunc funcName params retType body rest ->
            let
                funcType =
                    Func params retType
            in
            addParams params env
                |> Dict.insert funcName funcType
                |> typecheck body
                |> Result.andThen
                    (\bodyType ->
                        if typeEq bodyType retType then
                            env
                                |> Dict.insert funcName funcType
                                |> typecheck rest

                        else
                            Err "wrong return type"
                    )

        ObjectNew props ->
            props
                |> List.map
                    (\prop ->
                        typecheck prop.term env
                            |> Result.map (\propType -> { name = prop.name, type_ = propType })
                    )
                |> sequence
                |> Result.map Object

        ObjectGet obj propName ->
            typecheck obj env
                |> Result.andThen
                    (\objType ->
                        case objType of
                            Object props ->
                                getProp propName props

                            _ ->
                                Err "object expected"
                    )


expect : Type -> Term -> TypeEnv -> Result String Type
expect expected t env =
    typecheck t env
        |> Result.andThen
            (\actual ->
                if typeEq actual expected then
                    Ok actual

                else
                    Err (show expected ++ " expected")
            )


typeEq : Type -> Type -> Bool
typeEq ty1 ty2 =
    case ( ty1, ty2 ) of
        ( Boolean, Boolean ) ->
            True

        ( Number, Number ) ->
            True

        ( Func params1 ret1, Func params2 ret2 ) ->
            List.map .type_ params1 == List.map .type_ params2 && typeEq ret1 ret2

        ( Object props1, Object props2 ) ->
            props1 == props2

        _ ->
            False


lookup : String -> TypeEnv -> Result String Type
lookup name env =
    case Dict.get name env of
        Just ty ->
            Ok ty

        Nothing ->
            Err ("unknown variable: " ++ name)


addParams : List Param -> TypeEnv -> TypeEnv
addParams params env =
    List.foldl (\p env_ -> Dict.insert p.name p.type_ env_) env params


checkArgs : List Param -> List Term -> TypeEnv -> Result String ()
checkArgs params args env =
    case ( params, args ) of
        ( [], [] ) ->
            Ok ()

        ( p :: paramRest, arg :: argRest ) ->
            typecheck arg env
                |> Result.andThen
                    (\argType ->
                        if typeEq argType p.type_ then
                            checkArgs paramRest argRest env

                        else
                            Err "parameter type mismatch"
                    )

        _ ->
            Err "wrong number of arguments"


sequence : List (Result String a) -> Result String (List a)
sequence =
    List.foldr (Result.map2 (::)) (Ok [])


getProp : String -> List Property -> Result String Type
getProp name props =
    case List.find (\p -> p.name == name) props of
        Just prop ->
            Ok prop.type_

        Nothing ->
            Err ("unknown property: " ++ name)


show : Type -> String
show type_ =
    case type_ of
        Boolean ->
            "boolean"

        Number ->
            "number"

        Func _ _ ->
            "function"

        Object _ ->
            "object"
