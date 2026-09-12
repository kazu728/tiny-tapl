module Decoder exposing (term)

import Checker exposing (Param, Property, PropertyTerm, Term(..), Type(..))
import Json.Decode as Decode exposing (Decoder)


term : Decoder Term
term =
    Decode.field "tag" Decode.string |> Decode.andThen termOfTag


termOfTag : String -> Decoder Term
termOfTag tag =
    case tag of
        "true" ->
            Decode.succeed (BooleanLiteral True)

        "false" ->
            Decode.succeed (BooleanLiteral False)

        "if" ->
            Decode.map3 Conditional
                (Decode.field "cond" lazyTerm)
                (Decode.field "thn" lazyTerm)
                (Decode.field "els" lazyTerm)

        "number" ->
            Decode.map NumberLiteral (Decode.field "n" Decode.float)

        "add" ->
            Decode.map2 Addition
                (Decode.field "left" lazyTerm)
                (Decode.field "right" lazyTerm)

        "var" ->
            Decode.map Variable (Decode.field "name" Decode.string)

        "func" ->
            Decode.map2 Function
                (Decode.field "params" (Decode.list paramDecoder))
                (Decode.field "body" lazyTerm)

        "call" ->
            Decode.map2 Call
                (Decode.field "func" lazyTerm)
                (Decode.field "args" (Decode.list lazyTerm))

        "seq" ->
            Decode.map2 Seq
                (Decode.field "body" lazyTerm)
                (Decode.field "rest" lazyTerm)

        "const" ->
            Decode.map3 Const
                (Decode.field "name" Decode.string)
                (Decode.field "init" lazyTerm)
                (Decode.field "rest" lazyTerm)

        "objectNew" ->
            Decode.map ObjectNew
                (Decode.field "props" (Decode.list propertyTermDecoder))

        "objectGet" ->
            Decode.map2 ObjectGet
                (Decode.field "obj" lazyTerm)
                (Decode.field "propName" Decode.string)

        _ ->
            Decode.fail ("未知の項です: " ++ tag)


paramDecoder : Decoder Param
paramDecoder =
    Decode.map2 Param
        (Decode.field "name" Decode.string)
        (Decode.field "type" typeDecoder)


propertyDecoder : Decoder Property
propertyDecoder =
    Decode.map2 Property
        (Decode.field "name" Decode.string)
        (Decode.field "type" typeDecoder)


propertyTermDecoder : Decoder PropertyTerm
propertyTermDecoder =
    Decode.map2 PropertyTerm
        (Decode.field "name" Decode.string)
        (Decode.field "term" lazyTerm)


typeDecoder : Decoder Type
typeDecoder =
    Decode.field "tag" Decode.string |> Decode.andThen typeOfTag


typeOfTag : String -> Decoder Type
typeOfTag tag =
    case tag of
        "Boolean" ->
            Decode.succeed Boolean

        "Number" ->
            Decode.succeed Number

        "Func" ->
            Decode.map2 Func
                (Decode.field "params" (Decode.list paramDecoder))
                (Decode.field "retType" lazyType)

        "Object" ->
            Decode.map Object
                (Decode.field "props" (Decode.list propertyDecoder))

        _ ->
            Decode.fail ("未知の型です: " ++ tag)


lazyTerm : Decoder Term
lazyTerm =
    Decode.lazy (\_ -> term)


lazyType : Decoder Type
lazyType =
    Decode.lazy (\_ -> typeDecoder)
