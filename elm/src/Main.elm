port module Main exposing (main)

import Basic
import Basic.Decoder as Decoder
import Json.Decode as Decode
import Platform


port stdout : String -> Cmd msg


port stderr : String -> Cmd msg


main : Program Decode.Value () Never
main =
    Platform.worker
        { init = \json -> ( (), report json )
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


report : Decode.Value -> Cmd msg
report json =
    case
        Decode.decodeValue Decoder.term json
            |> Result.mapError Decode.errorToString
            |> Result.andThen Basic.typecheck
    of
        Ok ty ->
            stdout (Basic.show ty)

        Err error ->
            stderr error
