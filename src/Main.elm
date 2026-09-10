port module Main exposing (main)

import Checker
import Decoder
import Dict
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
            |> Result.andThen (\term -> Checker.typecheck term Dict.empty)
    of
        Ok ty ->
            stdout (Checker.show ty)

        Err error ->
            stderr error
