port module Main exposing (main)

import Checker
import Decoder
import Dict
import Json.Decode as Decode
import Platform
import Result.Extra as Result


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
    Decode.decodeValue Decoder.term json
        |> Result.mapError Decode.errorToString
        |> Result.andThen (\term -> Checker.typecheck term Dict.empty)
        |> Result.unpack stderr (\ty -> stdout (Checker.show ty))
