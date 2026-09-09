module Basic.DecoderTest exposing (suite)

import Basic exposing (Term(..))
import Basic.Decoder as Decoder
import Expect
import Json.Decode as Decode
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Basic.Decoder"
        [ test "項の構造を復元する" <|
            \_ ->
                decode addJson
                    |> Expect.equal
                        (Ok (Addition (NumberLiteral 1) (NumberLiteral 2)))
        , test "basic にない項を拒否する" <|
            \_ ->
                case decode unsupportedJson of
                    Ok _ ->
                        Expect.fail "デコードが成功してしまった"

                    Err error ->
                        String.contains "basic では扱えない項です: func" error
                            |> Expect.equal True
        ]


decode : String -> Result String Term
decode json =
    Decode.decodeString Decoder.term json
        |> Result.mapError Decode.errorToString


addJson : String
addJson =
    """{"tag":"add","left":{"tag":"number","n":1,"loc":{"start":{"line":1,"column":0},"end":{"line":1,"column":1}}},"right":{"tag":"number","n":2,"loc":{"start":{"line":1,"column":4},"end":{"line":1,"column":5}}},"loc":{"start":{"line":1,"column":0},"end":{"line":1,"column":5}}}"""


unsupportedJson : String
unsupportedJson =
    """{"tag":"func"}"""
