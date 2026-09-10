module DecoderTest exposing (suite)

import Checker exposing (Term(..), Type(..))
import Decoder
import Expect
import Json.Decode as Decode
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Decoder"
        [ test "項の構造を復元する" <|
            \_ ->
                decode addJson
                    |> Expect.equal
                        (Ok (Addition (NumberLiteral 1) (NumberLiteral 2)))
        , test "関数の項を復元する" <|
            \_ ->
                decode funcJson
                    |> Expect.equal
                        (Ok (Function [ { name = "x", type_ = Number } ] (Variable "x")))
        , test "seq の項を復元する" <|
            \_ ->
                decode seqJson
                    |> Expect.equal
                        (Ok (Seq (Addition (NumberLiteral 1) (NumberLiteral 2)) (BooleanLiteral True)))
        , test "const の項を復元する" <|
            \_ ->
                decode constJson
                    |> Expect.equal
                        (Ok (Const "x" (NumberLiteral 1) (Addition (Variable "x") (NumberLiteral 2))))
        , test "未知の項を拒否する" <|
            \_ ->
                case decode unsupportedJson of
                    Ok _ ->
                        Expect.fail "デコードが成功してしまった"

                    Err error ->
                        String.contains "未知の項です: obj" error
                            |> Expect.equal True
        ]


decode : String -> Result String Term
decode json =
    Decode.decodeString Decoder.term json
        |> Result.mapError Decode.errorToString


addJson : String
addJson =
    """{"tag":"add","left":{"tag":"number","n":1,"loc":{"start":{"line":1,"column":0},"end":{"line":1,"column":1}}},"right":{"tag":"number","n":2,"loc":{"start":{"line":1,"column":4},"end":{"line":1,"column":5}}},"loc":{"start":{"line":1,"column":0},"end":{"line":1,"column":5}}}"""


funcJson : String
funcJson =
    """{"tag":"func","params":[{"name":"x","type":{"tag":"Number"}}],"body":{"tag":"var","name":"x"}}"""


seqJson : String
seqJson =
    """{"tag":"seq","body":{"tag":"add","left":{"tag":"number","n":1},"right":{"tag":"number","n":2}},"rest":{"tag":"true"}}"""


constJson : String
constJson =
    """{"tag":"const","name":"x","init":{"tag":"number","n":1},"rest":{"tag":"add","left":{"tag":"var","name":"x"},"right":{"tag":"number","n":2}}}"""


unsupportedJson : String
unsupportedJson =
    """{"tag":"obj"}"""