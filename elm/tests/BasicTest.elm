module BasicTest exposing (suite)

import Basic exposing (Term(..), Type(..))
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Basic.typecheck"
        [ test "真偽値の型を返す" <|
            \_ ->
                Basic.typecheck (BooleanLiteral True)
                    |> Expect.equal (Ok Boolean)
        , test "数値の型を返す" <|
            \_ ->
                Basic.typecheck (NumberLiteral 1)
                    |> Expect.equal (Ok Number)
        , test "数値同士を加算できる" <|
            \_ ->
                Basic.typecheck (Addition (NumberLiteral 1) (NumberLiteral 2))
                    |> Expect.equal (Ok Number)
        , test "条件式の分岐の型を返す" <|
            \_ ->
                Basic.typecheck
                    (Conditional
                        (BooleanLiteral True)
                        (NumberLiteral 1)
                        (NumberLiteral 2)
                    )
                    |> Expect.equal (Ok Number)
        , test "条件には真偽値を要求する" <|
            \_ ->
                Basic.typecheck
                    (Conditional
                        (NumberLiteral 1)
                        (NumberLiteral 2)
                        (NumberLiteral 3)
                    )
                    |> Expect.equal (Err "boolean expected")
        , test "条件式の分岐には同じ型を要求する" <|
            \_ ->
                Basic.typecheck
                    (Conditional
                        (BooleanLiteral True)
                        (NumberLiteral 1)
                        (BooleanLiteral False)
                    )
                    |> Expect.equal (Err "then and else have different types")
        , test "加算には数値を要求する" <|
            \_ ->
                Basic.typecheck (Addition (BooleanLiteral True) (NumberLiteral 1))
                    |> Expect.equal (Err "number expected")
        ]
