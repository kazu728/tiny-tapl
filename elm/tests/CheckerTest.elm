module CheckerTest exposing (suite)

import Checker exposing (Term(..), Type(..))
import Dict
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Checker.typecheck"
        [ test "真偽値の型を返す" <|
            \_ ->
                Checker.typecheck (BooleanLiteral True) Dict.empty
                    |> Expect.equal (Ok Boolean)
        , test "数値の型を返す" <|
            \_ ->
                Checker.typecheck (NumberLiteral 1) Dict.empty
                    |> Expect.equal (Ok Number)
        , test "数値同士を加算できる" <|
            \_ ->
                Checker.typecheck (Addition (NumberLiteral 1) (NumberLiteral 2)) Dict.empty
                    |> Expect.equal (Ok Number)
        , test "条件式の分岐の型を返す" <|
            \_ ->
                Checker.typecheck
                    (Conditional
                        (BooleanLiteral True)
                        (NumberLiteral 1)
                        (NumberLiteral 2)
                    )
                    Dict.empty
                    |> Expect.equal (Ok Number)
        , test "条件には真偽値を要求する" <|
            \_ ->
                Checker.typecheck
                    (Conditional
                        (NumberLiteral 1)
                        (NumberLiteral 2)
                        (NumberLiteral 3)
                    )
                    Dict.empty
                    |> Expect.equal (Err "boolean expected")
        , test "条件式の分岐には同じ型を要求する" <|
            \_ ->
                Checker.typecheck
                    (Conditional
                        (BooleanLiteral True)
                        (NumberLiteral 1)
                        (BooleanLiteral False)
                    )
                    Dict.empty
                    |> Expect.equal (Err "then and else have different types")
        , test "加算には数値を要求する" <|
            \_ ->
                Checker.typecheck (Addition (BooleanLiteral True) (NumberLiteral 1)) Dict.empty
                    |> Expect.equal (Err "number expected")
        , test "環境にある変数の型を返す" <|
            \_ ->
                Checker.typecheck (Variable "x") (Dict.fromList [ ( "x", Number ) ])
                    |> Expect.equal (Ok Number)
        , test "環境にない変数を拒否する" <|
            \_ ->
                Checker.typecheck (Variable "x") Dict.empty
                    |> Expect.equal (Err "unknown variable: x")
        , test "関数の型を返す" <|
            \_ ->
                Checker.typecheck
                    (Function [ { name = "x", type_ = Number } ] (Variable "x"))
                    Dict.empty
                    |> Expect.equal (Ok (Func [ { name = "x", type_ = Number } ] Number))
        , test "引数の型をパラメータの型に照合して関数を呼べる" <|
            \_ ->
                Checker.typecheck
                    (Call
                        (Function [ { name = "x", type_ = Number } ] (Variable "x"))
                        [ NumberLiteral 1 ]
                    )
                    Dict.empty
                    |> Expect.equal (Ok Number)
        , test "引数の数が合わない呼び出しを拒否する" <|
            \_ ->
                Checker.typecheck
                    (Call
                        (Function [ { name = "x", type_ = Number } ] (Variable "x"))
                        [ NumberLiteral 1, NumberLiteral 2 ]
                    )
                    Dict.empty
                    |> Expect.equal (Err "wrong number of arguments")
        , test "引数の型が合わない呼び出しを拒否する" <|
            \_ ->
                Checker.typecheck
                    (Call
                        (Function [ { name = "x", type_ = Number } ] (Variable "x"))
                        [ BooleanLiteral True ]
                    )
                    Dict.empty
                    |> Expect.equal (Err "parameter type mismatch")
        , test "関数でない値の呼び出しを拒否する" <|
            \_ ->
                Checker.typecheck
                    (Call (NumberLiteral 1) [])
                    Dict.empty
                    |> Expect.equal (Err "function type expected")
        ]