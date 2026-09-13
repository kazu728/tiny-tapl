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
        , test "const は束縛を残りの式に引き継ぐ" <|
            \_ ->
                Checker.typecheck
                    (Const "x" (NumberLiteral 1) (Variable "x"))
                    Dict.empty
                    |> Expect.equal (Ok Number)
        , test "const の初期化式は束縛前に検査される" <|
            \_ ->
                Checker.typecheck
                    (Const "x" (Variable "x") (NumberLiteral 1))
                    Dict.empty
                    |> Expect.equal (Err "unknown variable: x")
        , test "const の初期化式は元の環境で検査される" <|
            \_ ->
                Checker.typecheck
                    (Const "x" (Variable "y") (Variable "x"))
                    (Dict.fromList [ ( "y", Number ) ])
                    |> Expect.equal (Ok Number)
        , test "const で束縛した関数を呼べる" <|
            \_ ->
                Checker.typecheck
                    (Const
                        "f"
                        (Function [ { name = "x", type_ = Number } ] (Variable "x"))
                        (Call (Variable "f") [ NumberLiteral 1 ])
                    )
                    Dict.empty
                    |> Expect.equal (Ok Number)
        , test "seq は body の型を捨てて残りの式の型を返す" <|
            \_ ->
                Checker.typecheck
                    (Seq (NumberLiteral 1) (BooleanLiteral True))
                    Dict.empty
                    |> Expect.equal (Ok Boolean)
        , test "seq は body も型検査する" <|
            \_ ->
                Checker.typecheck
                    (Seq (Variable "x") (NumberLiteral 1))
                    Dict.empty
                    |> Expect.equal (Err "unknown variable: x")
        , test "recFunc の body と rest で自分自身を参照できる" <|
            \_ ->
                Checker.typecheck
                    (RecFunc "f"
                        [ { name = "b", type_ = Boolean }, { name = "n", type_ = Number } ]
                        Number
                        (Conditional
                            (Variable "b")
                            (NumberLiteral 1)
                            (Call (Variable "f") [ BooleanLiteral True, Variable "n" ])
                        )
                        (Call (Variable "f") [ BooleanLiteral False, NumberLiteral 2 ])
                    )
                    Dict.empty
                    |> Expect.equal (Ok Number)
        , test "recFunc の戻り型が body と違えば拒否する" <|
            \_ ->
                Checker.typecheck
                    (RecFunc "f"
                        [ { name = "n", type_ = Number } ]
                        Number
                        (BooleanLiteral True)
                        (NumberLiteral 0)
                    )
                    Dict.empty
                    |> Expect.equal (Err "wrong return type")
        , test "recFunc の自己呼び出しも型検査する" <|
            \_ ->
                Checker.typecheck
                    (RecFunc "f"
                        [ { name = "n", type_ = Number } ]
                        Number
                        (Call (Variable "f") [ BooleanLiteral True ])
                        (Variable "f")
                    )
                    Dict.empty
                    |> Expect.equal (Err "parameter type mismatch")
        , test "recFunc のパラメータは rest から参照できない" <|
            \_ ->
                Checker.typecheck
                    (RecFunc "f"
                        [ { name = "n", type_ = Number } ]
                        Number
                        (Variable "n")
                        (Variable "n")
                    )
                    Dict.empty
                    |> Expect.equal (Err "unknown variable: n")
        , test "recFunc の名前は外に漏れない" <|
            \_ ->
                Checker.typecheck
                    (Seq
                        (RecFunc "f" [ { name = "n", type_ = Number } ] Number (Variable "n") (NumberLiteral 0))
                        (Variable "f")
                    )
                    Dict.empty
                    |> Expect.equal (Err "unknown variable: f")
        , test "オブジェクトの型を返す" <|
            \_ ->
                Checker.typecheck
                    (ObjectNew
                        [ { name = "x", term = NumberLiteral 1 }
                        , { name = "y", term = BooleanLiteral True }
                        ]
                    )
                    Dict.empty
                    |> Expect.equal
                        (Ok
                            (Object
                                [ { name = "x", type_ = Number }
                                , { name = "y", type_ = Boolean }
                                ]
                            )
                        )
        , test "オブジェクトのプロパティも型検査する" <|
            \_ ->
                Checker.typecheck
                    (ObjectNew [ { name = "x", term = Variable "nope" } ])
                    Dict.empty
                    |> Expect.equal (Err "unknown variable: nope")
        , test "プロパティ取得はプロパティの型を返す" <|
            \_ ->
                Checker.typecheck
                    (ObjectGet
                        (ObjectNew [ { name = "x", term = NumberLiteral 1 } ])
                        "x"
                    )
                    Dict.empty
                    |> Expect.equal (Ok Number)
        , test "存在しないプロパティの取得を拒否する" <|
            \_ ->
                Checker.typecheck
                    (ObjectGet
                        (ObjectNew [ { name = "x", term = NumberLiteral 1 } ])
                        "y"
                    )
                    Dict.empty
                    |> Expect.equal (Err "unknown property: y")
        , test "オブジェクトでない値のプロパティ取得を拒否する" <|
            \_ ->
                Checker.typecheck
                    (ObjectGet (NumberLiteral 1) "x")
                    Dict.empty
                    |> Expect.equal (Err "object expected")
        , test "プロパティが一致すればオブジェクト同士は同じ型になる" <|
            \_ ->
                Checker.typecheck
                    (Conditional
                        (BooleanLiteral True)
                        (ObjectNew [ { name = "x", term = NumberLiteral 1 } ])
                        (ObjectNew [ { name = "x", term = NumberLiteral 2 } ])
                    )
                    Dict.empty
                    |> Expect.equal (Ok (Object [ { name = "x", type_ = Number } ]))
        , test "プロパティが違うオブジェクト同士は別の型になる" <|
            \_ ->
                Checker.typecheck
                    (Conditional
                        (BooleanLiteral True)
                        (ObjectNew [ { name = "x", term = NumberLiteral 1 } ])
                        (ObjectNew [ { name = "y", term = NumberLiteral 1 } ])
                    )
                    Dict.empty
                    |> Expect.equal (Err "then and else have different types")
        , test "const で束縛したオブジェクトのプロパティを取得できる" <|
            \_ ->
                Checker.typecheck
                    (Const
                        "o"
                        (ObjectNew [ { name = "x", term = NumberLiteral 1 } ])
                        (ObjectGet (Variable "o") "x")
                    )
                    Dict.empty
                    |> Expect.equal (Ok Number)
        , test "余分なプロパティを持つオブジェクトを引数に渡せる" <|
            \_ ->
                Checker.typecheck
                    (Call
                        (Function [ { name = "r", type_ = objA } ] (ObjectGet (Variable "r") "a"))
                        [ ObjectNew
                            [ { name = "a", term = NumberLiteral 1 }
                            , { name = "b", term = BooleanLiteral True }
                            ]
                        ]
                    )
                    Dict.empty
                    |> Expect.equal (Ok Number)
        , test "ネストしたプロパティが部分型でも渡せる" <|
            \_ ->
                Checker.typecheck
                    (Call
                        (Function
                            [ { name = "r", type_ = Object [ { name = "a", type_ = objA } ] } ]
                            (ObjectGet (ObjectGet (Variable "r") "a") "a")
                        )
                        [ ObjectNew
                            [ { name = "a"
                              , term =
                                    ObjectNew
                                        [ { name = "a", term = NumberLiteral 1 }
                                        , { name = "b", term = BooleanLiteral True }
                                        ]
                              }
                            ]
                        ]
                    )
                    Dict.empty
                    |> Expect.equal (Ok Number)
        , test "必要なプロパティが無ければ拒否する" <|
            \_ ->
                Checker.typecheck
                    (Call
                        (Function [ { name = "r", type_ = objA } ] (ObjectGet (Variable "r") "a"))
                        [ ObjectNew [ { name = "b", term = NumberLiteral 1 } ] ]
                    )
                    Dict.empty
                    |> Expect.equal (Err "parameter type mismatch")
        , test "関数の引数は反変、戻り値は共変" <|
            \_ ->
                Checker.typecheck
                    (Call
                        (Function
                            [ { name = "f", type_ = Func [ { name = "x", type_ = objAB } ] objA } ]
                            (NumberLiteral 0)
                        )
                        [ Function [ { name = "x", type_ = objA } ]
                            (ObjectNew
                                [ { name = "a", term = NumberLiteral 1 }
                                , { name = "b", term = NumberLiteral 2 }
                                ]
                            )
                        ]
                    )
                    Dict.empty
                    |> Expect.equal (Ok Number)
        , test "戻り値が部分型でなければ拒否する" <|
            \_ ->
                Checker.typecheck
                    (Call
                        (Function
                            [ { name = "f", type_ = Func [ { name = "x", type_ = objAB } ] objAB } ]
                            (NumberLiteral 0)
                        )
                        [ Function [ { name = "x", type_ = objA } ]
                            (ObjectNew [ { name = "a", term = NumberLiteral 1 } ])
                        ]
                    )
                    Dict.empty
                    |> Expect.equal (Err "parameter type mismatch")
        , test "recFunc の body は戻り型の部分型でも良い" <|
            \_ ->
                Checker.typecheck
                    (RecFunc "f"
                        [ { name = "n", type_ = Number } ]
                        objA
                        (ObjectNew
                            [ { name = "a", term = NumberLiteral 1 }
                            , { name = "b", term = BooleanLiteral True }
                            ]
                        )
                        (Variable "f")
                    )
                    Dict.empty
                    |> Expect.equal (Ok (Func [ { name = "n", type_ = Number } ] objA))
        ]


objA : Type
objA =
    Object [ { name = "a", type_ = Number } ]


objAB : Type
objAB =
    Object [ { name = "a", type_ = Number }, { name = "b", type_ = Number } ]
