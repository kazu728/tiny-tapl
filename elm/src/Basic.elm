module Basic exposing (Term(..), Type(..), show, typecheck)


type Type
    = Boolean
    | Number


type Term
    = BooleanLiteral Bool
    | NumberLiteral Float
    | Conditional Term Term Term
    | Addition Term Term


typecheck : Term -> Result String Type
typecheck term =
    case term of
        BooleanLiteral _ ->
            Ok Boolean

        NumberLiteral _ ->
            Ok Number

        Conditional condition thenBranch elseBranch ->
            expect Boolean condition
                |> Result.andThen
                    (\_ ->
                        Result.map2 Tuple.pair
                            (typecheck thenBranch)
                            (typecheck elseBranch)
                    )
                |> Result.andThen
                    (\( thenType, elseType ) ->
                        if thenType == elseType then
                            Ok thenType

                        else
                            Err "then and else have different types"
                    )

        Addition left right ->
            expect Number left
                |> Result.andThen (\_ -> expect Number right)


expect : Type -> Term -> Result String Type
expect expected term =
    typecheck term
        |> Result.andThen
            (\actual ->
                if actual == expected then
                    Ok actual

                else
                    Err (show expected ++ " expected")
            )


show : Type -> String
show type_ =
    case type_ of
        Boolean ->
            "boolean"

        Number ->
            "number"
