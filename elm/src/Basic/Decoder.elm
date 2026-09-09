module Basic.Decoder exposing (term)

import Basic exposing (Term(..))
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

        _ ->
            Decode.fail ("basic では扱えない項です: " ++ tag)


lazyTerm : Decoder Term
lazyTerm =
    Decode.lazy (\_ -> term)
