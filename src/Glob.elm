module Glob exposing (..)

import Parser exposing (..)
import Regex


type GlobStructure
    = Pattern (List GlobStructure)
    | Str String
    | AnyChar
    | AnyString
    | CharacterClass (List ClassMember)
    | Complementation (List ClassMember)


type ClassMember
    = Ch String
    | Range String String


str : Parser GlobStructure
str =
    let
        chars char =
            char /= '?' && char /= '*' && char /= '['
    in
    succeed Str
        |= keep oneOrMore chars


anyChar : Parser GlobStructure
anyChar =
    succeed AnyChar
        |. keyword "?"


anyString : Parser GlobStructure
anyString =
    succeed AnyString
        |. keyword "*"


characterClassMembers : Parser (List ClassMember)
characterClassMembers =
    let
        notEndBracket =
            keep (Exactly 1) (\c -> c /= ']')
                |> Parser.map Ch

        letters =
            String.toList "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

        numbers =
            String.toList "0123456789"

        validRangeCharacter char =
            List.member char letters || List.member char numbers

        range =
            delayedCommitMap
                (\a b -> Range a b)
                (succeed identity
                    |= keep (Exactly 1) validRangeCharacter
                    |. symbol "-"
                )
                (keep (Exactly 1) validRangeCharacter)
    in
    oneOf
        [ succeed (\c cs -> Ch c :: cs)
            |= keep (Exactly 1) (\c -> c == ']')
            |= repeat zeroOrMore (oneOf [ range, notEndBracket ])
        , succeed identity
            |= repeat zeroOrMore (oneOf [ range, notEndBracket ])
        ]


characterClass : Parser GlobStructure
characterClass =
    delayedCommitMap
        (\a b -> a)
        (succeed CharacterClass
            |. symbol "["
            |= characterClassMembers
        )
        (symbol "]")


complementation : Parser GlobStructure
complementation =
    delayedCommitMap
        (\a b -> a)
        (succeed Complementation
            |. symbol "["
            |. symbol "!"
            |= characterClassMembers
        )
        (symbol "]")


pattern : Parser GlobStructure
pattern =
    inContext "pattern" <|
        map Pattern <|
            repeat oneOrMore
                (oneOf
                    [ str
                    , anyChar
                    , anyString
                    , complementation
                    , characterClass
                    ]
                )


parse : String -> Result Parser.Error GlobStructure
parse string =
    Parser.run pattern string



-- Rendering


renderRegexString : GlobStructure -> String
renderRegexString structure =
    case structure of
        Pattern list ->
            String.concat <| List.map renderRegexString list

        Str string ->
            string

        AnyChar ->
            "."

        AnyString ->
            ".*"

        CharacterClass list ->
            list
                |> List.map renderRegexClassMembers
                |> String.concat
                |> (\string -> "[" ++ string ++ "]")

        Complementation list ->
            list
                |> List.map renderRegexClassMembers
                |> String.concat
                |> (\string -> "[^" ++ string ++ "]")


renderRegexClassMembers : ClassMember -> String
renderRegexClassMembers member =
    case member of
        Ch char ->
            Regex.escape char

        Range from to ->
            from ++ "-" ++ to



-- Matching


match : String -> String -> Bool
match pattern string =
    parse pattern
        |> Result.map renderRegexString
        |> Result.map Regex.regex
        |> Result.map (\regex -> Regex.contains regex string)
        |> Result.withDefault False



-- Convert


toRegexString : String -> Result Error String
toRegexString pattern =
    parse pattern
        |> Result.map renderRegexString
