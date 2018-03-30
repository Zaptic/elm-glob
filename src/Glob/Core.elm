module Glob.Core exposing (..)

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



-- Parsing


str : Options -> Parser GlobStructure
str options =
    let
        chars char =
            (not options.enableQuestionMark || char /= '?')
                && (not options.enableAsterisk || char /= '*')
                && (not options.enableBrackets || char /= '[')
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


pattern : Options -> Parser GlobStructure
pattern options =
    let
        parsers =
            [ ( str options, True )
            , ( anyChar, options.enableQuestionMark )
            , ( anyString, options.enableAsterisk )
            , ( complementation, options.enableBrackets )
            , ( characterClass, options.enableBrackets )
            ]

        enabledParsers =
            parsers
                |> List.filter Tuple.second
                |> List.map Tuple.first
    in
    inContext "pattern" <|
        map Pattern <|
            repeat oneOrMore
                (oneOf enabledParsers)


parseWithOptions : Options -> String -> Result Parser.Error GlobStructure
parseWithOptions options string =
    Parser.run (pattern options) string



-- Rendering


renderRegexString : GlobStructure -> String
renderRegexString structure =
    case structure of
        Pattern list ->
            List.map renderRegexString list
                |> String.concat
                |> (\string -> "^" ++ string ++ "$")

        Str string ->
            Regex.escape string

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



-- Options


type alias Options =
    { enableAsterisk : Bool
    , enableQuestionMark : Bool
    , enableBrackets : Bool
    }


defaultOptions : Options
defaultOptions =
    { enableAsterisk = True
    , enableQuestionMark = True
    , enableBrackets = True
    }
