module Tests.Regex exposing (..)

import Expect exposing (Expectation)
import Glob exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Regex"
        [ describe "renders ?" <|
            testSuccesses
                [ Pattern [ Str "a", AnyChar, Str "b" ] => "a.b"
                , Pattern [ Str "a", AnyChar, Str "b", AnyChar, Str "c" ] => "a.b.c"
                , Pattern [ Str "a", AnyChar, AnyChar, Str "c" ] => "a..c"
                , Pattern [ AnyChar, Str "a", AnyChar, Str "c", AnyChar ] => ".a.c."
                ]
        , describe "renders *" <|
            testSuccesses
                [ Pattern [ Str "a", AnyString, Str "b" ] => "a.*b"
                , Pattern [ Str "a", AnyString, Str "b", AnyString, Str "c" ] => "a.*b.*c"
                , Pattern [ Str "a", AnyString, AnyString, Str "c" ] => "a.*.*c"
                , Pattern [ AnyString, Str "a", AnyString, Str "c", AnyString ] => ".*a.*c.*"
                ]
        , describe "renders []" <|
            testSuccesses
                [ Pattern [ CharacterClass [ Ch "a", Ch "b" ] ] => "[ab]"
                , Pattern [ CharacterClass [ Ch "]" ] ] => "[\\]]"
                , Pattern [ CharacterClass [ Ch "]", Ch "a" ] ] => "[\\]a]"
                , Pattern [ CharacterClass [ Ch "!" ] ] => "[!]"
                , Pattern [ CharacterClass [ Ch "?" ] ] => "[\\?]"
                , Pattern [ CharacterClass [ Ch "*" ] ] => "[\\*]"
                ]
        ]


testSuccesses : List ( GlobStructure, String ) -> List Test
testSuccesses list =
    let
        generateTest ( pattern, string ) =
            test string <|
                \() ->
                    Glob.renderRegexString pattern
                        |> Expect.equal string
    in
    List.map generateTest list


(=>) : a -> b -> ( a, b )
(=>) a b =
    ( a, b )
