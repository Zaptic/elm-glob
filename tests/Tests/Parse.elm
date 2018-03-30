module Tests.Parse exposing (..)

import Expect exposing (Expectation)
import Glob.Core exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Parse"
        [ describe "parses ?" <|
            testSuccesses
                [ "a?b" => Pattern [ Str "a", AnyChar, Str "b" ]
                , "a?b?c" => Pattern [ Str "a", AnyChar, Str "b", AnyChar, Str "c" ]
                , "a??c" => Pattern [ Str "a", AnyChar, AnyChar, Str "c" ]
                , "?a?c?" => Pattern [ AnyChar, Str "a", AnyChar, Str "c", AnyChar ]
                ]
        , describe "parses *" <|
            testSuccesses
                [ "a*b" => Pattern [ Str "a", AnyString, Str "b" ]
                , "a*b*c" => Pattern [ Str "a", AnyString, Str "b", AnyString, Str "c" ]
                , "a**c" => Pattern [ Str "a", AnyString, AnyString, Str "c" ]
                , "*a*c*" => Pattern [ AnyString, Str "a", AnyString, Str "c", AnyString ]
                ]
        , describe "parses []" <|
            testSuccesses
                [ "[ab]" => Pattern [ CharacterClass [ Ch "a", Ch "b" ] ]
                , "[]]" => Pattern [ CharacterClass [ Ch "]" ] ]
                , "[]a]" => Pattern [ CharacterClass [ Ch "]", Ch "a" ] ]
                , "[!]" => Pattern [ CharacterClass [ Ch "!" ] ]
                , "[?]" => Pattern [ CharacterClass [ Ch "?" ] ]
                , "[*]" => Pattern [ CharacterClass [ Ch "*" ] ]
                ]
        , describe "parses [!]" <|
            testSuccesses
                [ "[!ab]" => Pattern [ Complementation [ Ch "a", Ch "b" ] ]
                , "[!]]" => Pattern [ Complementation [ Ch "]" ] ]
                , "[!]a]" => Pattern [ Complementation [ Ch "]", Ch "a" ] ]
                , "b[!]a]c" => Pattern [ Str "b", Complementation [ Ch "]", Ch "a" ], Str "c" ]
                ]
        , describe "parses [a-b]" <|
            testSuccesses
                [ "[a-b]" => Pattern [ CharacterClass [ Range "a" "b" ] ]
                , "[a-bc]" => Pattern [ CharacterClass [ Range "a" "b", Ch "c" ] ]
                , "[A-Z0-9]" => Pattern [ CharacterClass [ Range "A" "Z", Range "0" "9" ] ]
                ]
        ]


testSuccesses : List ( String, GlobStructure ) -> List Test
testSuccesses list =
    let
        generateTest ( string, pattern ) =
            test string <|
                \() ->
                    Glob.Core.parseWithOptions defaultOptions string
                        |> Expect.equal (Ok pattern)
    in
    List.map generateTest list


(=>) : a -> b -> ( a, b )
(=>) a b =
    ( a, b )
