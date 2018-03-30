module Tests.Match exposing (..)

import Expect exposing (Expectation)
import Glob exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Match"
        [ describe "matches ?" <|
            testMatches
                [ ( "a?b", "acb" )
                , ( "a?b?c", "azbyc" )
                , ( "a??c", "apqc" )
                , ( "?a?c?", "lamcn" )
                ]
        , describe "does not match ?" <|
            testNoMatches
                [ ( "a?b", "acbd" )
                , ( "a?b.", "acbd" )
                , ( "a?b?c", "abzyc" )
                , ( "a??c", "apqdc" )
                , ( "?a?c?", "dlamcn" )
                ]
        , describe "matches *" <|
            testMatches
                [ ( "a*b", "aaaaaaaaabbbbbbbbbbb" )
                , ( "a*b*c", "aaafffbbbbbbbcccccc" )
                , ( "a**c", "abdefghig2341239+$£%c" )
                , ( "*a*c*", "ac" )
                , ( "*a*c*", "bbbbbbac" )
                , ( "*a*c*", "acdddd" )
                ]
        , describe "does not match *" <|
            testNoMatches
                [ ( "a*b", "aaaaaaaaabc" )
                , ( "a*b", "aaaaaaaaac" )
                ]
        , describe "matches []" <|
            testMatches
                [ ( "[ab]", "a" )
                , ( "[]]", "]" )
                , ( "[]a]", "]" )
                , ( "[]a]", "a" )
                , ( "[!]", "!" )
                , ( "[?]", "?" )
                , ( "[*]", "*" )
                ]
        , describe "matches [!]" <|
            testMatches
                [ ( "[!ab]", "f" )
                , ( "[!]]", "[" )
                , ( "[!]a]", "b" )
                , ( "b[!]a]c", "bdc" )
                ]
        , describe "matches [a-b]" <|
            testMatches
                [ ( "[a-c]", "b" )
                , ( "[a-bc]", "c" )
                , ( "[A-Z]", "F" )
                , ( "[0-9]", "3" )
                , ( "[A-Z0-9]", "Q" )
                ]
        ]


testMatches : List ( String, String ) -> List Test
testMatches list =
    let
        generateTest ( pattern, string ) =
            test (pattern ++ " matches " ++ string) <|
                \() ->
                    Glob.match pattern string
                        |> Expect.true ("Failed to match with regex: " ++ (toString <| Glob.toRegexString pattern))
    in
    List.map generateTest list


testNoMatches : List ( String, String ) -> List Test
testNoMatches list =
    let
        generateTest ( pattern, string ) =
            test (pattern ++ " matches " ++ string) <|
                \() ->
                    Glob.match pattern string
                        |> Expect.false ("Matched incorrectly with regex: " ++ (toString <| Glob.toRegexString pattern))
    in
    List.map generateTest list
