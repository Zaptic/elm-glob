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
        , describe "matches *" <|
            testMatches
                [ ( "a*b", "aaaaaaaaabbbbbbbbbbb" )
                , ( "a*b*c", "aaafffbbbbbbbcccccc" )
                , ( "a**c", "abdefghig2341239+$£%c" )
                , ( "*a*c*", "ac" )
                , ( "*a*c*", "bbbbbbac" )
                , ( "*a*c*", "acdddd" )
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
                , ( "[a-z][A-Z]", "aA" )
                ]
        , describe "does match" <|
            testMatches
                [ ( "a.+", "a.+" )
                , ( "abc", "abc" )
                ]
        , describe "does not match" <|
            testNoMatches
                [ ( "a*b", "aaaaaaaaabc" )
                , ( "a?b", "acbd" )
                , ( "a?b.", "acbd" )
                , ( "a.+", "ab" )
                , ( "a??c", "apqdc" )
                , ( "?a?c?", "dlamcn" )
                , ( "[A-Y]", "Z" )
                , ( "[a-z][A-Z]", "Aa" )
                ]
        , describe "match with options: caseInsensitive False" <|
            testMatchesWithOptions { defaultOptions | caseInsensitive = False }
                [ ( "a.+", "a.+" )
                , ( "abc", "abc" )
                ]
        , describe "match with options: caseInsensitive True" <|
            testMatchesWithOptions { defaultOptions | caseInsensitive = True }
                [ ( "a.+", "A.+" )
                , ( "abc", "AbC" )
                ]
        ]


testMatches : List ( String, String ) -> List Test
testMatches list =
    testMatchesWithOptions defaultOptions list


testMatchesWithOptions : Options -> List ( String, String ) -> List Test
testMatchesWithOptions options list =
    let
        generateTest ( pattern, string ) =
            test (pattern ++ " matches " ++ string) <|
                \() ->
                    Glob.matchWithOptions options pattern string
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
