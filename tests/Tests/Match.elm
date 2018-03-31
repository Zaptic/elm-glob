module Tests.Match exposing (..)

import Expect exposing (Expectation)
import Glob
import Glob.Core exposing (..)
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
            testMatchesWithOptions defaultOptions
                False
                [ ( "a.+", "a.+" )
                , ( "abc", "abc" )
                ]
        , describe "match with options: caseInsensitive True" <|
            testMatchesWithOptions defaultOptions
                True
                [ ( "a.+", "A.+" )
                , ( "abc", "AbC" )
                ]
        , describe "matches with options: enableAsterisk False" <|
            testMatchesWithOptions { defaultOptions | enableAsterisk = False }
                False
                [ ( "a*c", "a*c" )
                ]
        , describe "does not match with options: enableAsterisk False" <|
            testNoMatchesWithOptions { defaultOptions | enableAsterisk = False }
                False
                [ ( "a*c", "abc" )
                , ( "a**", "aaabbbb" )
                ]
        , describe "matches with options: enableQuestionMark False" <|
            testMatchesWithOptions { defaultOptions | enableQuestionMark = False }
                False
                [ ( "a*c", "a?c" )
                ]
        , describe "does not match with options: enableQuestionMark False" <|
            testNoMatchesWithOptions { defaultOptions | enableQuestionMark = False }
                False
                [ ( "a?c", "abc" )
                , ( "a??", "abb" )
                ]
        , describe "matches with options: enableBrackets False" <|
            testMatchesWithOptions { defaultOptions | enableBrackets = False }
                False
                [ ( "a[c]", "a[c]" )
                , ( "a[c", "a[c" )
                , ( "a]c", "a]c" )
                , ( "a[*]", "a[something]" )
                ]
        , describe "does not match with options: enableBrackets False" <|
            testNoMatchesWithOptions { defaultOptions | enableBrackets = False }
                False
                [ ( "a[c]", "ac" )
                , ( "a[*]", "ac" )
                ]
        ]


testMatches : List ( String, String ) -> List Test
testMatches list =
    testMatchesWithOptions defaultOptions False list


testMatchesWithOptions : Glob.Core.Options -> Bool -> List ( String, String ) -> List Test
testMatchesWithOptions options caseInsensitive list =
    let
        generateTest ( pattern, string ) =
            test (pattern ++ " matches " ++ string) <|
                \() ->
                    matchWithOptions options caseInsensitive pattern string
                        |> Expect.true ("Failed to match with regex: " ++ (toString <| Glob.toRegexString options pattern))
    in
    List.map generateTest list


testNoMatches : List ( String, String ) -> List Test
testNoMatches list =
    testNoMatchesWithOptions defaultOptions False list


testNoMatchesWithOptions : Options -> Bool -> List ( String, String ) -> List Test
testNoMatchesWithOptions options caseInsensitive list =
    let
        generateTest ( pattern, string ) =
            test (pattern ++ " matches " ++ string) <|
                \() ->
                    matchWithOptions options caseInsensitive pattern string
                        |> Expect.false ("Matched incorrectly with regex: " ++ (toString <| Glob.toRegexString options pattern))
    in
    List.map generateTest list


matchWithOptions : Options -> Bool -> String -> String -> Bool
matchWithOptions options caseInsensitive pattern string =
    Glob.globWithOptions options pattern
        |> Result.map
            (\glob ->
                if caseInsensitive then
                    Glob.caseInsensitive glob
                else
                    glob
            )
        |> Result.map (flip Glob.match string)
        |> Result.withDefault False
