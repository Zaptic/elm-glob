module Tests.Match exposing (..)

import Expect exposing (Expectation)
import Glob exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Match"
        [ describe "matches ?" <|
            testSuccesses
                [ ( "a?b", "acb" )
                , ( "a?b?c", "azbyc" )
                , ( "a??c", "apqc" )
                , ( "?a?c?", "lamcn" )
                ]
        , describe "matches *" <|
            testSuccesses
                [ ( "a*b", "aaaaaaaaabbbbbbbbbbb" )
                , ( "a*b*c", "aaafffbbbbbbbcccccc" )
                , ( "a**c", "abdefghig2341239+$£%c" )
                , ( "*a*c*", "ac" )
                , ( "*a*c*", "bbbbbbac" )
                , ( "*a*c*", "acdddd" )
                ]
        , describe "matches []" <|
            testSuccesses
                [ ( "[ab]", "a" )
                , ( "[]]", "]" )
                , ( "[]a]", "]" )
                , ( "[]a]", "a" )
                , ( "[!]", "!" )
                , ( "[?]", "?" )
                , ( "[*]", "*" )
                ]
        , describe "matches [!]" <|
            testSuccesses
                [ ( "[!ab]", "f" )
                , ( "[!]]", "[" )
                , ( "[!]a]", "b" )
                , ( "b[!]a]c", "bdc" )
                ]
        , describe "matches [a-b]" <|
            testSuccesses
                [ ( "[a-c]", "b" )
                , ( "[a-bc]", "c" )
                , ( "[A-Z]", "F" )
                , ( "[0-9]", "3" )
                , ( "[A-Z0-9]", "Q" )
                ]
        ]


testSuccesses : List ( String, String ) -> List Test
testSuccesses list =
    let
        generateTest ( pattern, string ) =
            test (pattern ++ " matches " ++ string) <|
                \() ->
                    Glob.match pattern string
                        |> Expect.true ("Failed with regex: " ++ (toString <| Glob.toRegexString pattern))
    in
    List.map generateTest list
