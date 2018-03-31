module Glob exposing (Error, Glob, Options, caseInsensitive, defaultOptions, glob, globWithOptions, match, toRegexString)

{-|

@docs Glob, Error, glob, globWithOptions, caseInsensitive, Options, defaultOptions, match, toRegexString

-}

import Glob.Core exposing (..)
import Parser
import Regex


{-| -}
type Glob
    = Glob Regex.Regex


{-| -}
type Error
    = ParserError Parser.Error


{-| -}
glob : String -> Result Error Glob
glob string =
    globWithOptions defaultOptions string


{-| -}
globWithOptions : Options -> String -> Result Error Glob
globWithOptions options string =
    parseWithOptions options string
        |> Result.mapError ParserError
        |> Result.map (renderRegexString >> Regex.regex >> Glob)


{-| -}
caseInsensitive : Glob -> Glob
caseInsensitive (Glob regex) =
    Regex.caseInsensitive regex
        |> Glob



-- Match


{-| -}
match : Glob -> String -> Bool
match (Glob regex) string =
    Regex.contains regex string



-- Convert


{-| -}
toRegexString : Options -> String -> Result Error String
toRegexString options pattern =
    parseWithOptions options pattern
        |> Result.mapError ParserError
        |> Result.map renderRegexString


{-| -}
type alias Options =
    { enableAsterisk : Bool
    , enableQuestionMark : Bool
    , enableBrackets : Bool
    }


{-| -}
defaultOptions : Options
defaultOptions =
    { enableAsterisk = True
    , enableQuestionMark = True
    , enableBrackets = True
    }
