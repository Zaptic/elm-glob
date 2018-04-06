module Glob exposing (Error, Glob, Options, caseInsensitive, defaultOptions, glob, globWithOptions, match, toRegexString)

{-| This module allows you to create patterns for matching strings based on standard glob or fn-match rules.

@docs Glob, Error, glob, globWithOptions, caseInsensitive, Options, defaultOptions, match, toRegexString

-}

import Glob.Core exposing (..)
import Parser
import Regex


{-| A glob with a particular pattern -}
type Glob
    = Glob Regex.Regex


{-| This type holds the potential errors that might come from trying to create a glob from a string. -}
type Error
    = ParserError Parser.Error


{-| Creates a glob from a string with the default options. This can fail as some patterns are invalid globs. -}
glob : String -> Result Error Glob
glob string =
    globWithOptions defaultOptions string


{-| Creates a Glob from a string allowing the caller to provide the options they desire. This can fail. -}
globWithOptions : Options -> String -> Result Error Glob
globWithOptions options string =
    parseWithOptions options string
        |> Result.mapError ParserError
        |> Result.map (renderRegexString >> Regex.regex >> Glob)


{-| Converts a Glob into a case insensitive version of itself. It applies Regex.caseInsensitive to the underlying Regex. -}
caseInsensitive : Glob -> Glob
caseInsensitive (Glob regex) =
    Regex.caseInsensitive regex
        |> Glob



-- Match


{-| Returns true if the string matches the Glob. -}
match : Glob -> String -> Bool
match (Glob regex) string =
    Regex.contains regex string



-- Convert


{-| Generates the regular expression syntax from the given string. -}
toRegexString : Options -> String -> Result Error String
toRegexString options pattern =
    parseWithOptions options pattern
        |> Result.mapError ParserError
        |> Result.map renderRegexString


{-| This type describes the options available when generating the Glob. The different entries allow you to disable parts of the Glob syntax. -}
type alias Options =
    { enableAsterisk : Bool
    , enableQuestionMark : Bool
    , enableBrackets : Bool
    }


{-| Default options value that has all options set to True. Useful if you want to only disable one option whilst keeping the rest as standard. -}
defaultOptions : Options
defaultOptions =
    { enableAsterisk = True
    , enableQuestionMark = True
    , enableBrackets = True
    }
