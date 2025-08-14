module Ulv.Parser exposing
    ( Context(..)
    , DeadEnd
    , Expression(..)
    , Module
    , NameSource(..)
    , Problem(..)
    , parse
    )

import Html.Attributes exposing (name)
import Parser.Advanced exposing ((|.), (|=), DeadEnd)
import Parser.Advanced.Workaround
import Ulv.Common


type alias Parser a =
    Parser.Advanced.Parser Context Problem a


type Context
    = Ctx_File String
    | Ctx_Includes
    | Ctx_IncludePath
    | Ctx_IncludeAlias
    | Ctx_Expression
    | Ctx_Integer
    | Ctx_Float
    | Ctx_Boolean
    | Ctx_String
    | Ctx_Quote
    | Ctx_Name
    | Ctx_Assign
    | Ctx_Drop
    | Ctx_ForwardAssign
    | Ctx_Push
    | Ctx_Comment
    | Ctx_DocComment
    | Ctx_InternalInline
    | Ctx_Tag


type alias DeadEnd =
    Parser.Advanced.DeadEnd Context Problem


type Problem
    = TodoInDebugMode
    | Expect_EndOfFile
    | Expect_Token String
    | Expect_Symbol String
    | Expect_Keyword String
    | Expect_Chomp
    | Expect_Integer String
    | Expect_Float String
    | Expect_CodePointLength Int String
    | Expect_CodePointSize String
    | Expect_StringBadQuote
    | Expect_SingleNameForwardAssign


type Expression
    = Exp_Integer Int
    | Exp_Float Float
    | Exp_Boolean Bool
    | Exp_String String
    | Exp_Quote (List Expression)
    | Exp_Name NameSource Ulv.Common.Name
    | Exp_Assign Ulv.Common.Name
    | Exp_Drop
    | Exp_ForwardAssign Ulv.Common.Name
    | Exp_Push NameSource Ulv.Common.Name
    | Exp_Comment String
    | Exp_DocComment String
    | Exp_InternalInline String
    | Exp_Tag Ulv.Common.Tag
    | Exp_FileInclude (List Ulv.Common.Include)


type NameSource
    = BuiltIn
    | LocalSource
    | ExternalSource Ulv.Common.Name


type alias Module =
    { includes : List Ulv.Common.Include
    , body : List Expression
    }


parse : Bool -> String -> String -> Result ( String, List DeadEnd ) Module
parse debugMode filePath source =
    Parser.Advanced.run (parseFile debugMode filePath) source
        |> Result.mapError (Tuple.pair source)


parseFile : Bool -> String -> Parser Module
parseFile debugMode filePath =
    Parser.Advanced.succeed Module
        |. parseSpaces
        |= Parser.Advanced.oneOf
            [ Parser.Advanced.succeed identity
                |. symbol "["
                |. parseSpaces
                |= Parser.Advanced.loop [] (includesHelp debugMode)
                |> Parser.Advanced.inContext Ctx_Includes
            , Parser.Advanced.succeed []
            ]
        |. parseSpaces
        |= Parser.Advanced.loop [] (parseFileHelper debugMode)
        |> Parser.Advanced.inContext (Ctx_File filePath)


includesHelp : Bool -> List Ulv.Common.Include -> Parser (Parser.Advanced.Step (List Ulv.Common.Include) (List Ulv.Common.Include))
includesHelp debugMode reverseIncludes =
    Parser.Advanced.oneOf
        [ Parser.Advanced.succeed (Parser.Advanced.Done (List.reverse reverseIncludes))
            |. symbol "]"
        , Parser.Advanced.succeed
            (\((Ulv.Common.Path p) as path) maybeAlias ->
                Parser.Advanced.Loop <|
                    case maybeAlias of
                        Just alias_ ->
                            ( path, alias_ ) :: reverseIncludes

                        Nothing ->
                            case p |> String.split "/" |> List.reverse of
                                [] ->
                                    Debug.todo "not sure what to do here, yet"

                                defaultName :: _ ->
                                    ( path, Ulv.Common.Name defaultName ) :: reverseIncludes
            )
            |= parsePath
            |. parseSpaces
            |= Parser.Advanced.oneOf
                [ Parser.Advanced.succeed Just
                    |. symbol ":"
                    |= parseNamePart debugMode
                    |> Parser.Advanced.inContext Ctx_IncludeAlias
                , Parser.Advanced.succeed Nothing
                ]
            |. parseSpaces
        ]


parsePath : Parser Ulv.Common.Path
parsePath =
    Parser.Advanced.succeed ()
        |. chompIf (\char -> (char /= ' ') && (char /= '\t') && (char /= '\n') && (char /= '\u{000D}'))
        |. Parser.Advanced.chompWhile (\char -> (char /= ' ') && (char /= '\t') && (char /= '\n') && (char /= '\u{000D}'))
        |> Parser.Advanced.getChompedString
        |> Parser.Advanced.map Ulv.Common.Path
        |> Parser.Advanced.inContext Ctx_IncludePath


parseFileHelper : Bool -> List Expression -> Parser (Parser.Advanced.Step (List Expression) (List Expression))
parseFileHelper debugMode reverseExpressions =
    Parser.Advanced.oneOf
        [ Parser.Advanced.succeed (Parser.Advanced.Done (List.reverse reverseExpressions))
            |. Parser.Advanced.end Expect_EndOfFile
        , Parser.Advanced.succeed (\expression -> Parser.Advanced.Loop (expression :: reverseExpressions))
            |= parseComment
            |. parseSpaces
        , Parser.Advanced.succeed (\expression -> Parser.Advanced.Loop (expression :: reverseExpressions))
            |= parseExpression debugMode
            |. parseSpaces
        ]


parseComment : Parser Expression
parseComment =
    Parser.Advanced.succeed identity
        |. token "#"
        |= Parser.Advanced.oneOf
            [ Parser.Advanced.succeed Exp_InternalInline
                |. token "{"
                |= (chompUntilEndOrBefore "}#"
                        |> Parser.Advanced.getChompedString
                        |> Parser.Advanced.map String.trim
                   )
                |. token "}#"
                |> Parser.Advanced.inContext Ctx_InternalInline
            , Parser.Advanced.succeed Exp_DocComment
                |. token "|"
                |= (chompUntilEndOrAfter "\n"
                        |> Parser.Advanced.getChompedString
                   )
                |> Parser.Advanced.inContext Ctx_DocComment
            , Parser.Advanced.succeed Exp_Comment
                |= (chompUntilEndOrAfter "\n"
                        |> Parser.Advanced.getChompedString
                   )
                |> Parser.Advanced.inContext Ctx_Comment
            ]


parseExpression : Bool -> Parser Expression
parseExpression debugMode =
    Parser.Advanced.oneOf
        [ Parser.Advanced.succeed Exp_Float
            |= parseFloat
        , Parser.Advanced.succeed Exp_Integer
            |= parseInt
        , Parser.Advanced.succeed Exp_String
            |= parseString
        , Parser.Advanced.succeed Exp_Boolean
            |= parseBoolean
            |> Parser.Advanced.backtrackable
        , Parser.Advanced.succeed Exp_Tag
            |= parseTag
        , parseNameLikeValue debugMode
        , Parser.Advanced.succeed Exp_Quote
            |= parseQuote debugMode
        ]
        |> Parser.Advanced.inContext Ctx_Expression


parseQuote : Bool -> Parser (List Expression)
parseQuote debugMode =
    Parser.Advanced.succeed identity
        |. symbol "("
        |. parseSpaces
        |= Parser.Advanced.loop [] (parseQuoteBody debugMode)
        |> Parser.Advanced.inContext Ctx_Quote


parseQuoteBody : Bool -> List Expression -> Parser (Parser.Advanced.Step (List Expression) (List Expression))
parseQuoteBody debugMode reverseExpressions =
    Parser.Advanced.oneOf
        [ Parser.Advanced.succeed (Parser.Advanced.Done (List.reverse reverseExpressions))
            |. symbol ")"
        , Parser.Advanced.succeed (\expression -> Parser.Advanced.Loop (expression :: reverseExpressions))
            |= parseComment
            |. parseSpaces
        , Parser.Advanced.succeed (\expression -> Parser.Advanced.Loop (expression :: reverseExpressions))
            |= Parser.Advanced.lazy (\() -> parseExpression debugMode)
            |. parseSpaces
        ]


parseBoolean : Parser Bool
parseBoolean =
    Parser.Advanced.oneOf
        [ Parser.Advanced.succeed True
            |. keyword "True"
        , Parser.Advanced.succeed False
            |. keyword "False"
        ]
        |> Parser.Advanced.inContext Ctx_Boolean


parseInt : Parser Int
parseInt =
    Parser.Advanced.succeed ()
        |. chompIf Char.isDigit
        |. Parser.Advanced.chompWhile Char.isDigit
        |> Parser.Advanced.getChompedString
        |> Parser.Advanced.andThen
            (\digits ->
                case String.toInt (String.trim digits) of
                    Nothing ->
                        Parser.Advanced.problem (Expect_Integer digits)

                    Just int ->
                        Parser.Advanced.succeed int
            )
        |> Parser.Advanced.inContext Ctx_Integer


parseFloat : Parser Float
parseFloat =
    Parser.Advanced.succeed ()
        |. chompIf Char.isDigit
        |. Parser.Advanced.chompWhile Char.isDigit
        |. chompIf (\char -> char == '.')
        |. chompIf Char.isDigit
        |. Parser.Advanced.chompWhile Char.isDigit
        |> Parser.Advanced.getChompedString
        |> Parser.Advanced.andThen
            (\digits ->
                case String.toFloat digits of
                    Nothing ->
                        Parser.Advanced.problem (Expect_Float digits)

                    Just float ->
                        Parser.Advanced.succeed float
            )
        |> Parser.Advanced.backtrackable
        |> Parser.Advanced.inContext Ctx_Float


builtInNames : List Ulv.Common.Name
builtInNames =
    List.map Ulv.Common.Name
        [ "+"
        , "-"
        , "*"
        , "/"
        , "todo"
        , "="
        , "and"
        , "or"
        , "not"
        , "if"
        , ">"
        , ">="
        , "<"
        , "<="
        , "compare"
        , "print"
        , "stack"
        , "force"
        , "map"
        , "fold"
        , "concat"
        ]


parseNameLikeValue : Bool -> Parser Expression
parseNameLikeValue debugMode =
    Parser.Advanced.oneOf
        [ Parser.Advanced.succeed Exp_Drop
            |. symbol ":_"
            |. Parser.Advanced.oneOf
                [ parseNamePart debugMode
                , Parser.Advanced.succeed (Ulv.Common.Name "")
                ]
            |> Parser.Advanced.inContext Ctx_Drop
        , Parser.Advanced.succeed Exp_Assign
            |. symbol ":"
            |= parseNamePart debugMode
            |> Parser.Advanced.inContext Ctx_Assign
        , Parser.Advanced.succeed
            (\( name, maybeName ) ->
                case maybeName of
                    Nothing ->
                        let
                            nameSource =
                                if List.member name builtInNames then
                                    BuiltIn

                                else
                                    LocalSource
                        in
                        Exp_Push nameSource name

                    Just actualName ->
                        Exp_Push (ExternalSource name) actualName
            )
            |. symbol "^"
            |= parseName debugMode
            |> Parser.Advanced.inContext Ctx_Push
        , Parser.Advanced.succeed Tuple.pair
            |= parseName debugMode
            |= Parser.Advanced.oneOf
                [ Parser.Advanced.succeed True
                    |. symbol ":"
                    |> Parser.Advanced.inContext Ctx_ForwardAssign
                , Parser.Advanced.succeed False
                ]
            |> Parser.Advanced.andThen
                (\( ( name, maybeName ), isForwardAssign ) ->
                    if isForwardAssign then
                        case maybeName of
                            Nothing ->
                                Parser.Advanced.succeed (Exp_ForwardAssign name)

                            Just _ ->
                                Parser.Advanced.problem Expect_SingleNameForwardAssign

                    else
                        Parser.Advanced.succeed <|
                            case maybeName of
                                Nothing ->
                                    let
                                        nameSource =
                                            if List.member name builtInNames then
                                                BuiltIn

                                            else
                                                LocalSource
                                    in
                                    Exp_Name nameSource name

                                Just actualName ->
                                    Exp_Name (ExternalSource name) actualName
                )
            |> Parser.Advanced.inContext Ctx_Name
        ]


parseName : Bool -> Parser ( Ulv.Common.Name, Maybe Ulv.Common.Name )
parseName debugMode =
    Parser.Advanced.succeed Tuple.pair
        |= parseNamePart debugMode
        |= Parser.Advanced.oneOf
            [ Parser.Advanced.succeed Just
                |. token "."
                |= parseNamePart debugMode
            , Parser.Advanced.succeed Nothing
            ]


parseNamePart : Bool -> Parser Ulv.Common.Name
parseNamePart debugMode =
    Parser.Advanced.succeed ()
        |. chompIf
            (\char ->
                (Char.isAlpha char && Char.isLower char)
                    || (char == '=')
                    || (char == '-')
                    || (char == '+')
                    || (char == '*')
                    || (char == '/')
                    || (char == '<')
                    || (char == '>')
            )
        |. Parser.Advanced.chompWhile
            (\char ->
                Char.isAlphaNum char
                    || (char == '_')
                    || (char == '-')
                    || (char == '?')
                    || (char == '!')
            )
        |> Parser.Advanced.getChompedString
        |> Parser.Advanced.andThen
            (\name ->
                if not debugMode && name == "todo" then
                    Parser.Advanced.problem TodoInDebugMode

                else
                    Parser.Advanced.succeed (Ulv.Common.Name name)
            )


parseTag : Parser Ulv.Common.Tag
parseTag =
    Parser.Advanced.succeed ()
        |. chompIf (\char -> char == '@')
        |. Parser.Advanced.chompWhile (\char -> Char.isAlphaNum char || (char == '_') || (char == '-'))
        |> Parser.Advanced.getChompedString
        |> Parser.Advanced.map Ulv.Common.Tag
        |> Parser.Advanced.inContext Ctx_Tag


parseSpace : Parser ()
parseSpace =
    chompIf (\c -> c == ' ' || c == '\n' || c == '\u{000D}' || c == '\t')


parseSpaces : Parser ()
parseSpaces =
    Parser.Advanced.chompWhile (\c -> c == ' ' || c == '\n' || c == '\u{000D}' || c == '\t')



-- STRINGS


parseString : Parser String
parseString =
    Parser.Advanced.oneOf
        [ Parser.Advanced.succeed identity
            |. token "`"
            |= Parser.Advanced.loop [] stringHelp
            |> Parser.Advanced.inContext Ctx_String
        , Parser.Advanced.succeed ()
            |. token "\""
            |> Parser.Advanced.andThen
                (\_ -> Parser.Advanced.problem Expect_StringBadQuote)
        ]


stringHelp : List String -> Parser (Parser.Advanced.Step (List String) String)
stringHelp revChunks =
    Parser.Advanced.oneOf
        [ Parser.Advanced.succeed (\chunk -> Parser.Advanced.Loop (chunk :: revChunks))
            |. token "\\"
            |= Parser.Advanced.oneOf
                [ Parser.Advanced.map (\_ -> "\\\\") (token "\\")
                , Parser.Advanced.map (\_ -> "`") (token "`")
                , Parser.Advanced.map (\_ -> "\\\n") (token "n")
                , Parser.Advanced.map (\_ -> "\\\t") (token "t")
                , Parser.Advanced.map (\_ -> "\\\u{000D}") (token "r")
                , Parser.Advanced.succeed String.fromChar
                    |. token "u{"
                    |= unicode
                    |. token "}"
                ]
        , token "`"
            |> Parser.Advanced.map (\_ -> Parser.Advanced.Done (String.join "" (List.reverse revChunks)))
        , Parser.Advanced.chompWhile isUninteresting
            |> Parser.Advanced.getChompedString
            |> Parser.Advanced.map (\chunk -> Parser.Advanced.Loop (chunk :: revChunks))
        ]


isUninteresting : Char -> Bool
isUninteresting char =
    char /= '\\' && char /= '`'



-- UNICODE


unicode : Parser Char
unicode =
    Parser.Advanced.getChompedString (Parser.Advanced.chompWhile Char.isHexDigit)
        |> Parser.Advanced.andThen codeToChar


codeToChar : String -> Parser Char
codeToChar str =
    let
        length =
            String.length (Debug.log "code to char" str)
                |> Debug.log "char len"

        code =
            String.foldl addHex 0 str
                |> Debug.log "code"
    in
    if 4 < length || length > 6 then
        let
            _ =
                Debug.log "carl" ( str, length, code )
        in
        Parser.Advanced.problem (Expect_CodePointLength length str)

    else if 0 <= code && code <= 0x0010FFFF then
        Parser.Advanced.succeed (Char.fromCode code)

    else
        -- "code point must be between 0 and 0x10FFFF"
        Parser.Advanced.problem (Expect_CodePointSize str)


addHex : Char -> Int -> Int
addHex char total =
    let
        code =
            Char.toCode char
    in
    if 0x30 <= code && code <= 0x39 then
        16 * total + (code - 0x30)

    else if 0x41 <= code && code <= 0x46 then
        16 * total + (10 + code - 0x41)

    else
        16 * total + (10 + code - 0x61)



-- HELPERS


token : String -> Parser ()
token tok =
    Parser.Advanced.token (Parser.Advanced.Token tok (Expect_Token tok))


symbol : String -> Parser ()
symbol tok =
    Parser.Advanced.symbol (Parser.Advanced.Token tok (Expect_Symbol tok))


keyword : String -> Parser ()
keyword tok =
    Parser.Advanced.keyword (Parser.Advanced.Token tok (Expect_Keyword tok))


chompUntilEndOrBefore : String -> Parser ()
chompUntilEndOrBefore tok =
    Parser.Advanced.Workaround.chompUntilEndOrBefore (Parser.Advanced.Token tok (Expect_Token tok))


chompUntilEndOrAfter : String -> Parser ()
chompUntilEndOrAfter tok =
    Parser.Advanced.Workaround.chompUntilEndOrAfter (Parser.Advanced.Token tok (Expect_Token tok))


chompIf : (Char -> Bool) -> Parser ()
chompIf fn =
    Parser.Advanced.chompIf fn Expect_Chomp
