module Ulv.Parser exposing
    ( Command(..)
    , Error(..)
    , Expression(..)
    , Name(..)
    , Tag(..)
    , parse
    )

import Parser exposing ((|.), (|=), Parser)
import Parser.Workaround


type Error
    = Error String


type Expression
    = Exp_Integer Int
    | Exp_Float Float
    | Exp_Boolean Bool
    | Exp_String String
    | Exp_Quote (List Expression)
    | Exp_Name (Maybe Command) Name
      -- Experimental
    | Exp_Tag Tag


type Name
    = Name String


type Tag
    = Tag String


type Command
    = Assign
    | Push


parse : String -> Result (List Error) (List Expression)
parse source =
    Parser.run parseFile source
        |> Result.mapError (\err -> List.map (Debug.toString >> Error) err)


parseFile : Parser (List Expression)
parseFile =
    Parser.succeed identity
        |. parseSpaces
        |= Parser.loop [] parseFileHelper


parseFileHelper : List Expression -> Parser (Parser.Step (List Expression) (List Expression))
parseFileHelper reverseExpressions =
    Parser.oneOf
        [ Parser.succeed (Parser.Done (List.reverse reverseExpressions))
            |. Parser.end
        , Parser.succeed (Parser.Loop reverseExpressions)
            |. parseLineComment
            |. parseSpaces
        , Parser.succeed (\expression -> Parser.Loop (expression :: reverseExpressions))
            |= parseExpression
            |. parseSpaces
        ]


parseLineComment : Parser ()
parseLineComment =
    Parser.Workaround.lineCommentAfter "#"


parseExpression : Parser Expression
parseExpression =
    Parser.oneOf
        [ Parser.succeed Exp_Float
            |= parseFloat
        , Parser.succeed Exp_Integer
            |= parseInt
        , Parser.succeed Exp_String
            |= parseString
        , Parser.succeed Exp_Boolean
            |= parseBoolean
            |> Parser.backtrackable
        , Parser.succeed Exp_Tag
            |= parseTag
        , parseName
        , Parser.succeed Exp_Quote
            |= parseQuote
        ]


parseQuote : Parser (List Expression)
parseQuote =
    Parser.succeed identity
        |. Parser.symbol "("
        |= Parser.loop [] parseQuoteBody


parseQuoteBody : List Expression -> Parser (Parser.Step (List Expression) (List Expression))
parseQuoteBody reverseExpressions =
    Parser.oneOf
        [ Parser.succeed (Parser.Done (List.reverse reverseExpressions))
            |. Parser.symbol ")"
        , Parser.succeed (Parser.Loop reverseExpressions)
            |. parseLineComment
            |. parseSpaces
        , Parser.succeed (\expression -> Parser.Loop (expression :: reverseExpressions))
            |= Parser.lazy (\() -> parseExpression)
            |. parseSpaces
        ]


parseBoolean : Parser Bool
parseBoolean =
    Parser.oneOf
        [ Parser.succeed True
            |. Parser.keyword "True"
        , Parser.succeed False
            |. Parser.keyword "False"
        ]


parseInt : Parser Int
parseInt =
    Parser.succeed ()
        |. Parser.chompIf Char.isDigit
        |. Parser.chompWhile Char.isDigit
        |> Parser.getChompedString
        |> Parser.andThen
            (\digits ->
                case String.toInt (String.trim digits) of
                    Nothing ->
                        Parser.problem ("Expected an Integer but found " ++ digits)

                    Just int ->
                        Parser.succeed int
            )


parseFloat : Parser Float
parseFloat =
    Parser.succeed ()
        |. Parser.chompIf Char.isDigit
        |. Parser.chompWhile Char.isDigit
        |. Parser.chompIf (\char -> char == '.')
        |. Parser.chompIf Char.isDigit
        |. Parser.chompWhile Char.isDigit
        |> Parser.getChompedString
        |> Parser.andThen
            (\digits ->
                case String.toFloat digits of
                    Nothing ->
                        Parser.problem ("Expected an Float but found " ++ digits)

                    Just float ->
                        Parser.succeed float
            )
        |> Parser.backtrackable


parseName : Parser Expression
parseName =
    Parser.succeed Exp_Name
        |= Parser.oneOf
            [ Parser.succeed Just
                |= parseCommand
            , Parser.succeed Nothing
            ]
        |= parseNameHelper


parseCommand : Parser Command
parseCommand =
    Parser.oneOf
        [ Parser.succeed Assign
            |. Parser.symbol ":"
        , Parser.succeed Push
            |. Parser.symbol "^"
        ]


parseNameHelper : Parser Name
parseNameHelper =
    Parser.succeed ()
        |. Parser.chompIf
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
        |. Parser.chompWhile
            (\char ->
                Char.isAlphaNum char
                    || (char == '_')
                    || (char == '-')
                    || (char == '?')
                    || (char == '!')
            )
        |> Parser.getChompedString
        |> Parser.map Name


parseTag : Parser Tag
parseTag =
    Parser.succeed ()
        |. Parser.chompIf (\char -> char == '@')
        |. Parser.chompWhile (\char -> Char.isAlphaNum char || (char == '_') || (char == '-'))
        |> Parser.getChompedString
        |> Parser.map Tag


parseSpace : Parser ()
parseSpace =
    Parser.chompIf (\c -> c == ' ' || c == '\n' || c == '\u{000D}' || c == '\t')


parseSpaces : Parser ()
parseSpaces =
    Parser.chompWhile (\c -> c == ' ' || c == '\n' || c == '\u{000D}' || c == '\t')



-- STRINGS


parseString : Parser String
parseString =
    Parser.succeed identity
        |. Parser.token "`"
        |= Parser.loop [] stringHelp


stringHelp : List String -> Parser (Parser.Step (List String) String)
stringHelp revChunks =
    Parser.oneOf
        [ Parser.succeed (\chunk -> Parser.Loop (chunk :: revChunks))
            |. Parser.token "\\"
            |= Parser.oneOf
                [ Parser.map (\_ -> "\\\\") (Parser.token "\\")
                , Parser.map (\_ -> "`") (Parser.token "`")
                , Parser.map (\_ -> "\\\n") (Parser.token "n")
                , Parser.map (\_ -> "\\\t") (Parser.token "t")
                , Parser.map (\_ -> "\\\u{000D}") (Parser.token "r")
                , Parser.succeed String.fromChar
                    |. Parser.token "u{"
                    |= unicode
                    |. Parser.token "}"
                ]
        , Parser.token "`"
            |> Parser.map (\_ -> Parser.Done (String.join "" (List.reverse revChunks)))
        , Parser.chompWhile isUninteresting
            |> Parser.getChompedString
            |> Parser.map (\chunk -> Parser.Loop (chunk :: revChunks))
        ]


isUninteresting : Char -> Bool
isUninteresting char =
    char /= '\\' && char /= '`'



-- UNICODE


unicode : Parser Char
unicode =
    Parser.getChompedString (Parser.chompWhile Char.isHexDigit)
        |> Parser.andThen codeToChar


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
        Parser.problem "code point must have between 4 and 6 digits"

    else if 0 <= code && code <= 0x0010FFFF then
        Parser.succeed (Char.fromCode code)

    else
        Parser.problem "code point must be between 0 and 0x10FFFF"


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
