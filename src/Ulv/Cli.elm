module Ulv.Cli exposing (run)

import BackendTask exposing (BackendTask)
import BackendTask.File
import Cli.Option
import Cli.OptionsParser
import Cli.Program
import FatalError exposing (FatalError)
import Pages.Script exposing (Script)
import Ulv.Canonical
import Ulv.Odin
import Ulv.Parser


run : Script
run =
    Pages.Script.withCliOptions program
        (\{ entryFilePath, debugMode } ->
            BackendTask.File.rawFile entryFilePath
                |> BackendTask.allowFatal
                |> BackendTask.andThen
                    (Ulv.Parser.parse debugMode entryFilePath
                        >> Result.mapError (parseErrorsPrettyPrint >> FatalError.fromString)
                        >> BackendTask.fromResult
                    )
                |> BackendTask.andThen
                    (Ulv.Canonical.canonicalize
                        >> Result.mapError (canonicalErrorsPrettyPrint >> FatalError.fromString)
                        >> BackendTask.fromResult
                    )
                |> BackendTask.andThen
                    (\expressions ->
                        Pages.Script.writeFile
                            { path = "ulv.odin"
                            , body = Ulv.Odin.compile debugMode expressions
                            }
                            |> BackendTask.allowFatal
                    )
        )


canonicalErrorsPrettyPrint : List Ulv.Canonical.Error -> String
canonicalErrorsPrettyPrint errors =
    Debug.toString errors


parseErrorsPrettyPrint : ( String, List Ulv.Parser.DeadEnd ) -> String
parseErrorsPrettyPrint ( source, deadEnds ) =
    "-- ERRORS --------------------\n"
        ++ (deadEnds
                |> List.map (parseErrorPrettyPrint source)
                |> String.join "\n------------\n"
           )


parseErrorPrettyPrint : String -> Ulv.Parser.DeadEnd -> String
parseErrorPrettyPrint source deadEnd =
    let
        _ =
            Debug.log "dead end" deadEnd
    in
    -- { col = 1
    -- , contextStack =
    --     [ { col = 1
    --       , context = Ctx_Quote
    --       , row = 2
    --       }
    --     , { col = 1
    --       , context = Ctx_Expression
    --       , row = 2
    --       }
    --     , { col = 1
    --       , context = Ctx_File "examples/core.ulv"
    --       , row = 1
    --       }
    --     ]
    -- , problem = Expect_Symbol "("
    -- , row = 2
    -- }
    let
        filePath =
            deadEnd.contextStack
                |> listFind
                    (\ctx ->
                        case ctx.context of
                            Ulv.Parser.Ctx_File path ->
                                Just (path ++ ":" ++ String.fromInt deadEnd.row ++ ":" ++ String.fromInt deadEnd.col)

                            _ ->
                                Nothing
                    )
                |> Maybe.withDefault ""

        errorLine =
            source
                |> String.lines
                |> List.drop (deadEnd.row - 1)
                |> List.head
                |> Maybe.withDefault ""
    in
    filePath
        ++ "\n"
        ++ (case deadEnd.contextStack of
                [] ->
                    "Unexpected Error!"

                narrowestScope :: _ ->
                    case narrowestScope.context of
                        Ulv.Parser.Ctx_Quote ->
                            case deadEnd.problem of
                                Ulv.Parser.Expect_Symbol symbol ->
                                    String.concat
                                        [ "I was expecting to find the start of a Quote but found:"
                                        , "\n\n"
                                        , String.fromInt deadEnd.row ++ "│ " ++ errorLine
                                        , "\n"
                                        , String.repeat (String.length (String.fromInt deadEnd.row)) " " ++ "│ " ++ String.repeat (deadEnd.col - 1) " " ++ String.repeat (String.length symbol) "^"
                                        , "\n\n"
                                        , "Quotes should be of the form: (words go here)"
                                        ]

                                _ ->
                                    "prob: " ++ Debug.toString deadEnd

                        _ ->
                            "ctx: " ++ Debug.toString narrowestScope
           )


listFind : (a -> Maybe b) -> List a -> Maybe b
listFind predicate list =
    case list of
        [] ->
            Nothing

        next :: rest ->
            case predicate next of
                Just res ->
                    Just res

                Nothing ->
                    listFind predicate rest


type alias CliOptions =
    { entryFilePath : String
    , debugMode : Bool
    }


program : Cli.Program.Config CliOptions
program =
    Cli.Program.config
        |> Cli.Program.add
            (Cli.OptionsParser.build CliOptions
                |> Cli.OptionsParser.with
                    (Cli.Option.requiredPositionalArg "entry file")
                |> Cli.OptionsParser.with
                    (Cli.Option.flag "debug")
            )
