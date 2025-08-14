module Ulv.Cli exposing (run)

import Ansi.Color
import AssocList
import BackendTask exposing (BackendTask)
import BackendTask.File
import Cli.Option
import Cli.OptionsParser
import Cli.Program
import Extra.List
import Extra.Result
import FatalError exposing (FatalError)
import Pages.Script exposing (Script)
import Ulv.Canonical
import Ulv.Common
import Ulv.Odin
import Ulv.Parser


run : Script
run =
    Pages.Script.withCliOptions program
        (\{ entryFilePath, debugMode } ->
            loadFiles debugMode (AssocList.singleton ( entryFilePath, Nothing ) ()) AssocList.empty
                |> BackendTask.andThen
                    (AssocList.toList
                        >> List.map (\( pathStr, module_ ) -> Ulv.Canonical.canonicalize (Ulv.Common.Path pathStr) module_)
                        >> Extra.Result.fromList
                        >> Result.map (List.concatMap (\( path, defs ) -> List.map (Tuple.pair path) defs))
                        >> Result.mapError (List.concat >> canonicalErrorsPrettyPrint >> FatalError.fromString)
                        >> BackendTask.fromResult
                    )
                |> BackendTask.andThen
                    (\modules_ ->
                        Pages.Script.writeFile
                            { path = "ulv.odin"
                            , body =
                                modules_
                                    |> List.reverse
                                    |> Ulv.Odin.compile debugMode
                            }
                            |> BackendTask.allowFatal
                    )
        )


type alias Modules =
    AssocList.Dict String Ulv.Parser.Module


loadFiles : Bool -> AssocList.Dict ( String, Maybe String ) () -> Modules -> BackendTask FatalError Modules
loadFiles debugMode filePaths loadedModules =
    case AssocList.keys filePaths of
        [] ->
            BackendTask.succeed loadedModules

        pathsToLoad ->
            pathsToLoad
                |> List.map
                    (\( filePath, maybeRelativeDir ) ->
                        BackendTask.File.rawFile filePath
                            |> (case maybeRelativeDir of
                                    Nothing ->
                                        identity

                                    Just relativeDir ->
                                        BackendTask.inDir relativeDir
                               )
                            |> BackendTask.allowFatal
                            |> BackendTask.andThen
                                (Ulv.Parser.parse debugMode filePath
                                    >> Result.mapError (parseErrorsPrettyPrint >> FatalError.fromString)
                                    >> Result.map (Tuple.pair filePath)
                                    >> BackendTask.fromResult
                                )
                    )
                |> BackendTask.combine
                |> BackendTask.andThen
                    (List.foldl
                        (\( filePath, module_ ) ( toLoad, mods ) ->
                            let
                                relativePath =
                                    filePath
                                        |> String.split "/"
                                        |> List.reverse
                                        |> List.drop 1
                                        |> List.reverse
                                        |> String.join "/"
                                        |> Just
                            in
                            ( module_.includes
                                |> List.filterMap
                                    (\( Ulv.Common.Path path, _ ) ->
                                        if AssocList.member path mods then
                                            Nothing

                                        else
                                            Just ( ( path ++ ".ulv", relativePath ), () )
                                    )
                                |> AssocList.fromList
                                |> AssocList.union toLoad
                            , AssocList.insert filePath module_ mods
                            )
                        )
                        ( AssocList.empty, loadedModules )
                        >> (\( newPaths, loaded ) -> loadFiles debugMode newPaths loaded)
                    )


canonicalErrorsPrettyPrint : List Ulv.Canonical.Error -> String
canonicalErrorsPrettyPrint errors =
    Debug.toString errors


parseErrorsPrettyPrint : ( String, List Ulv.Parser.DeadEnd ) -> String
parseErrorsPrettyPrint ( source, deadEnds ) =
    Ansi.Color.fontColor Ansi.Color.red "-- ERRORS --------------------\n"
        ++ (deadEnds
                |> List.map (parseErrorPrettyPrint source)
                |> String.join "\n------------\n"
           )


parseErrorPrettyPrint : String -> Ulv.Parser.DeadEnd -> String
parseErrorPrettyPrint source deadEnd =
    let
        filePath =
            deadEnd.contextStack
                |> Extra.List.find
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

        -- _ =
        --     Debug.log "dead end" deadEnd
    in
    filePath
        ++ "\n"
        ++ (case deadEnd.contextStack of
                [] ->
                    "Unexpected Error!"

                narrowestScope :: _ ->
                    case deadEnd.problem of
                        Ulv.Parser.Expect_Symbol symbol ->
                            String.concat
                                [ "I was expecting to find the start of a quote but found:"
                                , "\n\n"
                                , String.fromInt deadEnd.row ++ "│ " ++ errorLine
                                , "\n"
                                , String.repeat (String.length (String.fromInt deadEnd.row)) " " ++ "│ " ++ String.repeat (deadEnd.col - 1) " " ++ errorPointers (String.length symbol)
                                , "\n\n"
                                , "Quotes should be of the form: (words go here)"
                                ]

                        Ulv.Parser.Expect_StringBadQuote ->
                            String.concat
                                [ "I was expecting to find the start of a string but found:"
                                , "\n\n"
                                , String.fromInt deadEnd.row ++ "│ " ++ errorLine
                                , "\n"
                                , String.repeat (String.length (String.fromInt deadEnd.row)) " " ++ "│ " ++ errorPointers (deadEnd.col - 1)
                                , "\n\n"
                                , "In Ulv, strings start and end with `, try replacing your \" with ` and runnign again."
                                ]

                        _ ->
                            "prob: " ++ Debug.toString deadEnd
            --     case narrowestScope.context of
            --         Ulv.Parser.Ctx_Quote ->
            -- _ ->
            --     "ctx: " ++ Debug.toString narrowestScope
           )


errorPointers : Int -> String
errorPointers length =
    Ansi.Color.fontColor Ansi.Color.red (String.repeat length "^")


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
