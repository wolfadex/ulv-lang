module Ulv.Cli exposing (run)

import BackendTask
import BackendTask.File
import Cli.Option
import Cli.OptionsParser
import Cli.Program
import Pages.Script exposing (Script)
import Ulv.Odin
import Ulv.Parser


run : Script
run =
    Pages.Script.withCliOptions program
        (\{ entryFilePath, debugMode } ->
            BackendTask.File.rawFile entryFilePath
                |> BackendTask.allowFatal
                |> BackendTask.andThen
                    (\entryFileContent ->
                        case Ulv.Parser.parse entryFileContent of
                            Err errors ->
                                Pages.Script.log (Debug.toString errors)

                            Ok expressions ->
                                Pages.Script.writeFile
                                    { path = "ulv.odin"
                                    , body = Ulv.Odin.compile debugMode expressions
                                    }
                                    |> BackendTask.allowFatal
                    )
        )


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
