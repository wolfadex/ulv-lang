module Ulv.Canonical exposing
    ( Definition
    , Error(..)
    , Expression(..)
    , NameSource(..)
    , canonicalize
    )

import Extra.List
import Extra.Result
import Ulv.Common
import Ulv.Parser


type Error
    = Error String


type Expression
    = Exp_Integer Int
    | Exp_Float Float
    | Exp_Boolean Bool
    | Exp_String String
    | Exp_Quote (List Expression)
    | Exp_Name NameSource Ulv.Common.Name
    | Exp_Assign Ulv.Common.Name
    | Exp_Drop
    | Exp_Push NameSource Ulv.Common.Name
    | Exp_InternalInline String
    | Exp_Tag Ulv.Common.Tag


type NameSource
    = BuiltIn
    | LocalSource
    | ExternalSource Ulv.Common.Path


type alias Definition =
    { docComment : Maybe String
    , name : Ulv.Common.Name
    , body : Expression
    }


type alias NamedDefinition =
    { docComment : Maybe String
    , name : Ulv.Common.Name
    }


type DefinitionBuilder
    = NoDefintion
    | BuildingDocComment String
    | BuildingNamed NamedDefinition


canonicalize : Ulv.Common.Path -> Ulv.Parser.Module -> Result (List Error) ( Ulv.Common.Path, List Definition )
canonicalize myPath module_ =
    canonicalFoldDefinition myPath module_.includes module_.body NoDefintion [] []
        |> Result.map (Tuple.pair myPath)


canonicalFoldDefinition : Ulv.Common.Path -> List Ulv.Common.Include -> List Ulv.Parser.Expression -> DefinitionBuilder -> List Error -> List Definition -> Result (List Error) (List Definition)
canonicalFoldDefinition myPath includes expressionsToCanonicalize definitionSoFar errors definitions =
    case definitionSoFar of
        NoDefintion ->
            case expressionsToCanonicalize of
                [] ->
                    case errors of
                        [] ->
                            Ok definitions

                        _ ->
                            Err errors

                (Ulv.Parser.Exp_ForwardAssign defName) :: rest ->
                    canonicalFoldDefinition myPath
                        includes
                        rest
                        (BuildingNamed
                            { docComment = Nothing
                            , name = defName
                            }
                        )
                        errors
                        definitions

                (Ulv.Parser.Exp_DocComment docComment) :: rest ->
                    canonicalFoldDefinition myPath
                        includes
                        rest
                        (BuildingDocComment docComment)
                        errors
                        definitions

                (Ulv.Parser.Exp_Comment _) :: rest ->
                    canonicalFoldDefinition myPath
                        includes
                        rest
                        definitionSoFar
                        errors
                        definitions

                expr ->
                    Debug.todo ("error, unexpected expression: " ++ Debug.toString expr)

        BuildingDocComment docCommentSoFar ->
            case expressionsToCanonicalize of
                [] ->
                    Debug.todo "error, expecting a definition"

                (Ulv.Parser.Exp_ForwardAssign defName) :: rest ->
                    canonicalFoldDefinition myPath
                        includes
                        rest
                        (BuildingNamed
                            { docComment = Just docCommentSoFar
                            , name = defName
                            }
                        )
                        errors
                        definitions

                (Ulv.Parser.Exp_DocComment docComment) :: rest ->
                    canonicalFoldDefinition myPath
                        includes
                        rest
                        (BuildingDocComment (docCommentSoFar ++ "\n" ++ docComment))
                        errors
                        definitions

                (Ulv.Parser.Exp_Comment _) :: rest ->
                    canonicalFoldDefinition myPath
                        includes
                        rest
                        definitionSoFar
                        errors
                        definitions

                _ ->
                    Debug.todo "error, unexpected expression"

        BuildingNamed namedDefinition ->
            let
                namedToDefintion expr =
                    { docComment = namedDefinition.docComment
                    , name = namedDefinition.name
                    , body = expr
                    }

                toPath scoped =
                    Extra.List.find
                        (\( includePath, includeName ) ->
                            if includeName == scoped then
                                Just includePath

                            else
                                Nothing
                        )
                        includes
                        |> Maybe.map Ok
                        |> Maybe.withDefault (Err (Error "Unknown import"))
            in
            case expressionsToCanonicalize of
                [] ->
                    Debug.todo "error, expecting a value for the definition"

                (Ulv.Parser.Exp_ForwardAssign _) :: rest ->
                    Debug.todo "error, 2 sequential definition names"

                (Ulv.Parser.Exp_DocComment docComment) :: rest ->
                    Debug.todo "error, wrong place for a doc comment"

                (Ulv.Parser.Exp_Comment _) :: rest ->
                    canonicalFoldDefinition myPath
                        includes
                        rest
                        definitionSoFar
                        errors
                        definitions

                (Ulv.Parser.Exp_Integer int) :: rest ->
                    canonicalFoldDefinition myPath
                        includes
                        rest
                        NoDefintion
                        errors
                        (namedToDefintion (Exp_Integer int) :: definitions)

                (Ulv.Parser.Exp_Float float) :: rest ->
                    canonicalFoldDefinition myPath includes rest NoDefintion errors (namedToDefintion (Exp_Float float) :: definitions)

                (Ulv.Parser.Exp_Boolean bool) :: rest ->
                    canonicalFoldDefinition myPath includes rest NoDefintion errors (namedToDefintion (Exp_Boolean bool) :: definitions)

                (Ulv.Parser.Exp_String string) :: rest ->
                    canonicalFoldDefinition myPath includes rest NoDefintion errors (namedToDefintion (Exp_String string) :: definitions)

                (Ulv.Parser.Exp_Quote body) :: rest ->
                    case canonicalMapExpressions myPath includes body of
                        Ok bdy ->
                            canonicalFoldDefinition myPath includes rest NoDefintion errors (namedToDefintion (Exp_Quote bdy) :: definitions)

                        Err errs ->
                            canonicalFoldDefinition myPath includes rest NoDefintion (errs ++ errors) definitions

                (Ulv.Parser.Exp_Name nameSource name) :: rest ->
                    case nameSource of
                        Ulv.Parser.BuiltIn ->
                            canonicalFoldDefinition myPath
                                includes
                                rest
                                NoDefintion
                                errors
                                (namedToDefintion (Exp_Name BuiltIn name) :: definitions)

                        Ulv.Parser.LocalSource ->
                            canonicalFoldDefinition myPath
                                includes
                                rest
                                NoDefintion
                                errors
                                (namedToDefintion (Exp_Name LocalSource name) :: definitions)

                        Ulv.Parser.ExternalSource sourceName ->
                            case toPath sourceName of
                                Ok importPath ->
                                    canonicalFoldDefinition myPath
                                        includes
                                        rest
                                        NoDefintion
                                        errors
                                        (namedToDefintion (Exp_Name (ExternalSource importPath) name) :: definitions)

                                Err err ->
                                    canonicalFoldDefinition myPath
                                        includes
                                        rest
                                        NoDefintion
                                        (err :: errors)
                                        definitions

                (Ulv.Parser.Exp_Assign name) :: rest ->
                    canonicalFoldDefinition myPath includes rest NoDefintion errors (namedToDefintion (Exp_Assign name) :: definitions)

                Ulv.Parser.Exp_Drop :: rest ->
                    canonicalFoldDefinition myPath includes rest NoDefintion errors (namedToDefintion Exp_Drop :: definitions)

                (Ulv.Parser.Exp_Push nameSource name) :: rest ->
                    case nameSource of
                        Ulv.Parser.BuiltIn ->
                            canonicalFoldDefinition myPath
                                includes
                                rest
                                NoDefintion
                                errors
                                (namedToDefintion (Exp_Push BuiltIn name) :: definitions)

                        Ulv.Parser.LocalSource ->
                            canonicalFoldDefinition myPath
                                includes
                                rest
                                NoDefintion
                                errors
                                (namedToDefintion (Exp_Push LocalSource name) :: definitions)

                        Ulv.Parser.ExternalSource sourceName ->
                            case toPath sourceName of
                                Ok importPath ->
                                    canonicalFoldDefinition myPath
                                        includes
                                        rest
                                        NoDefintion
                                        errors
                                        (namedToDefintion (Exp_Push (ExternalSource importPath) name) :: definitions)

                                Err err ->
                                    canonicalFoldDefinition myPath
                                        includes
                                        rest
                                        NoDefintion
                                        (err :: errors)
                                        definitions

                (Ulv.Parser.Exp_InternalInline string) :: rest ->
                    canonicalFoldDefinition myPath includes rest NoDefintion errors (namedToDefintion (Exp_InternalInline string) :: definitions)

                (Ulv.Parser.Exp_Tag tag) :: rest ->
                    canonicalFoldDefinition myPath includes rest NoDefintion errors (namedToDefintion (Exp_Tag tag) :: definitions)

                (Ulv.Parser.Exp_FileInclude _) :: rest ->
                    canonicalFoldDefinition myPath includes rest definitionSoFar errors definitions


canonicalMapExpressions : Ulv.Common.Path -> List Ulv.Common.Include -> List Ulv.Parser.Expression -> Result (List Error) (List Expression)
canonicalMapExpressions myPath includes expressions =
    let
        toPath scoped =
            Extra.List.find
                (\( includePath, includeName ) ->
                    if includeName == scoped then
                        Just includePath

                    else
                        Nothing
                )
                includes
                |> Maybe.map Ok
                |> Maybe.withDefault (Err (Error "Unknown import"))
    in
    expressions
        |> List.filterMap
            (\expression ->
                case expression of
                    Ulv.Parser.Exp_ForwardAssign _ ->
                        Debug.todo "error, wrong place for forward assign"

                    Ulv.Parser.Exp_DocComment _ ->
                        Debug.todo "error, wrong place for a doc comment"

                    Ulv.Parser.Exp_Comment _ ->
                        Nothing

                    Ulv.Parser.Exp_Integer int ->
                        Just <| Ok <| Exp_Integer int

                    Ulv.Parser.Exp_Float float ->
                        Just <| Ok <| Exp_Float float

                    Ulv.Parser.Exp_Boolean bool ->
                        Just <| Ok <| Exp_Boolean bool

                    Ulv.Parser.Exp_String string ->
                        Just <| Ok <| Exp_String string

                    Ulv.Parser.Exp_Quote body ->
                        case canonicalMapExpressions myPath includes body of
                            Ok bdy ->
                                Just <| Ok <| Exp_Quote bdy

                            Err errs ->
                                Just <| Err errs

                    Ulv.Parser.Exp_Name nameSource name ->
                        Just <|
                            case nameSource of
                                Ulv.Parser.BuiltIn ->
                                    Ok <| Exp_Name BuiltIn name

                                Ulv.Parser.LocalSource ->
                                    Ok <| Exp_Name LocalSource name

                                Ulv.Parser.ExternalSource sourceName ->
                                    case toPath sourceName of
                                        Ok importPath ->
                                            Ok <| Exp_Name (ExternalSource importPath) name

                                        Err err ->
                                            Err [ err ]

                    Ulv.Parser.Exp_Assign name ->
                        Just <| Ok <| Exp_Assign name

                    Ulv.Parser.Exp_Drop ->
                        Just <| Ok Exp_Drop

                    Ulv.Parser.Exp_Push nameSource name ->
                        Just <|
                            case nameSource of
                                Ulv.Parser.BuiltIn ->
                                    Ok <| Exp_Push BuiltIn name

                                Ulv.Parser.LocalSource ->
                                    Ok <| Exp_Push LocalSource name

                                Ulv.Parser.ExternalSource sourceName ->
                                    case toPath sourceName of
                                        Ok importPath ->
                                            Ok <| Exp_Push (ExternalSource importPath) name

                                        Err err ->
                                            Err [ err ]

                    Ulv.Parser.Exp_InternalInline string ->
                        Just <| Ok <| Exp_InternalInline string

                    Ulv.Parser.Exp_Tag tag ->
                        Just <| Ok <| Exp_Tag tag

                    Ulv.Parser.Exp_FileInclude _ ->
                        Nothing
            )
        |> Extra.Result.fromList
        |> Result.mapError List.concat
