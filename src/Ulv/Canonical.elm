module Ulv.Canonical exposing
    ( Error(..)
    , Expression(..)
    , canonicalize
    )

import Ulv.Parser


type Error
    = Error String


type Expression
    = Exp_Integer Int
    | Exp_Float Float
    | Exp_Boolean Bool
    | Exp_String String
    | Exp_Quote (List Expression)
    | Exp_Name Ulv.Parser.Name
    | Exp_Assign Ulv.Parser.Name
    | Exp_Drop
    | Exp_Push Ulv.Parser.Name
    | Exp_Comment String
    | Exp_InternalInline String
    | Exp_Tag Ulv.Parser.Tag


canonicalize : List Ulv.Parser.Expression -> Result (List Error) (List Expression)
canonicalize expressions =
    canonicalFold expressions Nothing [] []


canonicalFold : List Ulv.Parser.Expression -> Maybe Expression -> List Error -> List Expression -> Result (List Error) (List Expression)
canonicalFold expressionsToCanonicalize forwardAssign errors expressions =
    case forwardAssign of
        Nothing ->
            case expressionsToCanonicalize of
                [] ->
                    case errors of
                        [] ->
                            Ok (List.reverse expressions)

                        _ ->
                            Err errors

                (Ulv.Parser.Exp_Integer int) :: rest ->
                    canonicalFold rest Nothing errors (Exp_Integer int :: expressions)

                (Ulv.Parser.Exp_Float float) :: rest ->
                    canonicalFold rest Nothing errors (Exp_Float float :: expressions)

                (Ulv.Parser.Exp_Boolean bool) :: rest ->
                    canonicalFold rest Nothing errors (Exp_Boolean bool :: expressions)

                (Ulv.Parser.Exp_String string) :: rest ->
                    canonicalFold rest Nothing errors (Exp_String string :: expressions)

                (Ulv.Parser.Exp_Quote body) :: rest ->
                    case canonicalFold body Nothing [] [] of
                        Ok bdy ->
                            canonicalFold rest Nothing errors (Exp_Quote bdy :: expressions)

                        Err errs ->
                            canonicalFold rest Nothing (errs ++ errors) expressions

                (Ulv.Parser.Exp_Name name) :: rest ->
                    canonicalFold rest Nothing errors (Exp_Name name :: expressions)

                (Ulv.Parser.Exp_Assign name) :: rest ->
                    canonicalFold rest Nothing errors (Exp_Assign name :: expressions)

                (Ulv.Parser.Exp_ForwardAssign name) :: rest ->
                    canonicalFold rest (Just (Exp_Assign name)) errors expressions

                Ulv.Parser.Exp_Drop :: rest ->
                    canonicalFold rest Nothing errors (Exp_Drop :: expressions)

                (Ulv.Parser.Exp_Push name) :: rest ->
                    canonicalFold rest Nothing errors (Exp_Push name :: expressions)

                (Ulv.Parser.Exp_Comment string) :: rest ->
                    canonicalFold rest Nothing errors (Exp_Comment string :: expressions)

                (Ulv.Parser.Exp_InternalInline string) :: rest ->
                    canonicalFold rest Nothing errors (Exp_InternalInline string :: expressions)

                (Ulv.Parser.Exp_Tag tag) :: rest ->
                    canonicalFold rest Nothing errors (Exp_Tag tag :: expressions)

        Just assign ->
            case expressionsToCanonicalize of
                [] ->
                    Err (Error "Cannot end with a foward assign" :: errors)

                (Ulv.Parser.Exp_Integer int) :: rest ->
                    canonicalFold rest Nothing errors (assign :: Exp_Integer int :: expressions)

                (Ulv.Parser.Exp_Float float) :: rest ->
                    canonicalFold rest Nothing errors (assign :: Exp_Float float :: expressions)

                (Ulv.Parser.Exp_Boolean bool) :: rest ->
                    canonicalFold rest Nothing errors (assign :: Exp_Boolean bool :: expressions)

                (Ulv.Parser.Exp_String string) :: rest ->
                    canonicalFold rest Nothing errors (assign :: Exp_String string :: expressions)

                (Ulv.Parser.Exp_Quote body) :: rest ->
                    case canonicalFold body Nothing [] [] of
                        Ok bdy ->
                            canonicalFold rest Nothing errors (assign :: Exp_Quote bdy :: expressions)

                        Err errs ->
                            canonicalFold rest Nothing (errs ++ errors) expressions

                (Ulv.Parser.Exp_Name name) :: rest ->
                    canonicalFold rest Nothing errors (assign :: Exp_Name name :: expressions)

                (Ulv.Parser.Exp_Assign name) :: rest ->
                    canonicalFold rest Nothing errors (assign :: Exp_Assign name :: expressions)

                (Ulv.Parser.Exp_ForwardAssign name) :: rest ->
                    canonicalFold rest Nothing (Error "2 sequential forward assignments is unallowed" :: errors) expressions

                Ulv.Parser.Exp_Drop :: rest ->
                    canonicalFold rest Nothing errors (Exp_Drop :: expressions)

                (Ulv.Parser.Exp_Push name) :: rest ->
                    canonicalFold rest Nothing errors (assign :: Exp_Push name :: expressions)

                (Ulv.Parser.Exp_Comment string) :: rest ->
                    canonicalFold rest Nothing errors (assign :: Exp_Comment string :: expressions)

                (Ulv.Parser.Exp_InternalInline string) :: rest ->
                    canonicalFold rest Nothing errors (assign :: Exp_InternalInline string :: expressions)

                (Ulv.Parser.Exp_Tag tag) :: rest ->
                    canonicalFold rest Nothing errors (assign :: Exp_Tag tag :: expressions)
