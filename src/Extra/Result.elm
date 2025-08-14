module Extra.Result exposing (fromList)


fromList : List (Result e a) -> Result (List e) (List a)
fromList =
    List.foldr
        (\res ( errs, oks ) ->
            case res of
                Ok a ->
                    ( errs, a :: oks )

                Err e ->
                    ( e :: errs, oks )
        )
        ( [], [] )
        >> (\( errs, oks ) ->
                case errs of
                    [] ->
                        Ok oks

                    _ ->
                        Err errs
           )
