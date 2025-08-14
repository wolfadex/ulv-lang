module Extra.List exposing (find)


find : (a -> Maybe b) -> List a -> Maybe b
find predicate list =
    case list of
        [] ->
            Nothing

        next :: rest ->
            case predicate next of
                Just res ->
                    Just res

                Nothing ->
                    find predicate rest
