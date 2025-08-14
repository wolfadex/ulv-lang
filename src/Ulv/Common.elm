module Ulv.Common exposing
    ( Include
    , Name(..)
    , Path(..)
    , Tag(..)
    )


type Path
    = Path String


type Name
    = Name String


type Tag
    = Tag String


type alias Include =
    ( Path, Name )
