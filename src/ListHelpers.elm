module ListHelpers exposing (..)


firstNonZeroValue : List Float -> Maybe Float
firstNonZeroValue =
    List.head
        >> Maybe.andThen
            (\firstValue ->
                if firstValue == 0 then
                    Nothing
                else
                    Just firstValue
            )
