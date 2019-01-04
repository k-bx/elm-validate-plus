module ValidatePlus exposing
    ( Validator(..)
    , append
    , ifNothing
    , ifNothingOrEmpty
    , map
    , ok
    , succeed
    , validate
    )


type alias FieldError =
    ( String, String )


type Validator subject result
    = Validator (subject -> Result (List FieldError) result)


map : (a -> b) -> Validator subj a -> Validator subj b
map f (Validator v1) =
    Validator
        (\subj ->
            mapOk f (v1 subj)
        )


succeed : (a -> b) -> Validator subj (a -> b)
succeed f =
    Validator
        (\subj -> Ok f)


append : Validator subj a -> Validator subj (a -> b) -> Validator subj b
append (Validator v1) (Validator v2) =
    Validator
        (\subj ->
            case v1 subj of
                Ok a ->
                    case v2 subj of
                        Ok aToB ->
                            Ok (aToB a)

                        Err errs ->
                            Err errs

                Err errs ->
                    Err errs
        )


ok : (subject -> a) -> Validator subject a
ok f =
    Validator (\subj -> Ok (f subj))


ifNothing : (subject -> Maybe a) -> FieldError -> Validator subject a
ifNothing f err =
    Validator
        (\subj ->
            case f subj of
                Nothing ->
                    Err [ err ]

                Just a ->
                    Ok a
        )


ifNothingOrEmpty : (subject -> Maybe String) -> FieldError -> Validator subject String
ifNothingOrEmpty f err =
    Validator
        (\subj ->
            case f subj of
                Nothing ->
                    Err [ err ]

                Just "" ->
                    Err [ err ]

                Just a ->
                    Ok a
        )


mapOk : (b -> c) -> Result a b -> Result a c
mapOk f res =
    case res of
        Err e ->
            Err e

        Ok v ->
            Ok (f v)


validate : Validator subj res -> subj -> Result (List FieldError) res
validate (Validator v) subj =
    v subj
