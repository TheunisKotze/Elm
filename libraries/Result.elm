module Result where
{-| Represents the result of a computation that may fail. 

# Type and Constructors
@docs Result

-}

import Maybe ( Maybe(..) )

{-| A `Result` is either `Ok` meaning the computation went through alright, or
it is an `Err` meaning that there was some failure.
-}
data Result error value = Ok value | Err error

{-| Apply a function to a result. If the result is `Ok`, it will be converted.
If the result is an `Err`, the same error value will propegate through.

      map sqrt (Ok 4.0)          == Ok 2.0
      map sqrt (Err "bad input") == Err "bad input"
-}
map : (a -> b) -> Result error a -> Result error b
map f result =
    case result of
      Ok  v -> Ok (f v)
      Err e -> Err e

formatError : (error -> error') -> Result error a -> Result error' a
formatError f result =
    case result of
      Ok  v -> Ok v
      Err e -> Err (f e)

toMaybe : Result e a -> Maybe a
toMaybe result =
    case result of
      Ok  v -> Just v
      Err _ -> Nothing

fromMaybe : e -> Maybe a -> Result e a
fromMaybe err maybe =
    case maybe of
      Just v  -> Ok v
      Nothing -> Err err

{-| Takes a result and a callback. If the result is `Ok` we run the callback
with the value. If the result is an `Err` we just return the error. This helps
you chain together many computations which may fail.

      safeSqrt : Float -> Result String Float
      safeSqrt n =
          if n >= 0
          then Ok (sqrt n)
          else Err "imaginary number"

      (Ok 4.0          `andThen` safeSqrt) == Ok 2.0
      (Err "bad input" `andThen` safeSqrt) == Err "bad input"
      (Ok -4.0         `andThen` safeSqrt) == Err "imaginary number"

-}
andThen : Result error a -> (a -> Result error b) -> Result error b
andThen result callback =
    case result of
      Ok  v -> callback v
      Err e -> Err e

result : { andThen : Result error a -> (a -> Result error b) -> Result error b }
result = { andThen = andThen }