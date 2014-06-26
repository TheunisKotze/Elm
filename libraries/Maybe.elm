module Maybe ( Maybe(..)
             , extract, isJust, isNothing
             , maybe, andThen
             ) where

{-| Represents an optional value. Maybe it is there, maybe it is not.

# Type and Constructors
@docs Maybe

# Taking Maybes apart
@docs extract, isJust, isNothing

# Command Syntax
The following functions help you use Elm's "Command Syntax", making it easy to
work with a bunch of computations that may fail. 

@docs andThen, maybe

-}

import Basics (not, (.))
import List (foldr, (::))

{-| The Maybe datatype. Useful when a computation may or may not
result in a value (e.g. logarithm is defined only for positive
numbers). 
-}
data Maybe a = Just a | Nothing

{-| Provide a default value and a function to extract the contents of a `Maybe`.
When given `Nothing` you get the default, when given a `Just` you apply the
function to the associated value.

      isPositive : Maybe Int -> Bool
      isPositive maybeInt = extract False (\n -> n > 0) maybeInt

      map : (a -> b) -> Maybe a -> Maybe b
      map f m = extract Nothing (\x -> Just (f x)) m
-}
extract : b -> (a -> b) -> Maybe a -> b
extract b f m =
    case m of
      Just v  -> f v
      Nothing -> b

{-| Check if a maybe happens to be a `Just`.

      isJust (Just 42) == True
      isJust (Just []) == True
      isJust Nothing   == False
-}
isJust : Maybe a -> Bool
isJust = extract False (\_ -> True)

{-| Check if constructed with `Nothing`.

      isNothing (Just 42) == False
      isNothing (Just []) == False
      isNothing Nothing   == True
-}
isNothing : Maybe a -> Bool
isNothing = not . isJust

{-| This value helps us Maybes with "Command Syntax". Think of this as syntactic
sugar for the `andThen` function. It lets us chain together many computations
that may fail:

      toDay   : String -> Maybe Int
      toMonth : String -> Maybe Int
      toYear  : String -> Maybe Int

      toDate : String -> String -> String -> Maybe (Int,Int,Int)
      toDate rawDay rawMonth rawYear = with maybe
          let day   <- toDay   rawDay
              month <- toMonth rawMonth
              year  <- toYear  rawYear
          Just (day, month, year)

If `(toDay rawDay)` succeeds we carry on with the rest of the computations, but
if it fails the whole command block fails and evaluates to `Nothing`. So the
reverse arrow `(<-)` is like syntactic sugar for `andThen` that helps make
things look nicer.
-}
maybe : { andThen : Maybe a -> (a -> Maybe b) -> Maybe b } 
maybe = { andThen = andThen }

{-| This helps us chain together many computations that may fail. It is probably
easiest to understand by seeing its definition:

      andThen : Maybe a -> (a -> Maybe b) -> Maybe b
      andThen maybeValue callback =
          case maybeValue of
            Just value -> callback value
            Nothing    -> Nothing

So we try the first computation, and then we try the next one. If any of them
fail, the whole process fails. For example, lets say the user is typing in
a month that we want to validate:

      toInt : String -> Maybe Int

      toMonth : String -> Maybe Int
      toMonth rawString =
          toInt rawString `andThen` \month ->
              if month < 1 || month > 12 then Nothing else Just month

If `toInt` fails, the whole chain fails! We can also chain lots of `andThen`
together like this:

       toDate : String -> String -> String -> Maybe (Int,Int,Int)
       toDate rawDay rawMonth rawYear =
          toDay   rawDay   `andThen` \day   ->
          toMonth rawMonth `andThen` \month ->
          toYear  rawYear  `andThen` \year  ->
              Just (day, month, year)

That's sort of ugly though, so you can use "Command Syntax" to make this much
prettier!
-}
andThen : Maybe a -> (a -> Maybe b) -> Maybe b
andThen maybeValue callback =
    case maybeValue of
      Just value -> callback value
      Nothing    -> Nothing