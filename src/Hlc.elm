module Hlc exposing
    ( Hlc, LocalHlc, RemoteHlc
    , create
    , local, remote
    , compare
    , toString, fromString
    , values
    )

{-|

@docs Hlc, LocalHlc, RemoteHlc

@docs create

@docs local, remote

@docs compare

@docs toString, fromString

@docs values

-}

import Time exposing (Posix, millisToPosix, posixToMillis)


{-| A hybrid logical clock.

The constructor for this type is hidden. You can create a new clock with the
[`create`](#create)
function in this module.

-}
type Hlc
    = Hlc Posix Int String


{-| An alias for an `Hlc` representing the local clock.

When used in this module, it's used to denote the difference between the local clock
and a remote clock when those differences affect the outcome of a function.

-}
type alias LocalHlc =
    Hlc


{-| An alias for an `Hlc` representing a remote clock.

When used in this module, it's used to denote the difference between the local clock
and a remote clock when those differences affect the outcome of a function.

-}
type alias RemoteHlc =
    Hlc


{-| Create a new hybrid logical-clock. To do so, you need a few pieces:

  - A `String` representing a unique id for this clock. In the case one clock is the
    same as another clock, this id will be used as a tiebreaker to determine which clock is greater.
  - The current time at the creation of a clock as `Posix` time. You will need to use the
    [`elm/time`](https://package.elm-lang.org/packages/elm/time/latest/) library to get
    this value.

-}
create : String -> Posix -> Hlc
create id now =
    Hlc now 0 id


{-| When an action takes place locally and you want to increment the clock, use this
function.

This will return a new clock that is guaranteed to result in a logical order of events
made on this machine, regardless of how accurate the clock on the machine is.

-}
local : String -> Posix -> LocalHlc -> Hlc
local id now (Hlc clock count _) =
    {- Implementation directly from the paper. This uses `max` and `=` to determine
       what to return instead of greater than comparison. Keeping with this for now
       given it's true to the paper's alogorithm.
    -}
    let
        clockMillis : Int
        clockMillis =
            posixToMillis clock

        maxClock : Int
        maxClock =
            {- Determine the max between the current time and the time of the passed
               clock.
            -}
            max clockMillis (posixToMillis now)

        newCount : Int
        newCount =
            if maxClock == clockMillis then
                {- If the time on the passed clock is equal to the maximum (in
                   other words, if the passed clock is greater than or equal to the
                   current time), increment the count of the clock by one
                -}
                count + 1

            else
                {- Otherwise, reset the count to 0 -}
                0
    in
    Hlc (millisToPosix maxClock) newCount id


{-| When an event is recieved from a remote source, this function can be used
to increment the clock.

Note: The order of arguments matters here; the first should be your local clock
and the second is the remote clock. It's important to order these correctly. If
you do not, this function will return an unreliable clock.

-}
remote : String -> LocalHlc -> RemoteHlc -> Posix -> Hlc
remote id (Hlc localClock localCount _) (Hlc remoteClock remoteCount _) now =
    let
        localClockMillis : Int
        localClockMillis =
            posixToMillis localClock

        remoteClockMillis : Int
        remoteClockMillis =
            posixToMillis remoteClock

        maxClock : Int
        maxClock =
            max (max localClockMillis remoteClockMillis) (posixToMillis now)

        newCount : Int
        newCount =
            if localClockMillis == maxClock && localClockMillis == remoteClockMillis then
                {- If the local clock and the remote clock are the same and both greater than
                   the current time, take the maximum of both the local and remote clocks counts
                   and increase it by one
                -}
                max localCount remoteCount + 1

            else if maxClock == localClockMillis then
                {- If the local clock is the highest clock, then increase the local clocks
                   count by one and use it as the new count
                -}
                localCount + 1

            else if maxClock == remoteClockMillis then
                {- If the remote clock is the highest clock, then increase the remote clocks
                   count by one and use it as the new count
                -}
                remoteCount + 1

            else
                {- In all other cases, specifically when the current time is greater than both
                   the local and remote clocks, reset the count to 0
                -}
                0
    in
    Hlc (millisToPosix maxClock) newCount id


{-| Compare two hybrid logical clocks to determine their order.

Note that the order of arguments is important! The first clock is being compared to the second.
The returned value will indicate if the first is greater than, less than, or equal
to, the second.

For example, running:

        const t = ""

-}
compare : Hlc -> Hlc -> Order
compare (Hlc firstClock firstCount firstId) (Hlc secondClock secondCount secondId) =
    case
        ( Basics.compare (posixToMillis firstClock) (posixToMillis secondClock)
        , Basics.compare firstCount secondCount
        , Basics.compare firstId secondId
        )
    of
        ( EQ, EQ, EQ ) ->
            {- This is highly unlikely as long as the id used for each clock are random! -}
            EQ

        ( EQ, EQ, GT ) ->
            GT

        ( EQ, EQ, LT ) ->
            GT

        ( EQ, GT, _ ) ->
            GT

        ( EQ, LT, _ ) ->
            LT

        ( GT, _, _ ) ->
            GT

        ( LT, _, _ ) ->
            LT


{-| Turn the clock into a string.

This string can use comparison operators (`==`, `>`, `<`, etc.) to determine what clock is
greater when comparing two clocks.

Running the following example:

        Hlc.create "unique-id" (millisToPosix 1685853812)
            |> Hlc.toString

Will result in the following string representation of the clock:

        "000001685853812:00000000:unique-id"

-}
toString : Hlc -> String
toString (Hlc clock count id) =
    let
        {- Turn the clock into a string. To do this in a way that allows string comparison,
           the string represenations of the clock must have the same amount of characters.

           For that reason, regardless of what the current clock `Posix` value is, this turns
           it into a 15 character string.
        -}
        clockString =
            clock
                |> posixToMillis
                |> String.fromInt
                |> String.padLeft 15 '0'

        {- I also make the count an 8 character string -}
        countString =
            String.fromInt count
                |> String.padLeft 8 '0'
    in
    {- And just concatinate the different strings with a `:` character! -}
    clockString
        ++ ":"
        ++ countString
        ++ ":"
        ++ id


{-| Attempts to take a string and turn it into a hybrid logical-clock.
-}
fromString : String -> Maybe Hlc
fromString hlcString =
    case String.split ":" hlcString of
        [ posixString, countString, idString ] ->
            Maybe.map2
                (\posix count ->
                    Hlc (millisToPosix posix) count idString
                )
                (String.toInt posixString)
                (String.toInt countString)

        _ ->
            Nothing


{-| Return the values in a clock.

This is primarily used in testing. It is not possible to reconstruct a clock from the
returned values.

-}
values : Hlc -> { clock : Posix, count : Int, id : String }
values (Hlc clock count id) =
    { clock = clock
    , count = count
    , id = id
    }
