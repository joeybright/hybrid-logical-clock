module HlcTests exposing
    ( localTests, localRemoteTests
    , sortTests
    )

{-|

@docs localTests, localRemoteTests

@docs sortTests

-}

import Expect
import Fuzz
import Hlc
import Test exposing (Test, describe, fuzz, test)
import Time exposing (millisToPosix)


type alias ClockTestValues =
    { clock : Hlc.Hlc
    , string : String
    , values : { clock : Time.Posix, count : Int, id : String }
    }


{-| Testing incrementing local clocks
-}
localTests : Test
localTests =
    describe "Testing incrementing a clock locally"
        [ test
            """If the local clock is ahead of the wall clock and the clock is incremented, the new
            clock maintains the local clock timestamp and has a count that is one higher than the
            local clock"""
            (\_ ->
                {- Order : local <- wall -}
                let
                    localClock =
                        {- Saturday, September 21, 1991 2:53:06 AM -}
                        Hlc.create "Fqwkze" (millisToPosix 685421586)

                    wallClock =
                        {- Monday, October 8, 1990 8:25:42 PM -}
                        millisToPosix 655417542
                in
                Hlc.local "bIfQod" wallClock localClock
                    |> (\clock -> ( Hlc.toString clock, Hlc.values clock ))
                    |> Expect.all
                        [ Tuple.first >> Expect.equal "000000685421586:00000001:bIfQod"
                        , Tuple.second >> .clock >> Expect.equal (millisToPosix 685421586)
                        , Tuple.second >> .count >> Expect.equal 1
                        , Tuple.second >> .id >> Expect.equal "bIfQod"
                        ]
            )
        , test
            """If the local clock is behind of the wall clock and the clock is incremented, the new
            clock is equal to the wall clock with a count of 0"""
            (\_ ->
                {- Order: wall <- local -}
                let
                    localClock =
                        {- Thursday, June 7, 1984 12:02:22 AM -}
                        Hlc.create "rPaUnw" (millisToPosix 455414542)

                    wallClock =
                        {- Wednesday, July 29, 1992 12:55:42 PM -}
                        millisToPosix 712414542
                in
                Hlc.local "w8RIc5" wallClock localClock
                    |> (\clock -> ( Hlc.toString clock, Hlc.values clock ))
                    |> Expect.all
                        [ Tuple.first >> Expect.equal "000000712414542:00000000:w8RIc5"
                        , Tuple.second >> .clock >> Expect.equal (millisToPosix 712414542)
                        , Tuple.second >> .count >> Expect.equal 0
                        , Tuple.second >> .id >> Expect.equal "w8RIc5"
                        ]
            )
        , test
            """If the local clock is equal to the wall clock and the clock is incremented, the new
            clock is equal to both the local and wall clock with a count that is one higher than the
            local clock"""
            (\_ ->
                {- Order: local = wall -}
                let
                    localClock =
                        Hlc.create "1MiUKZ" wallClock

                    wallClock =
                        {- Sunday, August 3, 1986 12:50:42 AM -}
                        millisToPosix 523414242
                in
                Hlc.local "ppXkLh" wallClock localClock
                    |> (\clock -> ( Hlc.toString clock, Hlc.values clock ))
                    |> Expect.all
                        [ Tuple.first >> Expect.equal "000000523414242:00000001:ppXkLh"
                        , Tuple.second >> .clock >> Expect.equal (millisToPosix 523414242)
                        , Tuple.second >> .count >> Expect.equal 1
                        , Tuple.second >> .id >> Expect.equal "ppXkLh"
                        ]
            )
        , fuzz
            (Fuzz.map2
                (\local remote -> ( local, remote ))
                (Fuzz.intRange 0 1203210302)
                (Fuzz.intRange 0 1203210303)
            )
            """When incrementing a local clock with random local and wall clock values, the result
            is expected"""
            (\( localPosix, wallPosix ) ->
                let
                    localClock =
                        Hlc.create "Opq4eb" (millisToPosix localPosix)

                    wallClock =
                        millisToPosix wallPosix

                    testHelper { newClock, newCount, newId } =
                        Expect.all
                            [ .values >> .clock >> Expect.equal newClock
                            , .values >> .count >> Expect.equal newCount
                            , .values >> .id >> Expect.equal newId

                            {- Testing to make sure conversion and comparion of strings is working
                               correctly
                            -}
                            , .string >> String.length >> Expect.equal 31
                            , \{ clock, string } -> Hlc.fromString string |> Expect.equal (Just clock)
                            , .clock >> (\c -> compare (Hlc.toString c) (Hlc.toString localClock)) >> Expect.equal GT
                            , .clock >> (\c -> compare (Hlc.toString localClock) (Hlc.toString c)) >> Expect.equal LT

                            {- Testing to make sure that the new clock `c` is greater then the previous
                               clock and the old clock is less than the new clock `c`
                            -}
                            , .clock >> (\c -> Hlc.compare c localClock) >> Expect.equal GT
                            , .clock >> (\c -> Hlc.compare localClock c) >> Expect.equal LT
                            ]
                in
                case compare localPosix wallPosix of
                    EQ ->
                        Hlc.local "Mvw5mt" wallClock localClock
                            |> clockToTestValues
                            |> testHelper
                                { newClock = millisToPosix wallPosix
                                , newCount = 1
                                , newId = "Mvw5mt"
                                }

                    LT ->
                        Hlc.local "taacA8" wallClock localClock
                            |> clockToTestValues
                            |> testHelper
                                { newClock = wallClock
                                , newCount = 0
                                , newId = "taacA8"
                                }

                    GT ->
                        Hlc.local "NW16aV" wallClock localClock
                            |> clockToTestValues
                            |> testHelper
                                { newClock = millisToPosix localPosix
                                , newCount = 1
                                , newId = "NW16aV"
                                }
            )
        ]


{-| Testing interactions between local and remote clocks
-}
localRemoteTests : Test
localRemoteTests =
    describe "Testing local and remote clock interactions"
        [ test
            """If receiving a remote clock that is ahead of the local clock and the wall clock, 
            the new clock is equal to the remote clock with a count of the remote clock's count 
            plus one"""
            (\_ ->
                {- Order: remote <- local / wall -}
                let
                    localClock =
                        {- Saturday, September 21, 1991 2:53:06 AM -}
                        Hlc.create "Fqwkze" (millisToPosix 685421586)

                    remoteClock =
                        {- Friday, April 14, 2023 7:02:40 PM -}
                        Hlc.create "WuVls0" (millisToPosix 1681498960)

                    wallClock =
                        {- Monday, October 8, 1990 8:25:42 PM -}
                        millisToPosix 655417542
                in
                Hlc.remote "Z3HGBA" localClock remoteClock wallClock
                    |> clockToTestValues
                    |> Expect.all
                        ([ .string >> Expect.equal "000001681498960:00000001:Z3HGBA"
                         , .values >> .clock >> Expect.equal (millisToPosix 1681498960)
                         , .values >> .count >> Expect.equal 1
                         , .values >> .id >> Expect.equal "Z3HGBA"
                         ]
                            ++ clockToStringTests localClock remoteClock
                        )
            )
        , test
            """If receiving a remote clock that is ahead of the local clock, but behind the wall
            clock, the new clock is equal to the wall clock with a count of 0"""
            (\_ ->
                {- Order: wall <- remote <- local -}
                let
                    localClock =
                        {- Saturday, September 21, 1991 2:53:06 AM -}
                        Hlc.create "Fqwkze" (millisToPosix 685421586)

                    remoteClock =
                        {- Friday, April 14, 2023 7:02:40 PM -}
                        Hlc.create "WuVls0" (millisToPosix 1681498960)

                    wallClock =
                        {- Sunday, June 4, 2023 5:27:11 PM -}
                        millisToPosix 1685899631
                in
                Hlc.remote "9FmFs7" localClock remoteClock wallClock
                    |> clockToTestValues
                    |> Expect.all
                        ([ .string >> Expect.equal "000001685899631:00000000:9FmFs7"
                         , .values >> .clock >> Expect.equal (millisToPosix 1685899631)
                         , .values >> .count >> Expect.equal 0
                         , .values >> .id >> Expect.equal "9FmFs7"
                         , (\_ -> compare (Hlc.toString remoteClock) (Hlc.toString (Hlc.create "" wallClock))) >> Expect.equal LT
                         , (\_ -> compare (Hlc.toString (Hlc.create "" wallClock)) (Hlc.toString remoteClock)) >> Expect.equal GT
                         , (\_ -> compare (Hlc.toString localClock) (Hlc.toString (Hlc.create "" wallClock))) >> Expect.equal LT
                         , (\_ -> compare (Hlc.toString (Hlc.create "" wallClock)) (Hlc.toString localClock)) >> Expect.equal GT
                         ]
                            ++ clockToStringTests localClock remoteClock
                        )
            )
        , test
            """If receiving a remote clock that is ahead of the local clock, but equal to the wall
            clock, the new clock is equal to the remote and wall clock with a count of the remote
            clock's count plus one"""
            (\_ ->
                {- Order: remote / wall <- local -}
                let
                    localClock =
                        {- Saturday, September 21, 1991 2:53:06 AM -}
                        Hlc.create "Fqwkze" (millisToPosix 685421586)

                    remoteClock =
                        {- Wednesday, August 5, 2026 3:13:51 AM -}
                        Hlc.create "WuVls0" wallClock
                            |> Hlc.local "jP132s" wallClock

                    wallClock =
                        {- Wednesday, August 5, 2026 3:13:51 AM -}
                        millisToPosix 1785899631
                in
                Hlc.remote "Q0wl8k" localClock remoteClock wallClock
                    |> clockToTestValues
                    |> Expect.all
                        ([ .string >> Expect.equal "000001785899631:00000002:Q0wl8k"
                         , .values >> .clock >> Expect.equal (millisToPosix 1785899631)
                         , .values >> .count >> Expect.equal 2
                         , .values >> .id >> Expect.equal "Q0wl8k"
                         ]
                            ++ clockToStringTests localClock remoteClock
                        )
            )
        , test
            """If receiving a remote clock that is behind the local clock, but ahead of the wall
            clock, the new clock is equal to the local clock with a count of the local clock's count
            plus one"""
            (\_ ->
                {- Order: local <- remote <- wall -}
                let
                    localClock =
                        {- Saturday, November 27, 2032 6:25:31 AM -}
                        Hlc.create "Fqwkze" (millisToPosix 1985149531)
                            |> Hlc.local "jP132s" wallClock

                    remoteClock =
                        {- Wednesday, August 5, 2026 3:13:51 AM -}
                        Hlc.create "WuVls0" wallClock

                    wallClock =
                        {- Wednesday, August 5, 2026 3:13:51 AM -}
                        millisToPosix 1785899631
                in
                Hlc.remote "i3skpO" localClock remoteClock wallClock
                    |> clockToTestValues
                    |> Expect.all
                        ([ .string >> Expect.equal "000001985149531:00000002:i3skpO"
                         , .values >> .clock >> Expect.equal (millisToPosix 1985149531)
                         , .values >> .count >> Expect.equal 2
                         , .values >> .id >> Expect.equal "i3skpO"
                         ]
                            ++ clockToStringTests localClock remoteClock
                        )
            )
        , test
            """If receiving a remote clock that is behind the local clock and the wall clock, the
            new clock will not be equal to the remote clock's value or count"""
            (\_ ->
                {- Order: local / wall <- remote -}
                let
                    localClock =
                        {- Thursday, March 31, 2039 1:58:52 AM -}
                        Hlc.create "Fqwkze" (millisToPosix 2185149532)
                            |> Hlc.local "jP132s" (millisToPosix 2185149532)

                    remoteClock =
                        {- Tuesday, May 3, 2005 7:45:31 PM -}
                        Hlc.create "WuVls0" (millisToPosix 1115149531)

                    wallClock =
                        {- Thursday, March 31, 2039 1:58:51 AM -}
                        millisToPosix 2185149531
                in
                Hlc.remote "u529dH" localClock remoteClock wallClock
                    |> clockToTestValues
                    |> Expect.all
                        ([ .string >> Expect.equal "000002185149532:00000002:u529dH"
                         , .values >> .clock >> Expect.equal (millisToPosix 2185149532)
                         , .values >> .clock >> Expect.notEqual (millisToPosix 1115149531) -- Remote clock
                         , .values >> .count >> Expect.equal 2
                         , .values >> .count >> Expect.notEqual 1 -- Remote clock count
                         , .values >> .id >> Expect.equal "u529dH"
                         ]
                            ++ clockToStringTests localClock remoteClock
                        )
            )
        , test
            """If receiving a remote clock that is behind the local clock, but equal to the wall
            clock, the new clock will equal the local clock with a count of the local clock's count
            plus one"""
            (\_ ->
                {- Order: local <- remote / wall -}
                let
                    localClock =
                        {- Thursday, March 31, 2039 1:58:52 AM -}
                        Hlc.create "Fqwkze" (millisToPosix 2185149532)

                    remoteClock =
                        {- Tuesday, May 3, 2005 7:45:31 PM -}
                        Hlc.create "WuVls0" wallClock

                    wallClock =
                        {- Thursday, March 31, 2039 1:58:51 AM -}
                        millisToPosix 2185149531
                in
                Hlc.remote "Wesdm1" localClock remoteClock wallClock
                    |> clockToTestValues
                    |> Expect.all
                        ([ .string >> Expect.equal "000002185149532:00000001:Wesdm1"
                         , .values >> .clock >> Expect.equal (millisToPosix 2185149532)
                         , .values >> .count >> Expect.equal 1
                         , .values >> .id >> Expect.equal "Wesdm1"
                         ]
                            ++ clockToStringTests localClock remoteClock
                        )
            )
        , test
            """If receiving a remote clock that is equal to the local clock, but ahead of the wall
            clock, the new clock is equal to both the remote and local clocks with a count that is 
            the maximum of the remote and local clock count's plus one"""
            (\_ ->
                {- Order: remote + local <- wall -}
                let
                    localClock =
                        {- Monday, April 24, 2023 2:23:43 AM -}
                        Hlc.create "Fqwkze" (millisToPosix 1682303023)

                    remoteClock =
                        {- Monday, April 24, 2023 2:23:43 AM -}
                        Hlc.create "WuVls0" (millisToPosix 1682303023)

                    wallClock =
                        {- Friday, October 3, 2014 2:23:43 AM -}
                        millisToPosix 1412303023
                in
                Hlc.remote "GcZ1TO" localClock remoteClock wallClock
                    |> clockToTestValues
                    |> Expect.all
                        ([ .string >> Expect.equal "000001682303023:00000001:GcZ1TO"
                         , .values >> .clock >> Expect.equal (millisToPosix 1682303023)
                         , .values >> .count >> Expect.equal 1
                         , .values >> .id >> Expect.equal "GcZ1TO"
                         ]
                            ++ clockToStringTests localClock remoteClock
                        )
            )
        , test
            """If receiving a remote clock that is equal to the local clock but behind the wall clock,
            the new clock is equal to the wall clock with a count of 0"""
            (\_ ->
                {- Order : wall <- remote + local -}
                let
                    localClock =
                        {- Monday, April 24, 2023 2:23:43 AM -}
                        Hlc.create "Fqwkze" (millisToPosix 1682303023)

                    remoteClock =
                        {- Monday, April 24, 2023 2:23:43 AM -}
                        Hlc.create "WuVls0" (millisToPosix 1682303023)

                    wallClock =
                        {- Sunday, October 27, 2109 7:43:43 AM -}
                        millisToPosix 4412303023
                in
                Hlc.remote "Yldc5a" localClock remoteClock wallClock
                    |> clockToTestValues
                    |> Expect.all
                        ([ .string >> Expect.equal "000004412303023:00000000:Yldc5a"
                         , .values >> .clock >> Expect.equal (millisToPosix 4412303023)
                         , .values >> .count >> Expect.equal 0
                         , .values >> .id >> Expect.equal "Yldc5a"
                         ]
                            ++ clockToStringTests localClock remoteClock
                        )
            )
        , test
            """If receiving a remote clock that is equal to the local and wall clock, the new clock is
            equal to all clocks (local, remote, and now) with a count that is the maximum of the remote 
            and local clock count's plus one"""
            (\_ ->
                {- Order : remote / local / wall -}
                let
                    localClock =
                        Hlc.create "Fqwkze" wallClock

                    remoteClock =
                        Hlc.create "WuVls0" wallClock
                            |> Hlc.local "" wallClock

                    wallClock =
                        {- Sunday, October 27, 2109 7:43:43 AM -}
                        millisToPosix 4412303023
                in
                Hlc.remote "MLTSnJ" localClock remoteClock wallClock
                    |> clockToTestValues
                    |> Expect.all
                        ([ .string >> Expect.equal "000004412303023:00000002:MLTSnJ"
                         , .values >> .clock >> Expect.equal (millisToPosix 4412303023)
                         , .values >> .count >> Expect.equal 2
                         , .values >> .id >> Expect.equal "MLTSnJ"
                         ]
                            ++ clockToStringTests localClock remoteClock
                        )
            )
        ]


{-| Testing sorting of clocks
-}
sortTests : Test
sortTests =
    describe "Testing how lists of hybrid logical clocks are ordered and if it's expected"
        [ test
            """Given a series of local changes to a clock, where all changes are kept in a list, the 
            changes, when sorted, result in the expected order of events"""
            (\_ ->
                let
                    initalClock =
                        Hlc.create "a" (millisToPosix 1685904424)

                    firstEvent =
                        Hlc.local "b" (millisToPosix 1685904425) initalClock

                    secondEvent =
                        Hlc.local "c" (millisToPosix 1685904425) firstEvent

                    thirdEvent =
                        Hlc.local "d" (millisToPosix 1685904416) secondEvent
                in
                [ thirdEvent, firstEvent, initalClock, secondEvent ]
                    |> List.map Hlc.toString
                    |> List.sort
                    |> Expect.equal
                        [ Hlc.toString initalClock
                        , Hlc.toString firstEvent
                        , Hlc.toString secondEvent
                        , Hlc.toString thirdEvent
                        ]
            )
        ]


{-| A helper function for taking a clock and converting it into values for testing.
-}
clockToTestValues : Hlc.Hlc -> ClockTestValues
clockToTestValues clock =
    { string = Hlc.toString clock
    , values = Hlc.values clock
    , clock = clock
    }


{-| Helper function to generate standard tests for converting and comparing hybrid logical clocks
as strings.
-}
clockToStringTests : Hlc.LocalHlc -> Hlc.RemoteHlc -> List (ClockTestValues -> Expect.Expectation)
clockToStringTests localClock remoteClock =
    {- Considering the `clock` value in the `ClockTestValues` to be the latest clock generated -}
    [ .string >> String.length >> Expect.equal 31
    , \{ clock, string } -> Hlc.fromString string |> Expect.equal (Just clock)
    , .clock >> (\c -> compare (Hlc.toString c) (Hlc.toString c)) >> Expect.equal EQ
    , .clock >> (\c -> compare (Hlc.toString c) (Hlc.toString localClock)) >> Expect.equal GT
    , .clock >> (\c -> compare (Hlc.toString localClock) (Hlc.toString c)) >> Expect.equal LT
    , .clock >> (\c -> compare (Hlc.toString c) (Hlc.toString remoteClock)) >> Expect.equal GT
    , .clock >> (\c -> compare (Hlc.toString remoteClock) (Hlc.toString c)) >> Expect.equal LT
    ]
