module Modulus exposing (D(..), D1(..), D2(..), D3(..), D4(..), D5(..), D6(..), D7(..), D8(..), D9(..), Modulus, d0, d1, d2, d3, d4, d5, d6, d7, d8, d9, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, modBySafe)

import Html


main =
    Html.text "Hello!"


type D f p
    = D0 f
    | D1 f
    | D2 f
    | D3 f
    | D4 f
    | D5 f
    | D6 f
    | D7 f
    | D8 f
    | D9 p


type D9 f p
    = D0_9 f
    | D1_9 f
    | D2_9 f
    | D3_9 f
    | D4_9 f
    | D5_9 f
    | D6_9 f
    | D7_9 f
    | D8_9 p


type D8 f p
    = D0_8 f
    | D1_8 f
    | D2_8 f
    | D3_8 f
    | D4_8 f
    | D5_8 f
    | D6_8 f
    | D7_8 p


type D7 f p
    = D0_7 f
    | D1_7 f
    | D2_7 f
    | D3_7 f
    | D4_7 f
    | D5_7 f
    | D6_7 p


type D6 f p
    = D0_6 f
    | D1_6 f
    | D2_6 f
    | D3_6 f
    | D4_6 f
    | D5_6 p


type D5 f p
    = D0_5 f
    | D1_5 f
    | D2_5 f
    | D3_5 f
    | D4_5 p


type D4 f p
    = D0_4 f
    | D1_4 f
    | D2_4 f
    | D3_4 p


type D3 f p
    = D0_3 f
    | D1_3 f
    | D2_3 p


type D2 f p
    = D0_2 f
    | D1_2 p


type D1 f p
    = D0_1 p


type Modulus f p
    = Modulus
        { partial : Int -> p
        , full : Int -> ( f, Int )
        , modulus : Int -> Int
        }


modBySafe : Modulus f p -> Int -> p
modBySafe (Modulus { partial, modulus }) n =
    let
        unsafe =
            modBy (modulus 0) n
    in
    partial unsafe


crash : Int -> b
crash n =
    case modBy 0 n of
        _ ->
            crash n


build1 : Int -> f -> p -> D1 f p
build1 n f p =
    case modBy 1 n of
        0 ->
            D0_1 p

        _ ->
            crash n


build2 : Int -> f -> p -> D2 f p
build2 n f p =
    case modBy 2 n of
        0 ->
            D0_2 f

        1 ->
            D1_2 p

        _ ->
            crash n


build3 : Int -> f -> p -> D3 f p
build3 n f p =
    case modBy 3 n of
        0 ->
            D0_3 f

        1 ->
            D1_3 f

        2 ->
            D2_3 p

        _ ->
            crash n


build4 : Int -> f -> p -> D4 f p
build4 n f p =
    case modBy 4 n of
        0 ->
            D0_4 f

        1 ->
            D1_4 f

        2 ->
            D2_4 f

        3 ->
            D3_4 p

        _ ->
            crash n


build5 : Int -> f -> p -> D5 f p
build5 n f p =
    case modBy 5 n of
        0 ->
            D0_5 f

        1 ->
            D1_5 f

        2 ->
            D2_5 f

        3 ->
            D3_5 f

        4 ->
            D4_5 p

        _ ->
            crash n


build6 : Int -> f -> p -> D6 f p
build6 n f p =
    case modBy 6 n of
        0 ->
            D0_6 f

        1 ->
            D1_6 f

        2 ->
            D2_6 f

        3 ->
            D3_6 f

        4 ->
            D4_6 f

        5 ->
            D5_6 p

        _ ->
            crash n


build7 : Int -> f -> p -> D7 f p
build7 n f p =
    case modBy 7 n of
        0 ->
            D0_7 f

        1 ->
            D1_7 f

        2 ->
            D2_7 f

        3 ->
            D3_7 f

        4 ->
            D4_7 f

        5 ->
            D5_7 f

        6 ->
            D6_7 p

        _ ->
            crash n


build8 : Int -> f -> p -> D8 f p
build8 n f p =
    case modBy 8 n of
        0 ->
            D0_8 f

        1 ->
            D1_8 f

        2 ->
            D2_8 f

        3 ->
            D3_8 f

        4 ->
            D4_8 f

        5 ->
            D5_8 f

        6 ->
            D6_8 f

        7 ->
            D7_8 p

        _ ->
            crash n


build9 : Int -> f -> p -> D9 f p
build9 n f p =
    case modBy 9 n of
        0 ->
            D0_9 f

        1 ->
            D1_9 f

        2 ->
            D2_9 f

        3 ->
            D3_9 f

        4 ->
            D4_9 f

        5 ->
            D5_9 f

        6 ->
            D6_9 f

        7 ->
            D7_9 f

        8 ->
            D8_9 p

        _ ->
            crash n


build10 : Int -> f -> p -> D f p
build10 n f p =
    case modBy 10 n of
        0 ->
            D0 f

        1 ->
            D1 f

        2 ->
            D2 f

        3 ->
            D3 f

        4 ->
            D4 f

        5 ->
            D5 f

        6 ->
            D6 f

        7 ->
            D7 f

        8 ->
            D8 f

        9 ->
            D9 p

        _ ->
            crash n


build : Int -> (Int -> f -> p -> n) -> Modulus f p -> Modulus (D f f) n
build modulus builder (Modulus next) =
    let
        full : Int -> ( D f f, Int )
        full n =
            let
                ( f, d ) =
                    next.full n
            in
            ( build10 d f f, d // 10 )

        partial n =
            let
                ( f, d ) =
                    next.full n

                p =
                    next.partial n
            in
            builder (modBy 10 d) f p
    in
    Modulus
        { full = full
        , partial = partial
        , modulus = \r -> next.modulus <| r * 10 + modulus
        }


d0 : Modulus f p -> Modulus (D f f) (D1 f p)
d0 =
    build 0 build1


d1 : Modulus f p -> Modulus (D f f) (D2 f p)
d1 =
    build 1 build2


d2 : Modulus f p -> Modulus (D f f) (D3 f p)
d2 =
    build 2 build3


d3 : Modulus f p -> Modulus (D f f) (D4 f p)
d3 =
    build 3 build4


d4 : Modulus f p -> Modulus (D f f) (D5 f p)
d4 =
    build 4 build5


d5 : Modulus f p -> Modulus (D f f) (D6 f p)
d5 =
    build 5 build6


d6 : Modulus f p -> Modulus (D f f) (D7 f p)
d6 =
    build 6 build7


d7 : Modulus f p -> Modulus (D f f) (D8 f p)
d7 =
    build 7 build8


d8 : Modulus f p -> Modulus (D f f) (D9 f p)
d8 =
    build 8 build9


d9 : Modulus f p -> Modulus (D f f) (D f p)
d9 =
    build 9 build10


buildFinal : Int -> (Int -> () -> () -> d) -> Modulus (D () ()) d
buildFinal m p =
    Modulus
        { modulus = \r -> r * 10 + m
        , full = \n -> ( build10 n () (), n // 10 )
        , partial = \n -> p (modBy 10 n) () ()
        }


f0 : Modulus (D () ()) Never
f0 =
    buildFinal 0 crash


f1 : Modulus (D () ()) (D1 () ())
f1 =
    buildFinal 1 build1


f2 : Modulus (D () ()) (D2 () ())
f2 =
    buildFinal 2 build2


f3 : Modulus (D () ()) (D3 () ())
f3 =
    buildFinal 3 build3


f4 : Modulus (D () ()) (D4 () ())
f4 =
    buildFinal 4 build4


f5 : Modulus (D () ()) (D5 () ())
f5 =
    buildFinal 5 build5


f6 : Modulus (D () ()) (D6 () ())
f6 =
    buildFinal 6 build6


f7 : Modulus (D () ()) (D7 () ())
f7 =
    buildFinal 7 build7


f8 : Modulus (D () ()) (D8 () ())
f8 =
    buildFinal 8 build8


f9 : Modulus (D () ()) (D9 () ())
f9 =
    buildFinal 9 build9
