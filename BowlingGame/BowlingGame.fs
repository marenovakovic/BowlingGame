module FSharp_Follies.BowlingGameTest

open FsUnit
open NUnit.Framework

let isSpare x y = x + y = 10

let calculateScore rolls =
    let rec loop score turn rolls =
        match turn, rolls with
        | 0, _ -> score
        | _, 10 :: (x :: y :: _ as rolls) -> loop (10 + x + y + score) (turn - 1) rolls
        | _, x :: y :: (z :: _ as rolls) when isSpare x y -> loop (10 + z + score) (turn - 1) rolls
        | _, x :: y :: rolls -> loop (x + y + score) (turn - 1) rolls
        | _, _ -> score

    loop 0 10 rolls

[<Test>]
let ``test gutter game`` () =
    List.replicate 20 0
    |> calculateScore
    |> should equal 0

[<Test>]
let ``test game with one pin`` () =
    [ 1 ] @ List.replicate 19 0
    |> calculateScore
    |> should equal 1

[<Test>]
let ``test game with one spare`` () =
    [ 5; 5; 3 ] @ List.replicate 17 0
    |> calculateScore
    |> should equal 16

[<Test>]
let ``test game with one strike`` () =
    [ 10; 3; 4 ] @ List.replicate 16 0
    |> calculateScore
    |> should equal 24

[<Test>]
let ``test perfect game`` () =
    List.replicate 12 10
    |> calculateScore
    |> should equal 300
