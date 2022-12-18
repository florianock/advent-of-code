module Day14Tests

open NUnit.Framework
open Day14

let test =
    @"498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9"

[<Test>]
let solvePart1 () =
    let input = test |> preprocess
    let actual = solvePart1 input
    Assert.AreEqual (24, actual)

[<Test>]
let solvePart2 () =
    let input = test |> preprocess
    let actual = solvePart2 input
    Assert.AreEqual (0, actual)

let a1 = 1
let a2 = 4
let b1 = 500
let b2 = 503

List.zip
    [ a1..a2 ]
    [ for i = 0 to 3 do
          500 ]
