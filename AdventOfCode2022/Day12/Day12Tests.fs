module Day12Tests

open NUnit.Framework
open Day12

let test =
    @"Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"

[<Test>]
let solvePart1 () =
    let input = test |> preprocess
    let actual = solvePart1 input
    Assert.AreEqual (31, actual)

[<Test>]
let solvePart2 () =
    let input = test |> preprocess
    let actual = solvePart2 input
    Assert.AreEqual (0, actual)
