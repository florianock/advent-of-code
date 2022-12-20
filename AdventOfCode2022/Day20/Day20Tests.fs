module Day20Tests

open NUnit.Framework
open Day20

let test =
    @"1
2
-3
3
-2
0
4"

[<Test>]
let solvePart1 () =
    let input = test |> preprocess
    let actual = solvePart1 input
    Assert.AreEqual (3, actual)

[<Test>]
let solvePart2 () =
    let input = test |> preprocess
    let actual = solvePart2 input
    Assert.AreEqual (0, actual)
