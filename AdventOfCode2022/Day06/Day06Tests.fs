module Day06Tests

open NUnit.Framework
open Day06

let test =
    @"mjqjpqmgbljsphdztnvjfqwrcgsmlb
"

[<Test>]
let solvePart1 () =
    let input = test |> preprocess
    let actual = solvePart1 input
    Assert.AreEqual (7, actual)

[<Test>]
let solvePart2 () =
    let input = test |> preprocess
    let actual = solvePart2 input
    Assert.AreEqual (19, actual)
