module Day02Tests

open NUnit.Framework
open Day02

let test = @"A Y
B X
C Z
"

[<Test>]
let solvePart1 () =
    let input = test |> preprocess
    let actual = solvePart1 input
    Assert.AreEqual(15, actual)


[<Test>]
let solvePart2 () =
    let input = test |> preprocess
    let actual = solvePart2 input
    Assert.AreEqual(12, actual)