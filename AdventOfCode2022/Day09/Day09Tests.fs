module Day09Tests

open NUnit.Framework
open Day09

let test =
    @"R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
"

[<Test>]
let solvePart1 () =
    let input = test |> preprocess
    let actual = solvePart1 input
    Assert.AreEqual (13, actual)

[<Test>]
let solvePart2 () =
    let input = test |> preprocess
    let actual = solvePart2 input
    Assert.AreEqual (0, actual)
