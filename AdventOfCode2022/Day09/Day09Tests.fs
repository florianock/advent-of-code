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

let testLarger =
    @"R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20"

[<Test>]
let solvePart1 () =
    let input = test |> preprocess
    let actual = solvePart1 input
    Assert.AreEqual (13, actual)

[<Test>]
let solvePart2Simple () =
    let input = test |> preprocess
    let actual = solvePart2 input
    Assert.AreEqual (1, actual)

[<Test>]
let solvePart2LargerExample () =
    let input = testLarger |> preprocess
    let actual = solvePart2 input
    Assert.AreEqual (36, actual)
