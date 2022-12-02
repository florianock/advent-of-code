module DayXXTests

open NUnit.Framework
open DayXX

let test = @"1
1
"

[<Test>]
let solvePart1 () =
    let input = test |> preprocess
    let actual = solvePart1 input
    Assert.AreEqual(0, actual)


[<Test>]
let solvePart2 () =
    let input = test |> preprocess
    let actual = solvePart2 input
    Assert.AreEqual(0, actual)