module Day24Tests

open NUnit.Framework
open Day24

let testSimple = @"#.#####
#.....#
#>....#
#.....#
#...v.#
#.....#
#####.#"

let test = @"#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#"

[<Test>]
let solvePart1 () =
    let input = test |> preprocess
    let actual = solvePart1 input
    Assert.AreEqual(18, actual)

[<Test>]
let solvePart2 () =
    let input = test |> preprocess
    let actual = solvePart2 input
    Assert.AreEqual(0, actual)
