module Day23Tests

open NUnit.Framework
open Day23

let test =
    @"....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#.."

[<Test>]
let solvePart1 () =
    let input = test |> preprocess
    let actual = solvePart1 input
    Assert.AreEqual(110, actual)

[<Test>]
let solvePart2 () =
    let input = test |> preprocess
    let actual = solvePart2 input
    Assert.AreEqual(0, actual)
