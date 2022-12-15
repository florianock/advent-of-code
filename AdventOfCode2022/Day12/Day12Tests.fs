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

// [<Test>]
// let testMap () =
//     let result = DijkstraMap.generate 100 100 [| [| 'a' |]; [| 'b' |] |] [ 30, 40 ]
//     Assert.AreEqual (100, (result |> Array2D.length1))
