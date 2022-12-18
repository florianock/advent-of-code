module Day18Tests

open NUnit.Framework
open Day18

let test = @"2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5"

[<Test>]
let solvePart1 () =
    let input = test |> preprocess
    let actual = solvePart1 input
    Assert.AreEqual(64, actual)

[<Test>]
let solvePart2 () =
    let input = test |> preprocess
    let actual = solvePart2 input
    Assert.AreEqual(58, actual)

[<Test>]
let ``Adjacent cubes have 2 coordinates in common and 1 differs with 1`` () =
    Assert.IsTrue(adjacent (1, 1, 1) (2, 1, 1))
    Assert.IsFalse(adjacent (1, 1, 1) (3, 1, 1))
    Assert.IsFalse(adjacent (1, 1, 1) (4, 1, 1))
    Assert.IsFalse(adjacent (2, 3, 2) (2, 3, 4))
    Assert.IsTrue(adjacent (2, 3, 2) (2, 4, 2))
    Assert.IsFalse(adjacent (13, 19, 7) (12, 19, 6))

[<Test>]
let ``Pockets of air are cubes trapped within`` () =
    let input = test |> preprocess
    let actual = getOtherCubesWithRoomForAir (2, 2, 4) input
    Assert.AreEqual(5, actual.Length)

