module Day15Tests

open NUnit.Framework
open Day15

let test = @"Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3
"

[<Test>]
let solvePart1 () =
    let input = test |> preprocess
    let actual = solvePart1 10 input
    Assert.AreEqual(26, actual)

[<Test>]
let solvePart2 () =
    let input = test |> preprocess
    let actual = solvePart2 20 input
    Assert.AreEqual(56000011, actual)

[<Test>]
let ``coversRowFromDistance correctly calculates cover`` () =
    let sensorData =
        { Position = (8, 7)
          Beacon = (2, 10)
          Distance = 9 }
    let actual = coversRowFromDistance 10 sensorData
    Assert.AreEqual(set [2..14], actual)
    