module Day21Tests

open NUnit.Framework
open Day21

let test =
    @"root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32"

[<Test>]
let solvePart1 () =
    let input = test |> preprocess
    let actual = solvePart1 input
    Assert.AreEqual (152, actual)

[<Test>]
let solvePart2 () =
    let input = test |> preprocess
    let actual = solvePart2 input
    Assert.AreEqual (301, actual)


// root: pppw = sjmn    (4 + (2 * (x - 3))) / 4 = 150

// left = (4 + (2 * (x - 3))) / 4 = 150
// pppw: cczh / lfqf    (4 + (2 * (x - 3))) / 4 = 150 -> 150 * 4
// cczh: sllz + lgvd    4 + (2 * (x - 3)) = 600 -> 600 - 4
// lfqf: 4
// sllz: 4
// lgvd: ljgn * ptdq    2 * (x - 3) = 596 -> 596 / 2
// ljgn: 2
// ptdq: humn - dvpt    x - 3 = 298 -> 298 + 3 = 301 __x = 301__
// humn: 5
// dvpt: 3

// right = 150
// sjmn: drzm * dbpl    30 * 5
// drzm: hmdt - zczc    32 - 2
// dbpl: 5
// hmdt: 32
// zczc: 2
