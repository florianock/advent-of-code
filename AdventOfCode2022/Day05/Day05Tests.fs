module Day05Tests

open NUnit.Framework
open Day05

let test = @"    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
"

[<Test>]
let solvePart1 () =
    let input = test |> readLines
    let actual = solvePart1 input
    Assert.AreEqual("CMZ", actual)

[<Test>]
let solvePart2 () =
    let input = test |> readLines
    let actual = solvePart2 input
    Assert.AreEqual("MCD", actual)

[<Test>]
let getTopCratesBasic () =
    let input = [| [| "A" |] |]
    let actual = getTopCrates input
    Assert.AreEqual("A", actual)

[<Test>]
let getTopCratesMulti () =
    let input =
        [|
            [| "X"; "A" |]
            [| "X"; "X"; "B" |]
            [| "X"; "X"; "X"; "C" |]
        |]
    let actual = getTopCrates input
    Assert.AreEqual("ABC", actual)

[<Test>]
let applyInstructionTest () =
    let stack =
        [|
            [| "X"; "X" |]
            [| "X"; "B"; "A"; "X" |]
            [| "X"; "X"; "X"; "C" |]
        |]
    let instruction = struct(2, 2, 1)
    let actual = applyInstruction false stack instruction
    Assert.AreEqual(
        [|
            [| "X"; "X"; "X"; "A" |]
            [| "X"; "B" |]
            [| "X"; "X"; "X"; "C" |]
        |],
        actual)

[<Test>]
let applyInstructionTestMultipleCratesAtOnce () =
    let stack =
        [|
            [| "X"; "X" |]
            [| "X"; "B"; "X"; "A" |]
            [| "X"; "X"; "X"; "C" |]
        |]
    let instruction = struct(2, 2, 1)
    let actual = applyInstruction true stack instruction
    Assert.AreEqual(
        [|
            [| "X"; "X"; "X"; "A" |]
            [| "X"; "B" |]
            [| "X"; "X"; "X"; "C" |]
        |],
        actual)

[<Test>]
let followInstructionsTestBasic () =
    let stack =
        [|
            [| "X"; "X" |]
            [| "X"; "B"; "A" |]
            [| "X"; "X"; "X"; "C" |]
        |]
    let instructions = seq { "move 1 from 2 to 1" }
    let actual = followInstructions false (instructions, stack)
    Assert.AreEqual(
        [|
            [| "X"; "X"; "A" |]
            [| "X"; "B" |]
            [| "X"; "X"; "X"; "C" |]
        |],
        actual)

[<Test>]
let followInstructionsTestMoreMoves () =
    let stack =
        [|
            [| "X"; "X" |]
            [| "X"; "A" |]
            [| "X"; "X"; "X"; "C"; "B"; "X"; "X" |]
        |]
    let instructions =
        seq {
            "move 1 from 2 to 1"
            "move 3 from 3 to 2"
        }
    let actual = followInstructions false (instructions, stack)
    Assert.AreEqual(
        [|
            [| "X"; "X"; "A" |]
            [| "X"; "X"; "X"; "B" |]
            [| "X"; "X"; "X"; "C" |]
        |],
        actual)

[<Test>]
let followInstructionsTestMoreMovesAndMultipleCratesAtOnce () =
    let stack =
        [|
            [| "X"; "X" |]
            [| "X"; "A" |]
            [| "X"; "X"; "X"; "C"; "X"; "X"; "B" |]
        |]
    let instructions =
        seq {
            "move 1 from 2 to 1"
            "move 3 from 3 to 2"
        }
    let actual = followInstructions true (instructions, stack)
    Assert.AreEqual(
        [|
            [| "X"; "X"; "A" |]
            [| "X"; "X"; "X"; "B" |]
            [| "X"; "X"; "X"; "C" |]
        |],
        actual)

[<Test>]
let parseTest () =
    let actualInstructions, actualStack = test |> readLines |> parse
    Assert.AreEqual(
        seq {
            "move 1 from 2 to 1"
            "move 3 from 1 to 3"
            "move 2 from 2 to 1"
            "move 1 from 1 to 2"
        },
        actualInstructions)
    Assert.AreEqual(
        [|
            [| "Z"; "N" |]
            [| "M"; "C"; "D" |]
            [| "P" |]
        |],
        actualStack)
