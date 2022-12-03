module AdventOfCode2022.Day02.Program

open System.IO
open Day02

let duration f input = 
    let timer = System.Diagnostics.Stopwatch()
    timer.Start()
    let returnValue = f input
    timer.Stop()
    printfn "Elapsed Time: %i ms" timer.ElapsedMilliseconds
    timer.Reset()
    returnValue

[<EntryPoint>]
let main argv =
    let input = File.ReadAllText "./input.txt" |> preprocess
    let resultPart1 = duration solvePart1 input
    printfn $"Part 1: %d{resultPart1}\n"
    let resultPart2 = duration solvePart2 input
    printfn $"Part 2: %d{resultPart2}"
    0
