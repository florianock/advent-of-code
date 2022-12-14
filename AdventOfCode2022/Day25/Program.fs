module AdventOfCode2022.Day25.Program

open System.IO
open Day25

[<EntryPoint>]
let main argv =
    let input = File.ReadAllText "./input.txt" |> preprocess

    let timer = System.Diagnostics.Stopwatch ()
    timer.Start ()

    let resultPart1 = solvePart1 input

    let part1Duration = timer.ElapsedMilliseconds
    printfn $"Part 1: %s{resultPart1} (execution took %i{part1Duration} ms)"

    let resultPart2 = solvePart2 input

    timer.Stop ()
    printfn $"Part 2: %s{resultPart2} (execution took %i{timer.ElapsedMilliseconds - part1Duration} ms)"
    printfn $"Total execution time: %i{timer.ElapsedMilliseconds} ms"
    0
