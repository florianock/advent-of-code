open System.IO
open System

// --- Day 1: Calorie Counting ---

let analyse (problem: string) =
    problem.TrimEnd().Split ("\n\n") |> Array.map (fun l -> l.Split ('\n'))

let sr = new StreamReader ("src/Day01/input.txt")
let fileContents = sr.ReadToEnd ()
let nums = fileContents |> analyse

let caloriesForElves (nums: string[][]) =
    nums
    |> Array.map (fun calories -> calories |> Array.map Int32.Parse |> Array.sum)

let solve1 (cals: string[][]) = caloriesForElves cals |> Array.max

let solve2 (cals: string[][]) =
    caloriesForElves cals |> Array.sortDescending |> Array.take 3 |> Array.sum

let raw_test =
    @"1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
"

let test = raw_test |> analyse

printfn $"first test answer is %d{solve1 test} (should be 24000)"
printfn $"first answer is %d{solve1 nums}"
printfn $"second test answer is %d{solve2 test} (should be 45000)"
printfn $"second answer is %d{solve2 nums}"
