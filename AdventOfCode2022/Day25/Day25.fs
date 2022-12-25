module Day25

// --- Day 25: Full of Hot Air ---

open Checked

let preprocess (puzzle: string) : seq<string> = puzzle.TrimEnd().Split "\n"

let snafuToDecimal number =
    if number |> String.length < 1 then
        failwith $"NaN {number}"

    number
    |> Seq.rev
    |> Seq.mapi (fun i c ->
        let powerOf5 = pown 5L i

        match c with
        | '2'
        | '1'
        | '0' -> powerOf5 * (int64 (c.ToString ()))
        | '-' -> powerOf5 * -1L
        | '=' -> powerOf5 * -2L
        | _ -> failwith $"Wrong input {c}")
    |> Seq.sum

let inline mapToSnafu num =
    let chars = [| "0"; "1"; "2"; "="; "-" |]

    if 0L <= num && num <= 2L then (chars[int num], 0L)
    elif num = 3L || num = 4L then (chars[int num], 1L)
    else failwith $"Unable to process number {num}"

let decimalToSnafu dec =
    let mutable snafu = ""
    let mutable num = dec

    while num > 0L do
        let n = num % 5L
        let newN, extra = mapToSnafu n
        snafu <- snafu + newN
        num <- (num + (5L * extra)) / 5L

    snafu |> Seq.rev |> Seq.map string |> Seq.fold (+) ""

let solvePart1 (input: seq<string>) =
    input |> Seq.sumBy snafuToDecimal |> decimalToSnafu

let solvePart2 (input: seq<string>) = "tbd"
