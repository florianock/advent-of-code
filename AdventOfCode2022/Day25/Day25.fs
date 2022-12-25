module Day25

// --- Day 25: Full of Hot Air ---

open Checked

let preprocess (puzzle: string) : seq<string> = puzzle.TrimEnd().Split "\n"

module Snafu =
    let toDecimal number =
        if number |> String.length < 1 then
            failwith $"NaN {number}"

        number
        |> Seq.rev
        |> Seq.mapi (fun i c ->
            match c with
            | '2'
            | '1'
            | '0' -> (pown 5L i) * (int64 (c.ToString ()))
            | '-' -> (pown 5L i) * -1L
            | '=' -> (pown 5L i) * -2L
            | _ -> failwith $"Wrong input {c}")
        |> Seq.sum

    let fromDecimal dec =
        let chars = [| "0"; "1"; "2"; "="; "-" |]

        let inline getChar n =
            if 0L <= n && n <= 2L then (chars[int n], 0L)
            elif n = 3L || n = 4L then (chars[int n], 5L)
            else failwith $"Unable to process number {n}"

        let mutable result = ""
        let mutable num = dec

        while num > 0L do
            let n = num % 5L
            let newN, extra = getChar n
            result <- result + newN
            num <- (num + extra) / 5L

        result |> Seq.rev |> Seq.map string |> Seq.fold (+) ""

let solvePart1 (input: seq<string>) =
    input |> Seq.sumBy Snafu.toDecimal |> Snafu.fromDecimal

let solvePart2 (input: seq<string>) = "tbd"
