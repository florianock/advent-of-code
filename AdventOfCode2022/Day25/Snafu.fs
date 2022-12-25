module Snafu

let chars = [| "="; "-"; "0"; "1"; "2" |]

let toDecimal (number: string) : int64 =
    if number |> String.length < 1 then
        failwith $"NaN {number}"

    number
    |> Seq.rev
    |> Seq.mapi (fun i c ->
        match c with
        | '2'
        | '1'
        | '0' -> (c |> string |> int64) * (pown 5L i)
        | '-' -> -1L * (pown 5L i)
        | '=' -> -2L * (pown 5L i)
        | _ -> failwith $"Wrong input {c}")
    |> Seq.sum

let fromDecimal (dec: int64) : string =
    let inline getChar n =
        let p = int (n + 2L) % 5

        if 0L <= n && n <= 2L then (chars[p], 0L)
        elif n = 3L || n = 4L then (chars[p], 5L)
        else failwith $"Unable to process number {n}"

    let rec loop n acc =
        match n with
        | 0L -> acc
        | num ->
            let rem = num % 5L
            let c, carry = getChar rem
            acc + c + (loop ((num + carry) / 5L) acc)

    (loop dec "") |> Seq.rev |> Seq.map string |> Seq.fold (+) ""
