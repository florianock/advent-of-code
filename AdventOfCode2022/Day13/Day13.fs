module Day13

// source: https://github.com/jovaneyck/advent-of-code-2022/tree/main/day%2013
open FParsec

type Packet =
    | Int of int
    | List of Packet list

let packetParser, packetParserRef = createParserForwardedToRef ()

let intParser = pint32 |>> Int

let listParser =
    (pchar '[') >>. (sepBy packetParser (pchar ',')) .>> (pchar ']') |>> List

packetParserRef.Value <- (listParser <|> intParser)

let parsePacket packet =
    let result = run packetParser packet

    match result with
    | Success (res, _, _) -> res
    | _ -> failwithf "%A" result

let parse (packets: string[]) =
    packets
    |> Array.map (fun ps ->
        let [ left; right ] = ps.Split "\n" |> Array.map parsePacket |> Array.toList

        (left, right))
    |> Array.toList

let rec sorted left right =
    match left, right with
    | Int l, Int b when l < b -> true |> Some
    | Int l, Int b when l > b -> false |> Some
    | Int l, Int b when l = b -> None
    | List [], List [] -> None
    | List [], List _ -> true |> Some
    | List _, List [] -> false |> Some
    | List (l :: ls), List (r :: rs) ->
        match sorted l r with
        | Some true -> Some true
        | Some false -> Some false
        | None -> sorted (List ls) (List rs)
    | List l, Int r -> sorted (List l) (List [ Int r ])
    | Int l, List r -> sorted (List [ Int l ]) (List r)

let preprocess (puzzle: string) = puzzle.TrimEnd().Split "\n\n"

let solvePart1 input : int =
    parse input
    |> List.map (fun t -> t ||> sorted)
    |> List.indexed
    |> List.map (fun (i, f) -> (i + 1, f))
    |> List.filter (fun (_, t) ->
        match t with
        | Some true -> true
        | _ -> false)
    |> List.sumBy fst

let solvePart2 input =
    let packets =
        input
        |> Array.filter (fun s -> s <> "")
        |> Array.map (fun s -> s.Split "\n")
        |> Array.reduce Array.append
        |> Array.toList

    let divTwo = List [ List [ Int 2 ] ]
    let divSix = List [ List [ Int 6 ] ]
    let dividers = [ divTwo; divSix ]

    let ssorted left right =
        match sorted left right with
        | Some true -> -1
        | Some false -> 1
        | None -> failwithf "Could not compare %A with %A" left right

    let parsed = packets |> List.map parsePacket
    let full = List.append dividers parsed
    let s = full |> List.sortWith ssorted
    let idx2 = 1 + (s |> List.findIndex ((=) divTwo))
    let idx6 = 1 + (s |> List.findIndex ((=) divSix))
    let decoderKey = idx2 * idx6
    decoderKey
