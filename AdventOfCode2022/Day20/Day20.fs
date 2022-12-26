module Day20

// --- Day 20: Grove Positioning System ---

let preprocess (puzzle: string) : seq<string> = puzzle.TrimEnd().Split "\n"

let groveCoordinatePositions = [| 1000; 2000; 3000 |]

let inline moveItem idx steps len =
    let newIdx = (int64 idx) + steps
    let l = int64 len

    let result =
        match newIdx with
        | n when n <= 0L -> l + (n % l)
        | n when 0L < n && n <= l -> n
        | n when n > l -> n % l
        | _ -> failwith $"Unacceptable {nameof steps}: {steps}"

    int result

let mix lst =
    let length = (lst |> List.length) - 1

    (lst, [ 0..length ])
    ||> List.fold (fun state i ->
        let originalIdx, num = state |> List.find (fun (idx, _) -> idx = i)
        let currentIdx = state |> List.findIndex (fun (idx, _) -> idx = originalIdx)
        let newIdx = moveItem currentIdx num length

        if newIdx = length then
            [ (originalIdx, num) ] |> List.append (state |> List.removeAt currentIdx)
        else
            state |> List.removeAt currentIdx |> List.insertAt newIdx (originalIdx, num))

let getGroveCoordinates lst =
    let zeroIdx = lst |> Seq.findIndex (fun v -> v = 0L)

    groveCoordinatePositions
    |> Array.sumBy (fun i -> lst |> List.item ((i + zeroIdx) % lst.Length))

let multiMix times lst =
    (lst, [ 1..times ]) ||> List.fold (fun state _ -> mix state)

let solvePart1 (input: seq<string>) =
    input
    |> Seq.map int64
    |> Seq.toList
    |> List.indexed
    |> (multiMix 1)
    |> List.map snd
    |> getGroveCoordinates

let solvePart2 (input: seq<string>) =
    let decryptionKey = 811589153L

    input
    |> Seq.map (fun n -> (int64 n) * decryptionKey)
    |> Seq.toList
    |> List.indexed
    |> (multiMix 10)
    |> List.map snd
    |> getGroveCoordinates
