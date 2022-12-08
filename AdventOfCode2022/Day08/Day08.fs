module Day08

// --- Day 8: Treetop Tree House ---

let preprocess (puzzle: string) =
    puzzle.TrimEnd().Split ("\n")
    |> Array.map (fun s -> s |> Seq.toList |> List.map (fun c -> System.Int32.Parse (c.ToString ())))
    |> List.ofArray

let lookAhead i j (matrix: int list list) = matrix[i][j + 1 ..]

let lookBehind i j (matrix: int list list) = matrix[i][.. j - 1] |> List.rev

let inAllDirections i j matrix matrix' func =
    (lookBehind j i matrix' |> func,
     lookAhead i j matrix |> func,
     lookAhead j i matrix' |> func,
     lookBehind i j matrix |> func)

let solvePart1 (input: int list list) : int =
    let input' = List.transpose input

    input
    |> List.mapi (fun i row ->
        if i = 0 || i = input.Length - 1 then
            Array.create row.Length true |> List.ofArray
        else
            row
            |> List.mapi (fun j cell ->
                if j = 0 || j = row.Length - 1 then
                    true
                else
                    let north, east, south, west =
                        inAllDirections i j input input' (List.forall (fun n -> n < cell))

                    north || east || south || west)
            |> List.filter id)
    |> List.fold (fun s r -> s + r.Length) 0

let solvePart2 (input: int list list) : int =
    let input' = List.transpose input

    input
    |> List.mapi (fun i row ->
        if i = 0 || i = input.Length - 1 then
            Array.create row.Length 0 |> List.ofArray
        else
            row
            |> List.mapi (fun j cell ->
                if j = 0 || j = row.Length - 1 then
                    0
                else
                    let north, east, south, west =
                        inAllDirections i j input input' (List.tryFindIndex (fun n -> n >= cell))

                    [ north |> Option.fold (fun _ v -> v + 1) i
                      east |> Option.fold (fun _ v -> v + 1) (row.Length - 1 - j)
                      south |> Option.fold (fun _ v -> v + 1) (input.Length - 1 - i)
                      west |> Option.fold (fun _ v -> v + 1) j ]
                    |> List.fold (*) 1))
    |> List.map List.max
    |> List.max
