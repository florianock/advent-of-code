module Day06

let preprocess (puzzle: string): string = puzzle.Trim()

let findDistinct (numberOfDistinctCharacters: int) (input: string): int =
    let uniq (str: string): bool = str.Length = (str.ToCharArray() |> Array.distinct |> Array.length)
    let rec loop n i (str: string): int = 
        match str with
        | str when str.Length < n -> -1
        | str when uniq str[.. n - 1] -> i
        | _ -> loop n (i + 1) str[1..]
    loop numberOfDistinctCharacters numberOfDistinctCharacters input

let solvePart1 = findDistinct 4

let solvePart2 = findDistinct 14
