module Day06

// --- Day 6: Tuning Trouble ---

let preprocess (puzzle: string) : string = puzzle.Trim ()

let findDistinct (numberOfDistinctCharacters: int) (input: string) : int =
    let uniq (str: string) : bool = str.Length = (Set str).Count

    let rec loop n i (str: string) : int =
        match str with
        | str when str.Length < n -> -1
        | str when uniq str[.. n - 1] -> i
        | _ -> loop n (i + 1) str[1..]

    loop numberOfDistinctCharacters numberOfDistinctCharacters input

let solvePart1: string -> int = findDistinct 4

let solvePart2: string -> int = findDistinct 14
