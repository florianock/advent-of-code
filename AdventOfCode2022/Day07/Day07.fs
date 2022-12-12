module Day07

let changeDirectory (path: string) =
    function
    | "/" -> "/"
    | ".." -> path[.. (path.LastIndexOf "/") - 1]
    | dir -> if path = "/" then path + dir else path + "/" + dir

let processLine (currentPath: string, directoryMap: Map<string, string[]>) (dirName, listing) =
    let newPath = changeDirectory currentPath dirName

    if not (directoryMap.ContainsKey newPath) then
        (newPath, directoryMap.Add (newPath, listing))
    else
        (newPath, directoryMap)

let preprocess (puzzle: string) : Map<string, string[]> =
    puzzle.TrimEnd().Split "$ cd "
    |> List.ofArray
    |> List.map (fun x -> x.Trim ())
    |> List.filter (fun x -> x <> "")
    |> List.map (fun s -> (s.Split "\n")[0], (s.Split "\n")[2..])
    |> List.fold processLine ("/", Map.empty)
    |> snd

let sumFiles (files: string[]) : int =
    files
    |> Array.map (fun s ->
        let split = s.Split ' '

        match split[0] with
        | "dir" -> 0
        | a -> int a)
    |> Array.sum

let getSize (directoryMap: Map<string, string[]>) (dir: string) : int =
    directoryMap
    |> Map.filter (fun k _ -> k.StartsWith dir)
    |> Map.map (fun _ -> sumFiles)
    |> Map.values
    |> Seq.sum

let solvePart1 (input: Map<string, string[]>) : int =
    input
    |> Map.map (fun k _ -> getSize input k)
    |> Map.values
    |> Seq.filter (fun x -> x <= 100_000)
    |> Seq.sum

let solvePart2 input =
    let sizeInformation = input |> Map.map (fun k _ -> getSize input k)

    let usedSpace = sizeInformation["/"]
    let availableSpace = 70_000_000 - usedSpace
    let requiredSpace = 30_000_000 - availableSpace

    sizeInformation.Values |> Seq.filter (fun s -> s >= requiredSpace) |> Seq.min
