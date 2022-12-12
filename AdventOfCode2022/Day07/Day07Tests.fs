module Day07Tests

open NUnit.Framework
open Day07

let test =
    @"$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"

[<Test>]
let solvePart1 () =
    let input = test |> preprocess
    let actual = solvePart1 input
    Assert.AreEqual (95437u, actual)

[<Test>]
let solvePart2 () =
    let input = test |> preprocess
    let actual = solvePart2 input
    Assert.AreEqual (24933642, actual)
