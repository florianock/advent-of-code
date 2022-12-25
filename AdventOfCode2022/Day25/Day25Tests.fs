module Day25Tests

open NUnit.Framework
open Day25

let test =
    @"1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122
"

[<TestFixture>]
type TestDay25 () =

    [<Test>]
    member this.solvePart1() =
        let input = test |> preprocess
        let actual = solvePart1 input
        Assert.AreEqual ("2=-1=0", actual)

    [<Test>]
    member this.solvePart2() =
        let input = test |> preprocess
        let actual = solvePart2 input
        Assert.AreEqual ("0", actual)

[<TestFixture>]
type SnafuTests () =

    [<TestCase(1, "1")>]
    [<TestCase(2, "2")>]
    [<TestCase(3, "1=")>]
    [<TestCase(4, "1-")>]
    [<TestCase(5, "10")>]
    [<TestCase(6, "11")>]
    [<TestCase(7, "12")>]
    [<TestCase(8, "2=")>]
    [<TestCase(9, "2-")>]
    [<TestCase(10, "20")>]
    [<TestCase(11, "21")>]
    [<TestCase(12, "22")>]
    [<TestCase(13, "1==")>]
    [<TestCase(14, "1=-")>]
    [<TestCase(15, "1=0")>]
    [<TestCase(16, "1=1")>]
    [<TestCase(17, "1=2")>]
    [<TestCase(18, "1-=")>]
    [<TestCase(19, "1--")>]
    [<TestCase(20, "1-0")>]
    [<TestCase(976, "2=-01")>]
    [<TestCase(2022, "1=11-2")>]
    [<TestCase(4890, "2=-1=0")>]
    [<TestCase(12345, "1-0---0")>]
    [<TestCase(314159265, "1121-1110-1=0")>]
    member this.TestSnafuToDecimal(expected, snafu) =
        let actual = snafuToDecimal snafu
        Assert.AreEqual (expected, actual)

    [<TestCase("1", 1)>]
    [<TestCase("2", 2)>]
    [<TestCase("1=", 3)>]
    [<TestCase("1-", 4)>]
    [<TestCase("10", 5)>]
    [<TestCase("11", 6)>]
    [<TestCase("12", 7)>]
    [<TestCase("2=", 8)>]
    [<TestCase("2-", 9)>]
    [<TestCase("20", 10)>]
    [<TestCase("1=0", 15)>]
    [<TestCase("1-0", 20)>]
    [<TestCase("2=-01", 976)>]
    [<TestCase("1=11-2", 2022)>]
    [<TestCase("2=-1=0", 4890)>]
    [<TestCase("1-0---0", 12345)>]
    [<TestCase("1121-1110-1=0", 314159265)>]
    member this.TestDecimalToSnafu(expected, dec) =
        let actual = decimalToSnafu dec
        Assert.AreEqual (expected, actual)
