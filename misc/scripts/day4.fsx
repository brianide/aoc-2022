#load "util.fsx"
open Util.Patterns
open Util.Extensions

let input =
    let parseLine line =
        line
        |> String.regGroups @"^(\d+)-(\d+),(\d+)-(\d+)$"
        |> function
        | [Integer a; Integer b; Integer x; Integer y] -> (a, b), (x, y)
        | g -> failwithf "Invalid input line: %s ==> %A" line g

    Util.inputLines.Value |> Seq.map parseLine

let checkContained ((a, b), (x, y)) =
    a <= x && b >= y || x <= a && y >= b

let checkOverlap ((a, b), (x, y)) =
    a <= y && x <= b

Seq.filter checkContained input
|> Seq.length
|> printfn "Part 1: %i"

Seq.filter checkOverlap input
|> Seq.length
|> printfn "Part 2: %i"