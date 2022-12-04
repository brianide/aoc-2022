#load "util.fsx"
open System.Text.RegularExpressions
open Util.Patterns

let parseLine line = 
    let reg = Regex(@"^(\d+)-(\d+),(\d+)-(\d+)$")

    reg.Match(line).Groups
    |> Seq.map (fun x -> x.Value)
    |> Seq.toList
    |> function
    | [_; Integer a; Integer b; Integer x; Integer y] -> (a, b), (x, y)
    | g -> failwithf "Invalid input line: %s ==> %A" line g

let checkContained ((a, b), (x, y)) =
    a <= x && b >= y || x <= a && y >= b

let checkOverlap ((a, b), (x, y)) =
    a <= y && x <= b

let input = Seq.map parseLine Util.inputLines.Value

Seq.filter checkContained input
|> Seq.length
|> printfn "Part 1: %i"

Seq.filter checkOverlap input
|> Seq.length
|> printfn "Part 2: %i"