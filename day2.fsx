#load "util.fsx"
open Util.Extensions

type Hand = Rock | Paper | Scissors
type Outcome = Win | Lose | Draw

let scoreMatch (mine, theirs) =
    let handScore =
        match mine with
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3

    let outcomeScore =
        let outcome =
            match (mine, theirs) with
            | Rock, Scissors -> Win
            | Scissors, Paper -> Win
            | Paper, Rock -> Win
            | a, b when a = b -> Draw
            | _ -> Lose

        match outcome with
        | Win -> 6
        | Draw -> 3
        | Lose -> 0

    handScore + outcomeScore

let parseLine line =
    let (|Hand|_|) = function
    | "A" | "X" -> Some Rock
    | "B" | "Y" -> Some Paper
    | "C" | "Z" -> Some Scissors
    | _ -> None

    line |> String.split " " |> Seq.toList
    |> function
    | [Hand theirs; Hand mine] -> mine, theirs
    | _ -> failwithf "Invalid input line: %s" line

let parseLineCorrectly line =
    let (|Hand|_|) = function
    | "A" -> Some Rock
    | "B" -> Some Paper
    | "C" -> Some Scissors
    | _ -> None

    let (|Outcome|_|) = function
    | "X" -> Some Lose
    | "Y" -> Some Draw
    | "Z" -> Some Win
    | _ -> None

    let calcPlay theirs outcome =
        match (theirs, outcome) with
        | x, Draw -> x
        | Rock, Win -> Paper
        | Rock, Lose -> Scissors
        | Paper, Win -> Scissors
        | Paper, Lose -> Rock
        | Scissors, Win -> Rock
        | Scissors, Lose -> Paper

    line |> String.split " " |> Seq.toList
    |> function
    | [Hand theirs; Outcome outcome] -> calcPlay theirs outcome, theirs
    | _ -> failwithf "Invalid input line: %s" line

Util.inputLines.Value
|> Seq.map (fun line -> parseLine line |> scoreMatch, parseLineCorrectly line |> scoreMatch)
|> Seq.sum2
|> printfn "%A"