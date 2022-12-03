#load "util.fsx"
open Util

type Hand = Rock | Paper | Scissors
type Outcome = Win | Lose | Draw

let scoreHand = function
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let scoreMatch mine theirs =
    let outcomeScore =
        let outcome =
            match (mine, theirs) with
            | Rock, Rock -> Draw
            | Rock, Paper -> Lose
            | Rock, Scissors -> Win
            | Paper, Rock -> Win
            | Paper, Paper -> Draw
            | Paper, Scissors -> Lose
            | Scissors, Rock -> Lose
            | Scissors, Paper -> Win
            | Scissors, Scissors -> Draw
        
        match outcome with
        | Win -> 6
        | Draw -> 3
        | Lose -> 0

    scoreHand mine + outcomeScore

let parseLine line =
    let (|Hand|_|) = function
    | "A" | "X" -> Some Rock
    | "B" | "Y" -> Some Paper
    | "C" | "Z" -> Some Scissors
    | _ -> None

    match split " " line |> Seq.toList with
    | [Hand theirs; Hand mine] -> (mine, theirs)
    | _ -> failwithf "Invalid input line: %s" line

inputLines.Value
|> Seq.map parseLine
|> Seq.map (fun (mine, theirs) -> scoreMatch mine theirs)
|> Seq.sum
|> printfn "%i"

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

    match split " " line |> Seq.toList with
    | [Hand theirs; Outcome outcome] -> (calcPlay theirs outcome, theirs)
    | _ -> failwithf "Invalid input line: %s" line

inputLines.Value
|> Seq.map parseLineCorrectly
|> Seq.map (fun (mine, theirs) -> scoreMatch mine theirs)
|> Seq.sum
|> printfn "%i"