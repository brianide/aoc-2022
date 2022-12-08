module Day1

open System.IO
open Util.Extensions
open Util.Patterns

let parse file =
    let rec handleLines total elves = function
    | Int32 num :: rest -> handleLines (total + num) elves rest    
    | "" :: rest -> handleLines 0 (total :: elves) rest
    | [] -> elves
    | x -> failwithf "Invalid input state: %A" x

    file |> File.ReadAllLines |> Array.toList |> handleLines 0 []

let solveSilver = parse >> Seq.max
let solveGold = parse >> Seq.sortDescending >> Seq.take 3 >> Seq.sum

let Solvers = {| Gold = solveGold; Silver = solveSilver |}