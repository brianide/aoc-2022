module Day7

open System.IO
open Util.Extensions
open Util.Patterns
open Util.Plumbing

let parse file =
    let rec handleCommand path table =
        function
        | ["$"; "cd"; "/"] :: rest -> handleCommand ["root"] table rest
        | ["$"; "cd"; ".."] :: rest -> handleCommand (List.tail path) table rest
        | ["$"; "cd"; dir] :: rest -> handleCommand (dir :: path) table rest
        | ["$"; "ls"] :: rest -> handleEntries path table 0L rest
        | [] -> table
        | x -> failwithf "Invalid state: %A" (path, x)

    and handleEntries path table total =
        function
        | [Int64 size; _] :: rest -> handleEntries path table (total + size) rest
        | ["dir"; _] :: rest -> handleEntries path table total rest
        | rest ->
            let tab =
                Seq.tails path
                |> Seq.map List.ofSeq
                |> Seq.fold (fun tab p -> Map.add p ((Map.findOrDefault p 0L tab) + total) tab) table
            handleCommand path tab rest

    File.ReadAllLines file
    |> Seq.map (String.split " ")
    |> Seq.toList
    |> handleCommand ["root"] Map.empty

let silver =
    Map.values
    >> Seq.filter (fun n -> n <= 100000L)
    >> Seq.sum
    >> string
    
let gold input =
    let needed = 30000000L - 70000000L + Map.find ["root"] input

    Map.values input
    |> Seq.sort
    |> Seq.find (fun n -> n >= needed)
    |> string

let Solver = chainSolver parse silver gold