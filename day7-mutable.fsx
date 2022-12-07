#load "util.fsx"
open System.Collections.Generic
open Util.Extensions
open Util.Patterns

let getSizeTable lines =
    let table = Dictionary<string list, int64>()

    let rec handleCommand path = function
    | ["$"; "cd"; "/"] :: rest -> handleCommand ["root"] rest
    | ["$"; "cd"; ".."] :: rest -> handleCommand (List.tail path) rest
    | ["$"; "cd"; dir] :: rest -> handleCommand (dir :: path) rest
    | ["$"; "ls"] :: rest -> handleEntries path 0L rest
    | [] -> ()
    | x -> failwithf "Invalid state: %A" (path, x)

    and handleEntries path total = function
    | [Int64 size; _] :: rest -> handleEntries path (total + size) rest
    | ["dir"; _] :: rest -> handleEntries path total rest
    | rest ->
        table[path] <- total
        Seq.eachTail path
        |> Seq.map Seq.toList
        |> Seq.iter (fun p -> table[p] <- table[p] + total)
        handleCommand path rest

    handleCommand ["root"] lines
    table |> Seq.map (|KeyValue|) |> Map.ofSeq

let sizes = 
    Util.inputLines.Value
    |> List.map (String.split " " >> Seq.toList)
    |> getSizeTable

let silver =
    sizes |> Map.values |> Seq.filter (fun n -> n <= 100000) |> Seq.sum
    
let gold =
    let needed = 30000000L - 70000000L + Map.find ["root"] sizes
    sizes |> Map.values |> Seq.sort |> Seq.find (fun n -> n >= needed)

printfn "%A" (silver, gold)