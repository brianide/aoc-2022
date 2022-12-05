#load "util.fsx"
open System.Collections.Generic
open Util.Extensions
open Util.Patterns

let scenario = 
    // Line-height of the diagram
    let diagramHeight = (Util.inputLines.Value |> Seq.findIndex (fun x -> x.Length = 0)) - 2

    // Initialize mutable stacks
    let stacks =
        let count = (Util.inputLines.Value[0].Length + 1) / 4
        List.init count (fun _ -> new Stack<char>())

    Util.inputLines.Value[.. diagramHeight]
    |> Seq.rev
    |> Seq.iter (Seq.everyNth 1 4 >> Seq.iteri (fun i -> function ' ' -> () | c -> stacks[i].Push(c)))

    // Parse the list of instructions
    let moves =
        Util.inputLines.Value[diagramHeight + 3 ..]
        |> Seq.map (String.regGroups @"move (\d+) from (\d+) to (\d+)" >> function
        | [Integer count; Integer src; Integer dest] -> (count, src - 1, dest - 1)
        | g -> failwithf "Invalid input line: %A" g)

    {| Stacks = stacks; Moves = moves |}

let movementScheme =
    match Util.args[0] with
    | "part1" -> fun (count, src, dest) ->
        for _ in 1 .. count do
            scenario.Stacks[src].Pop() |> scenario.Stacks[dest].Push

    | "part2" -> fun (count, src, dest) ->
        Seq.init count (fun _ -> scenario.Stacks[src].Pop())
        |> Seq.rev
        |> Seq.iter scenario.Stacks[dest].Push

    | m -> failwithf "Invalid mode: %s" m

Seq.iter movementScheme scenario.Moves

scenario.Stacks
|> Seq.map (fun s -> s.Peek())
|> Seq.toArray
|> System.String
|> printfn "%s"