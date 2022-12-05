#load "util.fsx"
open System.Collections.Generic
open Util.Extensions
open Util.Patterns

let scenario = 
    // Line index where the diagram ends
    let splitPoint = (Util.inputLines.Value |> Seq.findIndex (fun x -> x.Length = 0)) - 2

    // Parse the stack diagram
    let diagram =
        Util.inputLines.Value[.. splitPoint]
        |> Seq.map (String.regMatches @"(   |\[[A-Z]\])( |$)")
        |> Seq.map (List.map (function n when n.Trim().Length > 0 -> Some n[1] | _ -> None))
        |> Seq.toList

    // Initialize mutable stacks
    let stacks = List.init diagram[0].Length (fun _ -> new Stack<char>(diagram.Length))

    // Read from the diagram into the mutable stacks
    diagram
    |> Seq.rev
    |> Seq.iter (List.iteri (fun i -> function Some c -> stacks[i].Push(c) | None -> ()))

    // Parse the list of instructions
    let moves =
        Util.inputLines.Value[splitPoint + 3 ..]
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