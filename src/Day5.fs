module Day5

open System.IO
open Util.Extensions
open Util.Patterns
open Util.Plumbing

let parse file =
    let lines = File.ReadAllLines file

    // Line-height of the diagram
    let diagramHeight = (lines |> Array.findIndex (fun (x: string) -> x.Length = 0)) - 2

    // Parse diagram
    let stacks =
        lines[.. diagramHeight]
        |> Seq.map (Seq.everyNth 1 4)
        |> Seq.transpose
        |> Seq.map (Seq.skipWhile ((=) ' ') >> Seq.toList)
        |> Seq.toArray

    // Parse the list of instructions
    let moves =
        lines[diagramHeight + 3 ..]
        |> Seq.map (String.regGroups @"move (\d+) from (\d+) to (\d+)" >> function
        | [Int32 count; Int32 src; Int32 dest] -> (count, src - 1, dest - 1)
        | g -> failwithf "Invalid input line: %A" g)

    (stacks, moves)
    
let moveSilver (stacks: list<_> array) (count, src, dest) =
    for _ in 1 .. count do
        let item = List.head stacks[src]
        stacks[src] <- List.tail stacks[src]
        stacks[dest] <- item :: stacks[dest]

let moveGold (stacks: list<_> array) (count, src, dest) =
    let items = stacks[src] |> List.take count
    stacks[src] <- List.skip count stacks[src]
    stacks[dest] <- List.append items stacks[dest]

let solve scheme file =
    let (stacks, moves) = parse file
    Seq.iter (scheme stacks) moves
    stacks |> Seq.map (List.head >> string) |> String.concat ""

let Solvers = simpleSolver (solve moveSilver) (solve moveGold)