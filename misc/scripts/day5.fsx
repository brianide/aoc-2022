#load "util.fsx"
open Util.Extensions
open Util.Patterns

let scenario = 
    // Line-height of the diagram
    let diagramHeight = (Util.inputLines.Value |> Seq.findIndex (fun x -> x.Length = 0)) - 2

    // Parse diagram
    let stacks =
        Util.inputLines.Value[.. diagramHeight]
        |> Seq.map (Seq.everyNth 1 4)
        |> Seq.transpose
        |> Seq.map (Seq.skipWhile ((=) ' ') >> Seq.toList)
        |> Seq.toArray

    // Parse the list of instructions
    let moves =
        Util.inputLines.Value[diagramHeight + 3 ..]
        |> Seq.map (String.regGroups @"move (\d+) from (\d+) to (\d+)" >> function
        | [Integer count; Integer src; Integer dest] -> (count, src - 1, dest - 1)
        | g -> failwithf "Invalid input line: %A" g)

    (stacks, moves)

let moveSilver (stacks: list<_> array) (count, src, dest) =
    for _ in 1 .. count do
        let item = List.head stacks[src]
        stacks[src] <- List.tail stacks[src]
        stacks[dest] <- item :: stacks[dest]
    
    stacks

let moveGold (stacks: list<_> array) (count, src, dest) =
    let items = stacks[src] |> List.take count
    stacks[src] <- List.skip count stacks[src]
    stacks[dest] <- List.append items stacks[dest]
    stacks

let solve (stacks, moves) scheme =
    moves
    |> Seq.fold scheme (Array.copy stacks)
    |> Array.map (List.head >> string)
    |> String.concat ""

[moveSilver; moveGold]
|> List.map (solve scenario)
|> printfn "%A"