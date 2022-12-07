#load "util.fsx"
open System.Collections.Generic
open Util.Extensions
open Util.Patterns

type FSEntry =
| File of string * int64
| Directory of string

let () =
    let flatten = Seq.rev >> String.concat "/"

    let rec parseCommand path dirs lines =
        match lines with
        | ["$"; "cd"; "/"] :: rest -> parseCommand ["root"] dirs rest
        | ["$"; "cd"; ".."] :: rest -> parseCommand (List.tail path) dirs rest
        | ["$"; "cd"; dir] :: rest -> parseCommand (dir :: path) dirs rest
        | ["$"; "ls"] :: rest -> parseEntries path dirs rest []
        | [] -> dirs
        | x -> failwithf "Invalid state: %A" (flatten path, dirs, x)

    and parseEntries path dirs lines contents =
        match lines with
        | [Integer size; name] :: rest -> File (name, size) :: contents |> parseEntries path dirs rest
        | ["dir"; name] :: rest -> Directory (name :: path |> flatten) :: contents |> parseEntries path dirs rest
        | rest -> parseCommand path (Map.add (flatten path) contents dirs) rest

    let calcSizes table =
        let rec handle curr total dirs entries =
            match entries with
            | File (_, size) :: rest -> handle curr (total + size) dirs rest
            | Directory name :: rest -> let dirs = handle name 0L dirs (Map.find name table) in handle curr (total + Map.find name dirs) dirs rest
            | [] -> Map.add curr total dirs

        Map.find "root" table
        |> handle "root" 0 Map.empty

    let sizes = 
        Util.inputLines.Value
        |> Seq.map (String.split " " >> Seq.toList)
        |> Seq.toList
        |> parseCommand [] Map.empty
        |> calcSizes
    
    let silver =
        sizes |> Map.values |> Seq.filter (fun n -> n <= 100000) |> Seq.sum
        
    let gold =
        let needed = 30000000L - (70000000L - (Map.find "root" sizes))
        sizes |> Map.values |> Seq.sort |> Seq.find (fun n -> n >= needed)

    printfn "%A" (silver, gold)