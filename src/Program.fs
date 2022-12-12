open Util.Patterns
open Util.Plumbing

let solveDay day part file =
    let solver =
        match day with
        | "1" -> Day1.Solver
        | "2" -> Day2.Solver
        | "3" -> Day3.Solver
        | "4" -> Day4.Solver
        | "5" -> Day5.Solver
        | "6" -> Day6.Solver
        // | "7" -> Day7.Solver
        | "8" -> Day8.Solver
        | "9" -> Day9.Solver
        | "9m" -> Day9Mutable.Solver
        | "10" -> Day10.Solver
        | "10m" -> Day10Mutable.Solver
        | "11" -> Day11.Solver
        | "12" -> Day12.Solver
        //^ new days go here ^
        | _ -> failwith "Invalid day"
    
    let solution =
        match part with
        | "silver" -> solver file [Silver]
        | "gold" -> solver file [Gold]
        | "both" -> solver file [Silver; Gold]
        | _ -> failwithf "Invalid part; expected [silver|gold|both]"

    String.concat "\n" solution

[<EntryPoint>]
let main args =
    match args with
    | [| day; inputPath; part|] -> solveDay day part inputPath |> printfn "%s"
    | _ -> failwithf "Invalid arg string: %A" args
    0
