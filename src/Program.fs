open Util.Patterns
open Util.Plumbing

let solveDay day part file =
    let day =
        match day with
        | "1" -> Day1.Solvers
        | "2" -> Day2.Solvers
        | "3" -> Day3.Solvers
        | "4" -> Day4.Solvers
        | "5" -> Day5.Solvers
        | "6" -> Day6.Solvers
        // | "7" -> Day7.Solvers
        | "8" -> Day8.Solvers
        | "9" -> Day9.Solvers
        | "9m" -> Day9Mutable.Solvers
        | "10" -> Day10.Solvers
        | "11" -> Day11.Solvers
        | "12" -> Day12.Solvers
        //^ new days go here ^
        | _ -> failwith "Invalid day"
    
    let solution =
        match part with
        | "silver" -> day file [Silver]
        | "gold" -> day file [Gold]
        | "both" -> day file [Silver; Gold]
        | _ -> failwithf "Invalid part; expected [silver|gold|both]"

    String.concat "\n" solution

[<EntryPoint>]
let main args =
    match args with
    | [| day; inputPath; part|] -> solveDay day part inputPath |> printfn "%s"
    | _ -> failwithf "Invalid arg string: %A" args
    0
