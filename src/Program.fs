open Util.Patterns

let getSolver day part =
    let day =
        match day with
        | "1" -> Day1.Solvers
        | "2" -> Day2.Solvers
        | "3" -> Day3.Solvers
        | "4" -> Day4.Solvers
        | "5" -> Day5.Solvers
        | "6" -> Day6.Solvers
        | "7" -> Day7.Solvers
        | "8" -> Day8.Solvers
        | "9" -> Day9.Solvers
        | "9m" -> Day9Mutable.Solvers
        | "10" -> Day10.Solvers
        | _ -> failwith "Invalid day"
    
    match part with
    | "silver" -> fst day
    | "gold" -> snd day
    | "both" -> fun file -> fst day file + "\n" + snd day file
    | _ -> failwithf "Invalid part; expected [silver|gold|both]"

[<EntryPoint>]
let main args =
    match args with
    | [| day; inputPath; part|] -> getSolver day part inputPath |> printfn "%s"
    | _ -> failwithf "Invalid arg string: %A" args
    0