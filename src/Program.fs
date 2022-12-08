open Util.Patterns

let getSolver day part =
    let day =
        match day with
        | 1 -> Day1.Solvers
        | 2 -> Day2.Solvers
        | 3 -> Day3.Solvers
        | 4 -> Day4.Solvers
        | 5 -> Day5.Solvers
        | 6 -> Day6.Solvers
        | 7 -> Day7.Solvers
        | 8 -> Day8.Solvers
        | n -> failwithf "Invalid day: %i" n
    
    day |> match part with Silver -> fst | Gold -> snd

[<EntryPoint>]
let main args =
    match args with
    | [|Int32 day; inputPath; Part part|] -> getSolver day part inputPath |> printfn "%s"
    | _ -> failwithf "Invalid args: %A" args
    0