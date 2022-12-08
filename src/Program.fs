open Util.Patterns

let getSolver day part =
    let day =
        match day with
        | 1 -> Day1.Solvers
        | 8 -> Day8.Solvers
        | n -> failwithf "Invalid day: %i" n
    
    match part with
    | Silver -> day.Silver
    | Gold -> day.Gold

[<EntryPoint>]
let main args =
    match args with
    | [|Int32 day; inputPath; Part part|] -> getSolver day part inputPath |> printfn "%s"
    | _ -> failwithf "Invalid args: %A" args
    0