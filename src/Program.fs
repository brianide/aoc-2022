open Util.Plumbing

let solvers = [|
    ("1", Day1.Solver, "Calorie Counting")
    ("2", Day2.Solver, "Rock Paper Scissors")
    ("3", Day3.Solver, "Rucksack Reorganization")
    ("4", Day4.Solver, "Camp Cleanup")
    ("5", Day5.Solver, "Supply Sacks")
    ("6", Day6.Solver, "Tuning Trouble")
    ("7", Day7.Solver, "No Space Left On Device")
    ("8", Day8.Solver, "Treetop Tree House")
    ("9", Day9.Solver, "Rope Bridge")
    ("9m", Day9Mutable.Solver, "Rope Bridge (Mutable)")
    ("10", Day10.Solver, "Cathode-Ray Tube")
    ("10m", Day10Mutable.Solver, "Cathode-Ray Tube (Mutable)")
    ("11", Day11.Solver, "Monkey in the Middle")
    ("12", Day12.Solver, "Hill Climbing Algorithm")
    ("13", Day13.Solver, "Distress Signal")
    //^ new days go here ^
|]

let solveDay day part file =
    let solver =
        Seq.tryFind (fun (key, _, _) -> key = day) solvers
        |> function
        | Some (_, s, _) -> s
        | _ ->
            solvers
            |> Seq.map (fun (key, _, name) -> sprintf "%3s: %s" key name)
            |> String.concat "\n"
            |> failwithf "Invalid day; valid days are:\n%s" 
    
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
