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
    ("12r", Day12.Renderer, "Hill Climbing Algorithm (Render)")
    ("13", Day13.Solver, "Distress Signal")
    ("14", Day14.Solver, "Regolith Reservoir")
    //^ new days go here ^
|]

let solveDay day args =
    let solver =
        Seq.tryFind (fun (key, _, _) -> key = day) solvers
        |> function
        | Some (_, s, _) -> s
        | _ ->
            solvers
            |> Seq.map (fun (key, _, name) -> sprintf "%3s: %s" key name)
            |> String.concat "\n"
            |> failwithf "Invalid day; valid days are:\n%s" 
    
    solver args

[<EntryPoint>]
let main args =
    match args |> Array.toList with
    | day :: more -> solveDay day more |> printfn "%s"
    | _ -> failwithf "Invalid arg string: %A" args
    0
