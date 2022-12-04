module Patterns =
    let (|Integer|_|) (x: string) =
        try int x |> Some with :? System.FormatException -> None 

let juxtapose fs a =
    fs |> List.map (fun f -> f a)

let split delim str =
    (str: string).Split([|(delim: string)|], System.StringSplitOptions.None) |> Array.toSeq

let regSplit reg str =
    System.Text.RegularExpressions.Regex.Split (str, reg) |> Array.toSeq

let inputText =
    lazy (Array.get fsi.CommandLineArgs 1 |> System.IO.File.ReadAllText)

let inputLines =
    lazy (Array.get fsi.CommandLineArgs 1 |> System.IO.File.ReadAllLines |> List.ofArray)