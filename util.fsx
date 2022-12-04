module Patterns =
    let (|Integer|_|) (x: string) =
        try int x |> Some with :? System.FormatException -> None 

module Extensions =
    module Seq =
        let sum2 seq =
            Seq.fold (fun (aSum, bSum) (a, b) -> aSum + a, bSum + b) (0, 0) seq
    
    module String =
        let split delim str =
            (str: string).Split([|(delim: string)|], System.StringSplitOptions.None) |> Array.toSeq

        let regGroups patt str =
            let reg = System.Text.RegularExpressions.Regex(patt)
            reg.Match(str).Groups
            |> Seq.tail
            |> Seq.map (fun x -> x.Value)
            |> Seq.toList

        let regSplit reg str =
            System.Text.RegularExpressions.Regex.Split (str, reg) |> Array.toSeq

let juxtapose fs a =
    fs |> List.map (fun f -> f a)

let inputText =
    lazy (Array.get fsi.CommandLineArgs 1 |> System.IO.File.ReadAllText)

let inputLines =
    lazy (Array.get fsi.CommandLineArgs 1 |> System.IO.File.ReadAllLines |> List.ofArray)