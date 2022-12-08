module Util

module Patterns =
    let (|Int32|_|) (x: string) =
        try int x |> Some with :? System.FormatException -> None
    let (|Int64|_|) (x: string) =
        try int64 x |> Some with :? System.FormatException -> None

    type Part = Silver | Gold
    let (|Part|_|) = function
    | "silver" -> Some Silver
        | "gold" -> Some Gold
    | _ -> None

module Extensions =
    module Seq =
        let everyNth start skip =
            Seq.mapi (fun i e -> if i >= start && (i - start) % skip = 0 then Some e else None)
            >> Seq.choose id

        let sum2 seq =
            Seq.fold (fun (aSum, bSum) (a, b) -> aSum + a, bSum + b) (0, 0) seq

        let eachTail (input: seq<_>) = seq {
            let mutable head = Seq.tail input
            while Seq.length head > 0 do
                yield head
                head <- Seq.tail head
        }
    
    module String =
        let split delim str =
            (str: string).Split([|(delim: string)|], System.StringSplitOptions.None) |> Array.toSeq

        let regMatches patt str =
            let reg = System.Text.RegularExpressions.Regex(patt)
            reg.Matches(str)
            |> Seq.map (fun x -> x.Value)
            |> Seq.toList

        let regGroups patt str =
            let reg = System.Text.RegularExpressions.Regex(patt)
            reg.Match(str).Groups
            |> Seq.tail
            |> Seq.map (fun x -> x.Value)
            |> Seq.toList

        let regSplit reg str =
            System.Text.RegularExpressions.Regex.Split (str, reg) |> Array.toSeq