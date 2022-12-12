module Util

open System.Text.RegularExpressions

module Plumbing =

    type ProblemPart = Silver | Gold

    let simpleSolver silver gold file =
        List.map <| function
        | Silver -> silver file
        | Gold -> gold file

    let chainSolver parse silver gold file =
        let input = parse file
        List.map <| function
        | Silver -> silver input
        | Gold -> gold input

module Patterns =

    let (|Int32|_|) (x: string) =
        try int x |> Some with :? System.FormatException -> None
    let (|Int64|_|) (x: string) =
        try int64 x |> Some with :? System.FormatException -> None
    let (|RegGroups|_|) patt str =
        let reg = Regex(patt)
        let mat = reg.Match(str)
        match mat.Success, mat.Groups with
        | true, groups -> Seq.tail groups |> Seq.map (fun n -> n.Value) |> Seq.toList |> Some
        | false, _ -> None
        

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
            (str: string).Split([|(delim: string)|], System.StringSplitOptions.None) |> Array.toList

        let regMatches patt str =
            let reg = Regex(patt)
            reg.Matches(str)
            |> Seq.map (fun x -> x.Value)
            |> Seq.toList

        let regGroups patt str =
            let reg = Regex(patt)
            reg.Match(str).Groups
            |> Seq.tail
            |> Seq.map (fun x -> x.Value)
            |> Seq.toList

        let regMatchGroups patt str =
            let reg = Regex(patt)
            reg.Matches(str)
            |> Seq.map (fun x -> x.Groups |> Seq.tail |> Seq.map (fun x -> x.Value) |> Seq.toList)

        let regSplit reg str =
            System.Text.RegularExpressions.Regex.Split (str, reg) |> Array.toSeq

    module Array2D =
        let inside grid (x, y) =
            x >= 0 && x < Array2D.length1 grid && y >= 0 && y < Array2D.length2 grid
        let coordSeq grid = seq {
            for y in 0 .. Array2D.length2 grid - 1 do
                for x in 0 .. Array2D.length1 grid - 1 do
                    yield (x, y)
        }


module Math =

    let inline gcd (a: ^a) (b: ^a) =
        let (zero: ^a) = LanguagePrimitives.GenericZero
        let rec compute (a: ^a) (b: ^a) =
            if b = zero then
                a
            else
                compute b (a % b)
        compute a b

    let inline lcm (a: ^a) (b: ^a) = a * b / gcd a b


module Collections =

    module Queue =

        type Queue<'a> = Queue of 'a list * 'a list

        let empty = Queue ([], [])

        let singleton e = Queue ([], [e])

        let isEmpty = function Queue ([], []) -> true | _ -> false

        let enqueue e q =
            match q with Queue (ins, outs) -> Queue(e :: ins, outs)
        
        let dequeue =
            function
            | Queue ([], []) -> failwith "No elements remaining"
            | Queue (ins, e :: outs) -> (e, Queue (ins, outs))
            | Queue (ins, []) ->
                let outs = List.rev ins
                (List.head outs, Queue ([], List.tail outs))