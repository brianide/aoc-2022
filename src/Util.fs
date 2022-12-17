module Util

open System.Text.RegularExpressions


module Plumbing =
    let private (|Parts|) silver gold =
        function
        | "silver" -> [silver]
        | "gold" -> [gold]
        | "both" -> [silver; gold]
        | _ -> failwith "Invalid part; expected [silver|gold|both]"

    let simpleSolver silver gold =
        function
        | [file; Parts silver gold p] -> Seq.map (fun f -> f file) p |> String.concat "\n"
        | _ -> failwithf "Invalid args; please specifiy an input file and part"

    let chainSolver parse silver gold =
        function
        | [file; Parts silver gold p] ->
            let input = parse file
            Seq.map (fun f -> f input) p |> String.concat "\n"
        | _ -> failwith "Invalid args"

    let renderer func =
        function
        | infile :: outdir :: prefix :: args -> func infile outdir prefix args
        | _ -> failwith "Invalid args; please specify an input file, output dir, and prefix"


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

    module Map =
        let findOrDefault k v map =
            match Map.tryFind k map with
            | Some n -> n
            | None -> v

    module Seq =
        let everyNth start skip =
            Seq.mapi (fun i e -> if i >= start && (i - start) % skip = 0 then Some e else None)
            >> Seq.choose id

        let boundsBy f (source: seq<_>) =
            use e = source.GetEnumerator()
            if not <| e.MoveNext() then
                invalidArg "source" "Sequence is empty"
            let mutable low, hi = e.Current, e.Current
            while e.MoveNext() do
                let curr = e.Current
                if f curr > f hi then hi <- curr
                if f curr < f low then low <- curr
            (low, hi)

        let bounds source = boundsBy id source

        let sum2 seq =
            Seq.fold (fun (aSum, bSum) (a, b) -> aSum + a, bSum + b) (0, 0) seq

        let tails col =
            Seq.scan (fun a _ -> Seq.tail a) col col

        let pairs source =
            tails source
            |> Seq.map Seq.toList
            |> Seq.collect (function
            | head :: rest -> (Seq.map (fun e -> (head, e)) rest)
            | _ -> [])

    module List =
        let rec permutations = function
            | [] -> seq [List.empty]
            | x :: xs -> Seq.collect (insertions x) (permutations xs)
        and insertions x = function
            | [] -> [[x]]
            | y :: ys as xs -> (x :: xs) :: (List.map (fun x -> y :: x) (insertions x ys)) 
    
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
            let reg = Regex(patt, RegexOptions.Multiline)
            reg.Matches(str)
            |> Seq.map (fun x -> x.Groups |> Seq.tail |> Seq.map (fun x -> x.Value) |> Seq.toList)

        let regSplit reg str =
            System.Text.RegularExpressions.Regex.Split (str, reg) |> Array.toSeq

    module Array2D =
        let isInside grid (i, j) =
            i >= 0 && i < Array2D.length1 grid && j >= 0 && j < Array2D.length2 grid

        let coordSeq grid = seq {
            for i in 0 .. Array2D.length1 grid - 1 do
                for j in 0 .. Array2D.length2 grid - 1 do
                    yield (i, j)
        }

    module Tuple2 =
        let map f (a, b) = (f a, f b)

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


let formatGrid fmtVal limit grid =
    let limit = if limit <= 0 then System.Int32.MaxValue else limit

    seq {0 .. min (limit) (Array2D.length1 grid - 1)}
    |> Seq.map (fun r -> grid[r, *] |> Seq.map fmtVal |> String.concat "") 
    |> String.concat "\n"