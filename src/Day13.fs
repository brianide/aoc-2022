module Day13

open System.IO
open Util.Extensions
open Util.Patterns
open Util.Plumbing

type Atom =
| Digit of int
| List of Atom list

let parse file =
    let rec parseExp atoms stack =
        function
        | "[" :: rest -> parseExp [] (atoms :: stack) rest
        | "]" :: rest -> parseExp (List (List.rev atoms) :: List.head stack) (List.tail stack) rest
        | Int32 n :: rest -> parseExp ((Digit n) :: atoms) stack rest 
        | [] -> List.exactlyOne atoms
        | _ -> failwith "Invalid input state"

    File.ReadAllLines file
    |> Seq.chunkBySize 3
    |> Seq.collect (Seq.take 2 >> Seq.map (String.regMatches @"\[|]|\d+" >> parseExp [] []))

let rec compare a b =
    match (a, b) with
    | Digit a, Digit b -> a - b
    | List a, List b ->
        Seq.map2 compare a b
        |> Seq.tryFind (fun n -> n <> 0)
        |> function
        | Some n -> n
        | None -> a.Length - b.Length
    | Digit a, List b -> compare (List [Digit a]) (List b)
    | List a, Digit b -> compare (List a) (List [Digit b]) 

let solveSilver =
    let tuplize =
        Seq.toList
        >> function
        | [a; b] -> (a, b)
        | _ -> failwith "Invalid input"

    Seq.chunkBySize 2
    >> Seq.map tuplize
    >> Seq.mapi (fun i (a, b) -> (i + 1, compare a b <= 0))
    >> Seq.filter snd
    >> Seq.sumBy fst
    >> string

let solveGold (input: seq<Atom>) =
    let divs = [List[List[Digit 2]]; List[List[Digit 6]]]
    let ordered = Seq.append divs input |> Seq.sortWith compare

    divs
    |> Seq.map (fun k -> Seq.findIndex ((=) k) ordered + 1)
    |> Seq.reduce (*) |> string

let Solver = chainSolver parse solveSilver solveGold
