module Day13

open System
open System.IO
open Util.Extensions
open Util.Math
open Util.Patterns
open Util.Plumbing

type Atom =
| Digit of int
| List of Atom list

let parse file =
    let rec parseExp (atoms: Atom list) stack =
        function
        | "[" :: rest -> parseExp [] (atoms :: stack) rest
        | "]" :: rest -> parseExp (List (List.rev atoms) :: List.head stack) (List.tail stack) rest
        | Int32 n :: rest -> parseExp ((Digit n) :: atoms) stack rest 
        | [] -> List.exactlyOne atoms
        | _ -> failwith "Invalid input"

    let tuplize =
        function
        | [a; b] -> (a, b)
        | x -> failwithf "Malformed input: %A" x

    File.ReadAllLines file
    |> Seq.chunkBySize 3
    |> Seq.map (Seq.take 2 >> Seq.map (String.regMatches @"\[|]|\d+" >> parseExp [] []) >> Seq.toList >> tuplize)

let rec printable tree =
    match tree with
    | Digit n -> string n
    | List k -> List.map printable k |> String.concat "," |> sprintf "[%s]"

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
    Seq.mapi (fun i (a, b) -> (i + 1, a, b))
    >> Seq.filter (fun (_, a, b) -> compare a b <= 0)
    >> Seq.sumBy (fun (i, _, _) -> i)
    >> string

let solveGold input =
    ""

let Solver = chainSolver parse solveSilver solveGold
