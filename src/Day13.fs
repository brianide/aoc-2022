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
    let tuplize =
        Seq.toList
        >> function
        | [a; b] -> (a, b)
        | _ -> failwith "Invalid input"

    Seq.chunkBySize 2
    >> Seq.map tuplize
    >> Seq.mapi (fun i (a, b) -> (i + 1, a, b))
    >> Seq.filter (fun (_, a, b) -> compare a b <= 0)
    >> Seq.sumBy (fun (i, _, _) -> i)
    >> string

let solveGold (input: seq<Atom>) =
    let divA = List[List[Digit 2]]
    let divB = List[List[Digit 6]]

    let ordered = Seq.append [divA; divB] input |> Seq.sortWith compare

    (Seq.findIndex ((=) divA) ordered + 1) * (Seq.findIndex ((=) divB) ordered + 1) |> string

let Solver = chainSolver parse solveSilver solveGold
