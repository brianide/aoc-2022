module Day9

open System.IO
open Util.Extensions
open Util.Patterns

let parse file =
    let (|Dir|_|) = function
    | "R" -> Some (1, 0)
    | "L" -> Some (-1, 0)
    | "U" -> Some (0, -1)
    | "D" -> Some (0, 1)
    | _ -> None

    File.ReadAllLines file
    |> Seq.map (String.split " " >> Seq.toList)
    |> Seq.map (function
    | [Dir dir; Int32 dist] -> Seq.init dist (fun _ -> dir)
    | x -> failwithf "Invalid input: %A" x)
    |> Seq.collect id

let mag = function
| n when n > 0 -> 1
| n when n < 0 -> -1
| _ -> 0

let move ((hx, hy), (tx, ty), visited) (x, y) =
    let (hx, hy) = (hx + x, hy + y)
    let (tx, ty) =
        let (dx, dy) = (hx - tx, hy - ty)
        if abs dx > 1 || abs dy > 1 then
            (tx + mag dx, ty + mag dy)
        else
            (tx, ty)

    let visited = Set.add (tx, ty) visited

    ((hx, hy), (tx, ty), visited)

let solveSilver file =
    parse file
    |> Seq.fold move ((0, 0), (0, 0), Set.singleton (0, 0))
    |> fun (_, _, visited) -> Set.count visited
    |> printfn "%A"
    ""

let solveGold file =
    parse file
    |> ignore
    ""

let Solvers = (solveSilver, solveGold)