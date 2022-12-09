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
    | [Dir dir; Int32 dist] -> seq { for _ in 1 .. dist -> dir }
    | x -> failwithf "Invalid input: %A" x)
    |> Seq.collect id

let mag = function
| n when n > 0 -> 1
| n when n < 0 -> -1
| _ -> 0

let moveKnot (hx, hy) (kx, ky) =
        let (dx, dy) = (hx - kx, hy - ky)
        if abs dx > 1 || abs dy > 1 then
            (kx + mag dx, ky + mag dy)
        else
            (kx, ky)

let moveRope ((hx, hy), (tx, ty), visited) (x, y) =
    let (hx, hy) = (hx + x, hy + y)
    let (tx, ty) = moveKnot (hx, hy) (tx, ty)
    let visited = Set.add (tx, ty) visited

    ((hx, hy), (tx, ty), visited)

let solveSilver file =
    parse file
    |> Seq.fold moveRope ((0, 0), (0, 0), Set.singleton (0, 0))
    |> fun (_, _, visited) -> Set.count visited
    |> string

let moveLong (rope, visited) (x, y) =
    let (hx, hy) = List.head rope
    let (hx, hy) = (hx + x, hy + y)

    let rope = List.tail rope |> List.scan moveKnot (hx, hy)
    let visited = Set.add (List.last rope) visited

    (rope, visited)

let solveGold file =
    parse file
    |> Seq.fold moveLong ([for _ in 1 .. 10 -> (0, 0)], Set.singleton (0, 0))
    |> snd
    |> Set.count
    |> string

let Solvers = (solveSilver, solveGold)