module Day9

open System.IO
open Util.Extensions
open Util.Patterns
open Util.Plumbing

let parse =
    let (|Dir|_|) = function
    | "R" -> Some (1, 0)
    | "L" -> Some (-1, 0)
    | "U" -> Some (0, -1)
    | "D" -> Some (0, 1)
    | _ -> None

    let parseLine = function
    | [Dir dir; Int32 dist] -> seq { for _ in 1 .. dist -> dir }
    | x -> failwithf "Invalid input: %A" x

    File.ReadAllLines >> Seq.collect (String.split " " >> Seq.toList >> parseLine)

let moveKnot (hx, hy) (kx, ky) =
    let (dx, dy) = (hx - kx, hy - ky)
    if abs dx > 1 || abs dy > 1 then
        (kx + sign dx, ky + sign dy)
    else
        (kx, ky)

let moveRope (rope, visited) (x, y) =
    let (hx, hy) = List.head rope
    let (hx, hy) = (hx + x, hy + y)

    let rope = List.tail rope |> List.scan moveKnot (hx, hy)
    let visited = Set.add (List.last rope) visited

    (rope, visited)

let solve length =
    parse
    >> Seq.fold moveRope ([for _ in 1 .. length -> (0, 0)], Set.singleton (0, 0))
    >> snd
    >> Set.count

let Solvers = simpleSolver (solve 2 >> string) (solve 10 >> string)