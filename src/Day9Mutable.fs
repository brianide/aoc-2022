module Day9Mutable

open System.IO
open System.Collections.Generic
open Util.Extensions
open Util.Patterns
open Util.Plumbing

let moveKnot (hx, hy) (kx, ky) =
    let (dx, dy) = (hx - kx, hy - ky)
    if abs dx > 1 || abs dy > 1 then
        (kx + sign dx, ky + sign dy)
    else
        (kx, ky)

let moveRope (rope: (int * int)[], visited: HashSet<int * int>) (x, y) =
    rope[0] <- (fst rope[0] + x, snd rope[0] + y)
    for i in 1 .. rope.Length - 1 do
        rope[i] <- moveKnot rope[i - 1] rope[i]

    visited.Add rope[rope.Length - 1] |> ignore

let solve length file =
    let (|Dir|_|) = function
    | "R" -> Some (1, 0)
    | "L" -> Some (-1, 0)
    | "U" -> Some (0, -1)
    | "D" -> Some (0, 1)
    | _ -> None

    let handleLine rope visited = function
    | [Dir dir; Int32 dist] -> 
        for _ in 1 .. dist do
            moveRope (rope, visited) dir
    | x -> failwithf "Invalid input: %A" x

    let rope = [|for _ in 1 .. length -> (0, 0)|]
    let visited = HashSet<int * int>()

    File.ReadAllLines file |> Array.iter (String.split " " >> handleLine rope visited)
    visited.Count |> string

let Solver = simpleSolver (solve 2) (solve 10)