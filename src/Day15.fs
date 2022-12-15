module Day15

open System.IO
open Util.Extensions
open Util.Math
open Util.Patterns
open Util.Plumbing

let parse file =
    let parseLine =
        function
        | [Int32 sx; Int32 sy; Int32 bx; Int32 by] -> (sx, sy), (bx, by)
        | _ -> failwith "Malformed input"

    File.ReadAllText file
    |> String.regMatchGroups @"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)"
    |> Seq.map parseLine

let inline (.+) (x, y) (i, j) = (x + i, y + j)
let inline (.-) (x, y) (i, j) = (x - i, y - j)
let inline (.*) (x, y) n = (x * n, y * n)
let inline distance (ax, ay) (bx, by) = abs (bx - ax) + abs (by - ay)
let inline bounds min max n = n >= min && n <= max

let solveSilver input =
    let yval = 2000000
    let coverage = Seq.map (fun (spos, bpos) -> (spos, distance spos bpos)) input
    let beacons = input |> Seq.map snd |> Seq.filter (fun (_, y) -> y = yval) |> Seq.map fst |> Set.ofSeq

    let covered =
        coverage
        |> Seq.filter (fun ((_, y), rng) -> abs (y - yval) < rng)
        |> Seq.collect (fun ((x, y), rng) ->
            let ydiff = abs (y - yval)
            let offset = rng - ydiff
            seq {x - offset .. x + offset})
        |> Set.ofSeq

    Set.difference covered beacons
    |> Set.count
    |> string

let solveGold input =
    let limit = 4000000
    // let limit = 20
    let coverage = Seq.map (fun (spos, bpos) -> (spos, distance spos bpos)) input
    let beacons = input |> Seq.map snd |> Set.ofSeq

    let xs =
        coverage
        |> Seq.map (fun ((x, y), rng) -> let xp = x - y in (xp - rng - 1, xp + rng + 1))
        |> Seq.fold (fun (acc, bcc) (a, b) -> Set.add a acc, Set.add b bcc) (Set.empty, Set.empty)
        |> fun (a, b) -> Set.intersect a b

    let ys =
        coverage
        |> Seq.map (fun ((x, y), rng) -> let xp = x + y in (xp - rng - 1, xp + rng + 1))
        |> Seq.fold (fun (acc, bcc) (a, b) -> Set.add a acc, Set.add b bcc) (Set.empty, Set.empty)
        |> fun (a, b) -> Set.intersect a b

    Seq.allPairs xs ys
    |> Seq.map (fun (a, b) -> let x = (a + b) / 2 in (x, b - x))
    |> Seq.filter (fun pc -> not <| Set.contains pc beacons)
    |> Seq.filter (fun pc -> coverage |> Seq.forall (fun (ps, rng) -> distance pc ps > rng))
    |> Seq.exactlyOne
    |> fun (x, y) -> (uint64 x) * 4000000UL + (uint64 y) |> string

let Solver = chainSolver parse solveSilver solveGold
