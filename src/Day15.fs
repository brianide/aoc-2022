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

let distance (ax, ay) (bx, by) = abs (bx - ax) + abs (by - ay)
let add (x, y) (i, j) = (x + i, y + j)
let mul (x, y) n = (x * n, y * n)

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
    let coverage = Seq.map (fun (spos, bpos) -> (spos, distance spos bpos)) input
    let beacons = input |> Seq.map snd |> Set.ofSeq

    let getShell =
        let steps = [
            ((-1, 0), (1, -1))
            ((0, 1), (1, 1))
            ((1, 0), (-1, 1))
            ((0, 1), (-1, -1))
        ]

        fun (pos, range) -> 
            let rad = range + 1
            steps |> Seq.collect (fun (off, step) -> seq {for i in 0 .. range -> mul step i |> add (mul off rad) |> add pos})

    let (px, py) =
        coverage
        |> Seq.mapi (fun i s -> (getShell s |> Seq.filter (fun (x, y) -> x >= 0 && x <= limit && y >= 0 && y <= limit), Seq.removeAt i coverage))
        |> Seq.choose (fun (hull, others) -> Seq.tryFind (fun p -> Seq.exists (fun (spos, srng) -> distance spos p <= srng) others |> not) hull)
        |> Seq.filter (fun e -> not <| Set.contains e beacons)
        |> Seq.take 1
        |> Seq.exactlyOne

    (uint64 px) * 4000000UL + (uint64 py) |> string

let Solver = chainSolver parse solveSilver solveGold
