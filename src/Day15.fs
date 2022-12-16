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
    |> String.regMatchGroups @"^Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)$"
    |> Seq.map parseLine

let inline bounds lo hi n = lo <= n && n <= hi
let inline distance (ax, ay) (bx, by) = abs (bx - ax) + abs (by - ay)

let solveSilver input =
    let yval = 2000000
    // let yval = 10

    // Combines sorted ranges
    let rangeFold ranges e =
        match ranges, e with
        | [], e -> [e]
        | (lh, rh) :: more, (le, re) when rh >= le -> (lh, re) :: more
        | ranges, e -> e :: ranges 

    // Find coverage slices
    let ranges =
        input
        |> Seq.map (fun (spos, bpos) -> (spos, distance spos bpos))
        |> Seq.filter (fun ((_, y), rng) -> abs (y - yval) < rng)
        |> Seq.map (fun ((x, y), rng) ->
            let ydiff = abs (y - yval)
            let offset = rng - ydiff
            (x - offset, x + offset))
        |> Seq.sortBy fst
        |> Seq.fold rangeFold []

    // Get beacons falling inside the covered ranges
    let beacons =
        input
        |> Seq.map snd
        |> Seq.filter (fun (_, y) -> y = yval)
        |> Seq.map fst
        |> Seq.distinct
        |> Seq.filter (fun n -> Seq.exists (fun (lo, hi) -> bounds lo hi n) ranges)
        |> Seq.length

    // Total of the covered ranges, less the number of beacons in the coverage area
    (ranges |> Seq.map (fun (lo, hi) -> hi - lo + 1) |> Seq.sum) - beacons |> string

let solveGold input =
    let limit = 4000000
    // let limit = 20

    let sensors = Seq.map (fun (spos, bpos) -> (spos, distance spos bpos)) input
    let beacons = input |> Seq.map snd |> Set.ofSeq

    // Converts coordinates to a tilted axis where our diamonds are squares instead
    let tilt (x, y) =
        (x - y, x + y)

    // Unconverts coordinates
    let detilt (a, b) =
        let x = (a + b) / 2
        (x, b - x)

    // Gets the bounds of each sensor's coverage area, expanded by one, in squarespace coordinates
    let squareBounds =
        Seq.map (fun ((x, y), rng) -> (tilt (x - rng - 1, y), tilt (x + rng + 1, y))) sensors

    // Find all squarepace X-axis values where two coverage areas are separated by one tile
    let xs =
        squareBounds
        |> Seq.map (Tuple2.map fst)
        |> Seq.fold (fun (acc, bcc) (a, b) -> Set.add a acc, Set.add b bcc) (Set.empty, Set.empty)
        |> fun (a, b) -> Set.intersect a b

    // Same as above but for the Y axis
    let ys =
        squareBounds
        |> Seq.map (Tuple2.map snd)
        |> Seq.fold (fun (acc, bcc) (a, b) -> Set.add a acc, Set.add b bcc) (Set.empty, Set.empty)
        |> fun (a, b) -> Set.intersect a b

    // Pair up coordinates, convert them back into diamondspace, and find the one we want
    Seq.allPairs xs ys
    |> Seq.map detilt
    |> Seq.filter (fun (x, y) -> bounds 0 limit x && bounds 0 limit y)
    |> Seq.filter (fun pc -> not <| Set.contains pc beacons)
    |> Seq.filter (fun pc -> sensors |> Seq.forall (fun (ps, rng) -> distance pc ps > rng))
    |> Seq.exactlyOne
    |> fun (x, y) -> (uint64 x) * 4000000UL + (uint64 y) |> string

let Solver = chainSolver parse solveSilver solveGold
