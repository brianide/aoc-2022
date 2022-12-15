module Day15

open System.IO
open Util.Extensions
open Util.Math
open Util.Patterns
open Util.Plumbing

// Sensor at x=12, y=14: closest beacon is at x=10, y=16

let parse file =
    let parseLine =
        function
        | [Int32 sx; Int32 sy; Int32 bx; Int32 by] -> (sx, sy), (bx, by)
        | _ -> failwith "Malformed input"

    File.ReadAllText file
    |> String.regMatchGroups @"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)"
    |> Seq.map parseLine

let distance (ax, ay) (bx, by) = abs (bx - ax) + abs (by - ay)

// let solveSilver input =
//     let sensors = Seq.map (fun (spos, bpos) -> spos, distance spos bpos) input
//     let beacons = Seq.map snd input
//     let minx = sensors |> Seq.map (fun ((x, _), rng) -> x - rng) |> Seq.min
//     let maxx = sensors |> Seq.map (fun ((x, _), rng) -> x + rng) |> Seq.max
    
//     seq {minx .. maxx}
//     |> Seq.filter (fun x -> beacons |> Seq.exists (fun bpos -> (x, 2000000) = bpos) || sensors |> Seq.exists (fun (spos, rng) -> distance (x, 2000000) spos <= rng))
//     |> Seq.length
//     |> string

let solveSilver input =
    let coverage = Seq.map (fun (spos, bpos) -> spos, distance spos bpos) input
    
    // let axis = 10
    let axis = 2000000

    let beacons = input |> Seq.map snd |> Seq.filter (fun (_, y) -> y = axis) |> Seq.map fst |> Set.ofSeq

    let covered =
        coverage
        |> Seq.filter (fun ((_, y), rng) -> abs (y - axis) < rng)
        |> Seq.collect (fun ((x, y), rng) ->
            let ydiff = abs (y - axis)
            let offset = rng - ydiff
            seq {x - offset .. x + offset})
        |> Set.ofSeq

    Set.difference covered beacons
    |> Set.count
    |> string

let solveGold input =
    let beacon = (14, 11)
    let coverage = Seq.mapi (fun i (spos, bpos) -> {|Pos = spos; Range = distance spos bpos; Id = i|}) input

    Seq.allPairs coverage coverage
    |> Seq.filter (fun (a, b) -> a.Id <> b.Id && distance a.Pos b.Pos <= a.Range + b.Range + 2)
    |> Seq.iter (printfn "%A")

    // coverage
    // |> Seq.pairs
    // |> Seq.filter (fun (a, b) -> distance a.Pos b.Pos <= a.Range + b.Range)
    // |> Seq.iter (printfn "%A")

    ""

// let solveGold input =
//     let beacon = (14, 11)
//     let coverage = Seq.map (fun (spos, bpos) -> spos, distance spos bpos) input

//     let drawCoverageDiagram n (pos, rng) =
//         let fmt x y =
//             if (x, y) = beacon then
//                 (240uy, 100uy, 200uy)
//             elif distance (x, y) pos <= rng then
//                 (180uy, 120uy, 230uy)
//             else
//                 (60uy, 60uy, 60uy)

//         Image.saveToPPM fmt (sprintf "out/test%03d.ppm" n) 20 20

//     Seq.iteri drawCoverageDiagram coverage
//     ""

let Solver = chainSolver parse solveSilver solveGold
