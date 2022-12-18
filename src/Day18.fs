module Day18

open System.IO
open Util.Collections
open Util.Extensions
open Util.Patterns
open Util.Plumbing

let parse file =
    File.ReadAllLines file
    |> Seq.map (fun n -> n.Split(',') |> Array.choose (|Int32|_|))
    |> Seq.map (function [|a; b; c|] -> (a, b, c) | _ -> failwith "Invalid input")
    |> Set.ofSeq

let inline (.+) (a, b, c) (i, j, k) = (a + i, b + j, c + k)
let inline bounds (a, b, c) (i, j, k) (x, y, z) =
    a <= x && x <= i && b <= y && y <= j && c <= z && z <= k

let neighbors p =
    [(1,0,0); (0,1,0); (0,0,1); (-1,0,0); (0,-1,0); (0,0,-1)]
    |> List.map (fun o -> p .+ o)

let solveSilver points =
    points
    |> Seq.collect (neighbors >> Seq.filter (fun p -> not <| Set.contains p points))
    |> Seq.length
    |> string

let getBounds points margin =
    let xs = Seq.map (fun (x, _, _) -> x) points
    let ys = Seq.map (fun (_, y, _) -> y) points
    let zs = Seq.map (fun (_, _, z) -> z) points
    let minx, maxx = Seq.min xs - margin, Seq.max xs + margin
    let miny, maxy = Seq.min ys - margin, Seq.max ys + margin
    let minz, maxz = Seq.min zs - margin, Seq.max zs + margin
    (minx, miny, minz), (maxx, maxy, maxz)

let calcArea points =
    // Get a bounding box around the droplet with a margin of "air" one unit wide
    let (lower, upper) = getBounds points 1

    // BFS starting from the bottom corner of the bounding box. We use the same
    // neighbor selector from the first half of the problem; nodes constituent to
    // the droplet (ie. in the set of points) are tallied up, while "air" nodes
    // are queued.
    let rec recur explored queue area =
        if Queue.isEmpty queue then
            area
        else
            let (next, queue) = Queue.dequeue queue
            
            // Find neighbors inside the bounding box
            let matter, air =
                neighbors next
                |> List.filter (fun p -> bounds lower upper p)
                |> List.partition (fun p -> Set.contains p points)

            // Tally neighboring matter tiles
            let area = area + List.length matter

            // Queue neighboring air tiles
            let air = List.filter (fun p -> not <| Set.contains p explored) air
            let explored = List.fold (fun acc p -> Set.add p acc) explored air
            let queue = List.fold (fun acc p -> Queue.enqueue p acc) queue air

            recur explored queue area

    let explored = Set.singleton lower
    let queue = Queue.singleton lower
    recur explored queue 0
    


let solveGold = calcArea >> string

let Solver = chainSolver parse solveSilver solveGold
