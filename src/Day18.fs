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
let inline negate (a, b, c) = (-a, -b, -c)
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

let renderFrame =
    let mutable count = 0
    let mutable dlast = -1

    let cExplored = Image.intToRGB 0x54ca5f
    let cQueued = Image.intToRGB 0x44864a
    let cBack = Image.intToRGB 0x242424
    let cMatter = Image.intToRGB 0x666666
    let cBorder = cMatter

    fun lower upper points depth explored queue ->
        let (lx, _, lz) = lower
        let (ux, _, uz) = upper
        let owidth, oheight = ux - lx, uz - lz
        let twidth = 1 + owidth * 7
        let theight = 1 + owidth * 3

        if true then
            let queued = Queue.toSeq queue |> Seq.map fst |> Set.ofSeq
            Image.saveToPPM twidth theight (sprintf @"out/drop%08d.ppm" count)
            <| fun c r ->
                if c % owidth = 0 || r % oheight = 0 then
                    cBorder
                else
                    let layer = (c - 1) / owidth + ((r - 1) / oheight * 7)
                    let xloc, zloc = c % owidth - 2, r % oheight - 2
                    match (xloc, layer, zloc) with
                    | p when Set.contains p queued -> cQueued
                    | p when Set.contains p explored -> cExplored
                    | p when Set.contains p points -> cMatter
                    | _ -> cBack

            dlast <- depth
            count <- count + 1 

let calcArea points =
    // Get a bounding box around the droplet with a margin of "air" one unit wide
    let (lower, upper) = getBounds points 3

    printfn "%A" (upper .+ negate lower)

    // BFS starting from the bottom corner of the bounding box. We use the same
    // neighbor selector from the first half of the problem; nodes constituent to
    // the droplet (ie. in the set of points) are tallied up, while "air" nodes
    // are queued.
    let rec recur explored queue area =
        if Queue.isEmpty queue then
            area
        else
            let ((next, depth), queue) = Queue.dequeue queue
            
            let drawable =
                let lower = lower .+ (0, 3, 0)
                let upper = upper .+ (0, -3, 0)
                bounds lower upper next

            if drawable then
                renderFrame lower upper points depth explored queue

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
            let queue = List.fold (fun acc p -> Queue.enqueue (p, depth + 1) acc) queue air

            recur explored queue area

    let center =
        let (a, b, c) = upper
        let (x, y, z) = lower
        ((a + x) / 2, y, (c + z) / 2)
    let explored = Set.singleton center
    let queue = Queue.singleton (center, 0)
    recur explored queue 0
    


let solveGold = calcArea >> string

let Solver = chainSolver parse solveSilver solveGold
