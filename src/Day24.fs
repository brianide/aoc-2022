module Day24

open System.IO
open Util.Collections
open Util.Extensions
open Util.Extensions.Tuple2.Infix
open Util.Patterns
open Util.Plumbing

[<Struct>]
type Direction = North | South | East | West
[<Struct>]
type Blizzard = Blizzard of (int * int) * Direction

let inline bounds lo hi n = lo <= n && n <= hi

let parse file =
    let lines = File.ReadAllLines file
    let width, height = lines[0].Length - 2, lines.Length - 2

    let parseChar = function
    | '^' -> Some North | '<' -> Some West
    | 'v' -> Some South | '>' -> Some East
    | _ -> None

    lines[1 .. lines.Length - 1]
    |> Seq.mapi (fun r -> Seq.mapi (fun c -> parseChar >> Option.map (fun dir -> Blizzard ((r, c - 1), dir))))
    |> Seq.collect (Seq.choose id)
    |> Array.ofSeq
    |> fun n -> (width, height), n

let advanceBlizzards (width, height) (blizzards: Blizzard[]) =
    let move (Blizzard ((r, c), dir)) = 
        let (i, j) =
            match dir with
            | North -> (-1, 0) | West -> (0, -1)
            | South -> ( 1, 0) | East -> (0,  1)
        Blizzard (((r + i + height) % height, (c + j + width) % width), dir)
    Array.mutate move blizzards

let getNeighbors (width, height) (blizzards: Blizzard[]) pos =
    [1, 0; -1, 0; 0, -1; 0, 1; 0, 0]
    |> List.map ((.+) pos)
    |> List.filter (fun (r, c) -> bounds 0 (height - 1) r && bounds 0 (width - 1) c)
    |> List.filter (fun p -> Array.forall (fun (Blizzard (bpos, _)) -> p <> bpos) blizzards)

let breadthFirst (width, height as dims) blizzards =
    let goal = (height - 1, width - 1)
    // printfn "%A" goal
    let mutable lastGen = -1
    let rec recur queue explored blizzards =
        if Queue.isEmpty queue then
            None
        else
            let ((pos, gen), queue) = Queue.dequeue queue
            if pos = goal then
                Some (pos, gen)
            else
                if gen > lastGen then
                    lastGen <- gen
                    advanceBlizzards dims blizzards |> ignore
                    printfn "%A" gen

                let branches =
                    getNeighbors dims blizzards pos
                    |> List.map (fun e -> e, gen + 1)
                    |> List.filter (fun e -> not <| Set.contains e explored)
                let explored =
                    List.fold (fun acc e -> Set.add e acc) explored branches

                let queue = Seq.fold (fun acc e -> Queue.enqueue e acc) queue branches
                recur queue explored blizzards
    
    let init = (0, 0), 1
    let queue = Queue.singleton init
    let explored = Set.singleton init
    recur queue explored blizzards

let printable (fromPos, toPos) =
    match toPos .- fromPos with
    | (-1, 0) -> Some North | (0, -1) -> Some West
    | ( 1, 0) -> Some South | (0,  1) -> Some East
    | ( 0, 0) -> None | x -> failwithf "wtf: %A" x

let solveSilver (dims, blizzards) =
    // advanceBlizzards dims blizzards |> ignore
    breadthFirst dims blizzards
    |> function
    | Some (pos, steps) ->
        // moves |> Seq.pairwise |> Seq.map (fun e -> fst e, printable e) |> Seq.iter (printfn "%A")
        printfn "%A %A" pos steps
    | None -> ()
    ""

let solveGold input =
    ""

let Solver = chainSolver parse solveSilver solveGold