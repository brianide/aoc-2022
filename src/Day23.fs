module Day23

open System.IO
open Util.Collections
open Util.Extensions
open Util.Math
open Util.Patterns
open Util.Plumbing

let parse file =
    File.ReadAllLines file
    |> Array.rev
    |> Array.mapi (fun r -> Seq.mapi (fun c -> function '#' -> Some (r, c) | _ -> None) >> Seq.choose id)
    |> Seq.collect id
    |> Set.ofSeq

let inline pass f v = f v; v
let inline passSeq f v = Seq.iter f v; v
let inline (.+) (x, y) (a, b) = (x + a, y + b)
let inline (.-) (x, y) (a, b) = (x - a, y - b)

let checkLive elves pos =
    [ for i in -1 .. 1 do for j in -1 .. 1 do if not (i = 0 && j = 0) then yield (i, j) ]
    |> List.map ((.+) pos)
    |> List.exists (elves: Set<_>).Contains
    // |> pass (printfn "Elf at %A: %b" pos)

let proposeMove elves index pos =
    let checkEmpty offs =
        Seq.map ((.+) pos) offs
        |> Seq.forall (fun n -> Set.contains n elves |> not)

    let proposals = [
        [ 1, -1;  1,  0;  1,  1], ( 1,  0) // North
        [-1, -1; -1,  0; -1,  1], (-1,  0) // South
        [-1, -1;  0, -1;  1, -1], ( 0, -1) // West 
        [-1,  1;  0,  1;  1,  1], ( 0,  1) // East
    ]

    [ index .. index + 3 ]
    |> List.map (fun i -> List.item (i % 4) proposals)
    |> List.tryFind (fst >> checkEmpty)
    |> Option.map (fun (_, prop) -> pos, pos .+ prop)
    
let getBounds elves =
    let ll = Seq.map fst elves |> Seq.min, Seq.map snd elves |> Seq.min
    let ur = Seq.map fst elves |> Seq.max, Seq.map snd elves |> Seq.max
    ll, ur

let countEmpty elves =
    let (r0, c0), (r1, c1) = getBounds elves
    let mutable count = 0
    for i in r0 .. r1 do
        for j in c0 .. c1 do
            if not <| Set.contains (i, j) elves then
                count <- count + 1
    count

let doRound n elves =
    elves
    |> Seq.filter (fun e -> checkLive elves e)
    |> Seq.choose (fun e -> proposeMove elves n e)

let doRounds numRounds elves =
    let rec recur n elves =
        if n = numRounds then
            elves
        else
            let moves =
                doRound n elves
                // |> passSeq (fun (f, t) -> printfn "* proposes %A" (t .- f))
                |> Seq.groupBy snd
                |> Seq.filter (fun (_, n) -> Seq.length n = 1)
                |> Seq.collect snd
                |> Seq.groupBy fst
                |> Seq.map (fun (_, n) -> Seq.head n)
            let elves = Seq.fold (fun acc (rem, add) ->
                // printfn "%A moves by %A" rem (add .- rem)
                acc |> Set.remove rem |> Set.add add) elves moves
            
            // printfn "== End of Round %i ==" (n + 1)
            // let (r0, c0), (r1, c1) = getBounds elves
            // for i in r1 + 1 .. -1 .. r0 - 1 do
            //     for j in c0 - 1 .. c1 + 1 do
            //         printf "%c" (if Set.contains (i, j) elves then '#' else '.')
            //     printfn ""
            // printfn ""

            recur (n + 1) elves
    recur 0 elves

let solveSilver input =
    let elves = doRounds 10 input
    
    let (r0, c0), (r1, c1) = getBounds elves
    for i in r1 .. -1 .. r0 do
        for j in c0 .. c1 do
            printf "%c" (if Set.contains (i, j) elves then '#' else '.')
        printfn ""
    printfn ""

    countEmpty elves
    |> sprintf "%A"
    // |> Seq.iter (printf "%A")
    // ""

let solveGold input =
    ""

let Solver = chainSolver parse solveSilver solveGold
