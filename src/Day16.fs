module Day16

open System.IO
open Util.Collections
open Util.Extensions
open Util.Patterns
open Util.Plumbing

// Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE

type ValveGraph = Map<string, int * Map<string, int>>

let parse file =
    let parseLine line =
        let [rateString; tunnelString] = String.split "; " line
        let [valve; Int32 rate] = String.regGroups @"Valve ([A-Z]{2}) has flow rate=(\d+)" rateString

        let outlets =
            String.regMatches @"[A-Z]{2}" tunnelString
            |> Seq.map (fun n -> (n, 1))
            |> Map.ofSeq

        valve, (rate, outlets)

    File.ReadAllLines file
    |> Seq.map parseLine
    |> Map.ofSeq

let pruneZeroNodes graph =
    let rec prune tab =
        tab
        |> Map.tryFindKey (fun k (flow, _) -> k <> "AA" && flow = 0)
        |> function
        | None -> tab
        | Some kz ->
            tab
            |> Map.toSeq
            |> Seq.filter (fun (_, (_, outs)) -> Map.containsKey kz outs)
            |> Seq.map (fun (ks, (flow, outs)) ->
                let repl =
                    tab[kz] |> snd |> Map.toSeq
                    |> Seq.filter (fun (kd, _) -> kd <> ks)
                    |> Seq.map (fun (k, v) -> k, v + 1)
                let outs =
                    outs |> Map.toSeq
                    |> Seq.filter (fun (dest, _) -> dest <> kz)
                    |> Seq.append repl
                    |> Map.ofSeq
                ks, (flow, outs))
            |> Seq.fold (fun acc (k, v) -> Map.add k v acc) (Map.filter (fun k _ -> k <> kz) tab)
            |> prune 
    prune graph

let calcNodeCosts graph start =
    let mutable unvisited = Map.keys graph |> Set.ofSeq
    let mutable dists =
        Map.keys graph
        |> Seq.map (fun k -> k, if k = start then 0 else System.Int32.MaxValue)
        |> Map.ofSeq
    let mutable next = Some start

    while Option.isSome next do
        let curr = Option.get next
        let currDist = dists[curr]
        let newCosts = graph[curr] |> snd |> Map.map (fun _ cost -> cost + currDist)
        dists <- dists |> Map.map (fun k v -> min v (Map.tryFind k newCosts |> Option.defaultValue v))
        unvisited <- unvisited |> Set.remove curr

        if Set.isEmpty unvisited then
            next <- None
        else
            next <- unvisited |> seq |> Seq.map (fun k -> k, dists[k]) |> Seq.minBy snd |> fst |> Some

    dists |> Map.filter (fun k _ -> k <> start)

let solveSilver input =
    let pruned = pruneZeroNodes input
    Seq.fold (fun acc k -> Map.add k (fst acc[k], calcNodeCosts acc k) acc) pruned (Map.keys pruned)
    |> Map.iter (printfn "%A %A")
    ""

let solveGold input =
    ""

let Solver = chainSolver parse solveSilver solveGold
