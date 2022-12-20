module Day19

open System.Collections.Generic
open System.IO
open Util.Collections
open Util.Extensions
open Util.Math
open Util.Patterns
open Util.Plumbing

//Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian.

type Resource =
    Ore | Clay | Obsidian | Geode
    static member Each = [Ore; Clay; Obsidian; Geode]
    static member EmptyMap = Seq.replicate 4 0 |> Seq.zip Resource.Each |> Map.ofSeq

// type Inventory = Map<Resource, int>
// type CostSpec = Map<Resource, int>
// type Blueprint = {Id: int; Costs: Map<Resource, CostSpec>; MaxCosts: Map<Resource, int>}
// type Strategy = Build of Resource | Wait
type Inventory = Map<Resource, int>
type CostSpec = Resource * int
type Blueprint = {Id: int; Costs: Map<Resource, CostSpec list>; MaxCosts: Map<Resource, int>}
type Strategy = Build of Resource | Wait

let parse file =
    let parseLine =
        let reg = "Blueprint (\\d+): \
                   Each ore robot costs (\\d+) ore. \
                   Each clay robot costs (\\d+) ore. \
                   Each obsidian robot costs (\\d+) ore and (\\d+) clay. \
                   Each geode robot costs (\\d+) ore and (\\d+) obsidian."
        String.regGroups reg
        >> List.map int
        >> function
        | [ident; ore; clay; obsidOre; obsidClay; geodeOre; geodeOsid] ->
            // let costMap =
            //     Seq.append (Seq.replicate 4 0 |> Seq.zip Resource.Each) >> Map.ofSeq
            let bill =
                Map.ofList [
                    // Ore, costMap [Ore, ore]
                    // Clay, costMap [Ore, clay]
                    // Obsidian, costMap [Ore, obsidOre; Clay, obsidClay]
                    // Geode, costMap [Ore, geodeOre; Obsidian, geodeOsid]
                    Ore, [Ore, ore]
                    Clay, [Ore, clay]
                    Obsidian, [Ore, obsidOre; Clay, obsidClay]
                    Geode, [Ore, geodeOre; Obsidian, geodeOsid]
                ]
            // let maxes =
            //     Map.values bill
            //     |> Seq.collect Map.toSeq
            //     |> Seq.groupBy fst
            //     |> Seq.map (fun (k, v) -> k, Seq.map snd v |> Seq.max)
            //     |> Map.ofSeq
            let maxes =
                Map.values bill
                |> Seq.collect id
                |> Seq.groupBy fst
                |> Seq.map (fun (k, v) -> k, Seq.map snd v |> Seq.max)
                |> Seq.append (Map.toSeq Resource.EmptyMap)
                |> Map.ofSeq
            {Id = ident; Costs = bill; MaxCosts = maxes}
        | _ -> failwith "Invalid input"
    File.ReadAllLines file
    |> Array.map parseLine

let breadthFirst neighborFn pruneFn initState =
    let rec recur queue explored =
        if Queue.isEmpty queue then
            explored
        else
            let (next, queue) = Queue.dequeue queue
            // printfn "%A" next
            let branches = neighborFn next |> Seq.filter (fun e -> not <| Set.contains e explored)
            let explored = Seq.fold (fun acc e -> Set.add e acc) explored branches
            let queue = Seq.fold (fun acc e -> Queue.enqueue e acc) queue branches
            let queue = if Queue.isEmpty queue then queue else pruneFn queue
            recur queue explored
    
    let queue = Queue.singleton initState
    let explored = Set.singleton initState
    recur queue explored

let solveBlueprint print maxTime =
    let canBuildMiner (inv: Inventory) kind =
        print.Costs[kind] |> Seq.forall (fun (res, amt) -> inv[res] >= amt)

    let buildMiner (inv: Inventory) (bots: Inventory) kind =
        let inv = Seq.fold (fun acc (res, amt) -> Map.add res (acc[res] - amt) acc) inv print.Costs[kind]
        let bots = Map.add kind (bots[kind] + 1) bots
        (inv, bots)

    let nextStates (inv: Inventory, bots: Inventory, timeLeft) =
        let buildable = Resource.Each |> List.filter (canBuildMiner inv)
        let inv = Map.map (fun res amt -> bots[res] + amt) inv

        if timeLeft = 0 then
            []
        elif Seq.contains Geode buildable then
            let (inv, bots) = buildMiner inv bots Geode
            [inv, bots, timeLeft - 1]
        else
            buildable
            |> List.filter (fun kind -> bots[kind] < print.MaxCosts[kind])
            |> List.map (buildMiner inv bots >> fun (inv, bots) -> (inv, bots, timeLeft - 1))
            |> List.append [inv, bots, timeLeft - 1]

    let pruneQueue =
        let mutable lastDepth = maxTime
        fun queue ->
            let (_, _, depth) = Queue.head queue
            if depth <> lastDepth then
                printfn "%A" depth
                lastDepth <- depth
                Queue.toSeq queue
                |> Seq.groupBy (fun (inv: Inventory, _, _) -> inv[Geode])
                |> Seq.maxBy fst
                |> snd
                |> Seq.toList
                |> Queue.ofList

            else
                queue
    
    let inv = Resource.EmptyMap
    let bots = Map.add Ore 1 Resource.EmptyMap
    breadthFirst nextStates pruneQueue (inv, bots, maxTime)
    |> Seq.map (fun (inv, _, _) -> inv[Geode])
    |> Seq.max
    |> (*) print.Id
    
let solveSilver (input: Blueprint[]) =
    input
    |> Seq.map (fun print -> async {return solveBlueprint print 24})
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Seq.sum
    |> string

let solveGold (input: Blueprint[]) =
    ""

let Solver = chainSolver parse solveSilver solveGold
