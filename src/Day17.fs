module Day17

open System.IO
open Util.Extensions
open Util.Plumbing

let ChamberWidth = 7

let Shapes =
    let conv shp =
        (shp: string).Replace(" ", "").Split('\n')
        |> Seq.rev
        |> Seq.mapi (fun r row -> Seq.mapi (fun c n -> if n = '#' then [(r, c)] else []) row)
        |> Seq.collect (Seq.collect id)
        |> Seq.toList

    [|
        "####";

        ".#.
         ###
         .#.";

        "..#
         ..#
         ###";

        "#
         #
         #
         #";
        
        "##
         ##";
    |]
    |> Array.map conv

type Side =
| Left = 0uy
| Right = 1uy

let parse file =
    File.ReadAllText file
    |> Seq.map (function '<' -> Side.Left | '>' -> Side.Right | _ -> failwith "Invalid input")
    |> Seq.toArray

let inline (.+) (ax, ay) (bx, by) = (ax + bx, ay + by)

let dropShape (jets: Side[]) jetIndex (tower, height) shape =
    let checkCollide shape =
        let outside =
            let bottom = List.map fst shape |> List.min
            let cols = List.map snd shape
            bottom < 0 || List.min cols < 0 || List.max cols >= ChamberWidth

        outside || List.exists (fun p -> Set.contains p tower) shape

    let rec fall ind shape =
        let shift =
            match jets[ind % jets.Length] with
            | Side.Left -> (0, -1)
            | Side.Right -> (0, 1)
            | c -> failwithf "Invalid jet value: %A" c

        let shape' = shape |> List.map (fun p -> p .+ shift)
        let shape = if checkCollide shape' then shape else shape'

        let shape' = shape |> List.map (fun p -> p .+ (-1, 0))
        if checkCollide shape' then
            let tower = Set.union tower (Set.ofList shape)
            let height = max height ((List.map fst shape |> List.max) + 1)
            ind + 1, (tower, height)
        else
            // for r in height + 5 .. -1 .. 0 do
            //     for c in 0 .. 6 do
            //         printf "%c" (if Set.contains (r, c) tower then '#' elif List.contains (r, c) shape' then '@' else '.')
            //     printfn ""
            // printfn ""
            fall (ind + 1) shape'

    let origin = (height + 3, 2)

    shape
    |> List.map (fun c -> c .+ origin)
    |> fall jetIndex

let solveSilver jets =
    let rec drop tower jetIndex shapeIndex =
        // let () =
        //     let (blocks, height) = tower
            // printfn "%A %A:" shapeIndex height
            // for r in height .. -1 .. 0 do
            //     for c in 0 .. 6 do
            //         printf "%c" (if Set.contains (r, c) blocks then '#' else '.')
            //     printfn ""
            // printfn ""
            // printfn "%A" blocks

        if shapeIndex = 2022 then
            tower
        else
            let jetIndex, tower = dropShape jets jetIndex tower Shapes[shapeIndex % Shapes.Length]
            drop tower jetIndex (shapeIndex + 1)
    
    let (final, height) = drop (Set.empty, 0) 0 0

    for r in 0 .. height do
        [for c in 0 .. 6 -> (r, c)]
        |> List.forall (fun p -> Set.contains p final)
        |> fun b -> if b then printfn "%A" r

    // printfn "%A" height
    // Image.saveToPPM   
    "" 

let solveGold input =
    ""

let Solver = chainSolver parse solveSilver solveGold
