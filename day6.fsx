#load "util.fsx"
open Util.Extensions

let findMarker length =
    Seq.windowed length
    >> Seq.findIndex (fun w -> w |> Seq.distinct |> Seq.length = length)
    >> (+) length

[4; 14]
|> List.map (fun n -> findMarker n Util.inputText.Value)
|> printfn "%A"