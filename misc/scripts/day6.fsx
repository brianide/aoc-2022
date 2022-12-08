#load "util.fsx"

let findMarker length =
    Seq.windowed length
    >> Seq.findIndex (Seq.distinct >> Seq.length >> (=) length)
    >> (+) length

[4; 14]
|> List.map (fun n -> findMarker n Util.inputText.Value)
|> printfn "%A"