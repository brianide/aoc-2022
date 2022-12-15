module Image

open System.IO
open System.Text

let saveToPGM byteFn path grid =
    let width, height = Array2D.length2 grid, Array2D.length1 grid
    let header = $"P5 {width} {height} 255 "
    
    let body = Array.zeroCreate<byte> (ASCIIEncoding.ASCII.GetByteCount header + width * height)
    let offset = ASCIIEncoding.ASCII.GetBytes(header, body)

    for i in 0 .. width * height - 1 do
        let r, c = i / width, i % width
        body[i + offset] <- byteFn c r grid[r, c]

    File.WriteAllBytes(path, body)

let saveToPPM byteFn path width height =
    let header = $"P6 {width} {height} 255 "
    
    let body = Array.zeroCreate<byte> (ASCIIEncoding.ASCII.GetByteCount header + width * height * 3)
    let offset = ASCIIEncoding.ASCII.GetBytes(header, body)

    for i in 0 .. width * height - 1 do
        let r, c = i / width, i % width
        let (red, green, blue) = byteFn c r
        body[i * 3 + offset] <- red
        body[i * 3 + offset + 1] <- green
        body[i * 3 + offset + 2] <- blue

    File.WriteAllBytes(path, body)