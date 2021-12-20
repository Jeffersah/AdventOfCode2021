

let inputFile = "./inputs16-20/d20.txt"

let lines = System.IO.File.ReadAllLines(inputFile) |> List.ofArray

printfn "%A" (AdventOfCode2021.Day20.run lines)