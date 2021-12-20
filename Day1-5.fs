namespace AdventOfCode2021
open Utils

module Day1 =
    let run (input:int list) = 
        List.fold (fun (count, prev) next -> if next > prev then (count + 1, next) else (count, next)) (0, input.[0]) input |> fst

    let runPart2 input =
        List.windowed 3 input |> List.map List.sum |> run

module Day2 =
    type Instruction = Forward of int | Down of int | Up of int

    let parseInput (str: string) = 
        let splIndex = Seq.findIndex ((=) ' ') str
        let instr = match (str.[0..splIndex - 1]) with
                    | "forward" -> Forward
                    | "down" -> Down
                    | "up" -> Up
        let ct = int str.[splIndex+1..]
        instr ct

    let runInstrs instrs =
        instrs |> Seq.fold (fun (x,y) instr -> 
            match instr with
            | Forward f -> (x+f,y)
            | Down d -> (x,y+d)
            | Up u -> (x,y-u)
        ) (0,0)

    let run lines =
        let x,y = lines |> List.map parseInput |> runInstrs
        x*y
        
    let runInstrsPart2 instrs =
        instrs |> Seq.fold (fun (x,y,aim) instr -> 
            match instr with
            | Forward f -> (x+f, y+f*aim, aim)
            | Down d -> (x, y, aim + d)
            | Up u -> (x, y, aim - u)
        ) (0,0,0)

    let runPart2 lines =
        let x,y,_ = lines |> List.map parseInput |> runInstrsPart2
        x*y

module Day3 =
    let findMostCommonBit (bitstrings:string list) most index =
        let oneCount = bitstrings |> List.where (fun s -> s.[index] = '1') |> List.length
        if (oneCount * 2 >= (List.length bitstrings)) = most then 1 else 0

    let valueFromBits bits =
        bits |> List.fold (fun v n -> (v <<< 1) + n) 0

    let run (bitstrings:string list) =
        let length = String.length bitstrings.[0]
        let gamma = List.init length (findMostCommonBit bitstrings true) |> valueFromBits
        let epsilon = List.init length (findMostCommonBit bitstrings false) |> valueFromBits
        gamma * epsilon

    let filterUncommonBits bitstrings most index =
        let tgtBit = findMostCommonBit bitstrings most index |> string |> (fun s -> s.[0])
        bitstrings |> List.where (fun s -> s.[index] = tgtBit)

    let run2 (bitstrings:string list) =
        let rec run2rec most index bitstrings =
            match bitstrings with
            | [v] -> v
            | _ -> filterUncommonBits bitstrings most index |> run2rec most (index+1)
        let gamma = run2rec true 0 bitstrings |> Seq.map (string>>int) |> List.ofSeq |> valueFromBits
        let epsilon = run2rec false 0 bitstrings |> Seq.map (string>>int) |> List.ofSeq |> valueFromBits
        gamma * epsilon

module Day4 =
    type BoardCell = int * bool
    type Board = BoardCell[,]

    let getRow board row =
        Seq.init (Array2D.length1 board) (fun i -> Array2D.get board i row)

    let getColumn board col =
        Seq.init (Array2D.length2 board) (fun i -> Array2D.get board col i)

    let isWin cells =
        cells |> Seq.forall Utils.Tuples.second

    let checkWin board =
        let cols = [0..(Array2D.length1 board)-1] |> List.map (getColumn board)
        let cols = cols @ ([0..(Array2D.length2 board)-1] |> List.map (getRow board))
        if Seq.exists (isWin) cols 
        then Some (board |> Utils.Array2D.toSeq |> Seq.where (Utils.Tuples.second >> not) |> Seq.map (Utils.Tuples.first) |> Seq.sum)
        else None

    let markNumber callNumber board =
        let indecies = seq { 
            for x in [0..(Array2D.length1 board)-1] do 
                for y in [0..(Array2D.length2 board) - 1] do 
                    if Array2D.get board x y |> Utils.Tuples.first = callNumber then yield (x,y)
        }
        Seq.iter (fun (x,y) -> Array2D.get board x y |> Utils.Tuples.mapSecond (fun _ -> true) |> Array2D.set board x y) indecies
        board

    let runUntilWin sequence board =
        let mutable index = 0;
        Seq.tryPick (fun call -> 
            let board = markNumber call board
            let winningRow = checkWin board
            index <- index + 1
            winningRow |> Option.map (Utils.Tuples.from3 index call)
        ) sequence

    let getScore (_, call, seq) =
        call * seq

    let findScore sequence boards =
        let winners = boards |> List.choose (runUntilWin sequence)
        let firstWinIndex = winners |> List.minBy Utils.Tuples.first3 |> Utils.Tuples.first3
        let fastestWinners = winners |> List.where (Utils.Tuples.first3 >> ((=)firstWinIndex))
        fastestWinners |> List.map getScore |> List.max
       
    let parseBoard lines =
        let ints = lines |> List.map (String.splitWithOptions " " System.StringSplitOptions.RemoveEmptyEntries >> List.map int)
        Array2D.init 5 5 (fun x y -> (ints.[y][x],false))

    let run (lines:string list) =
        let seqLine = lines.[0]
        let boardLines = lines.[1..]
        let calls = seqLine |> String.split "," |> List.where (String.isEmptyOrWhitespace >> not) |> List.map int
        let boardStrings = 
            boardLines |> 
            List.splitOn String.isEmptyOrWhitespace |> 
            List.where (List.length >> ((<>) 0)) |> 
            List.map (List.where (String.isEmptyOrWhitespace >> not))
        let parsedBoards = boardStrings |> List.map parseBoard
        findScore calls parsedBoards
    
    let findScorePart2 sequence boards =
        let winners = boards |> List.choose (runUntilWin sequence)
        let lastWinIndex = winners |> List.maxBy Utils.Tuples.first3 |> Utils.Tuples.first3
        let lastWinners = winners |> List.where (Utils.Tuples.first3 >> ((=)lastWinIndex))
        lastWinners |> List.map getScore |> List.max
    
    let runPart2 (lines:string list) =
        let seqLine = lines.[0]
        let boardLines = lines.[1..]
        let calls = seqLine |> String.split "," |> List.where (String.isEmptyOrWhitespace >> not) |> List.map int
        let boardStrings = 
            boardLines |> 
            List.splitOn String.isEmptyOrWhitespace |> 
            List.where (List.length >> ((<>) 0)) |> 
            List.map (List.where (String.isEmptyOrWhitespace >> not))
        let parsedBoards = boardStrings |> List.map parseBoard
        findScorePart2 calls parsedBoards

module Day5 =

    let isGridAligned (x1, y1, x2, y2) = x1 = x2 || y1 = y2

    let getPoints (x1,y1,x2,y2) =
        let dx = x2-x1
        let dy = y2-y1
        let len = max (abs dx) (abs dy)
        [0..len] |> List.map (fun i -> (x1 + i * (sign dx), y1 + i * (sign dy)))

    let parseLine line =
        let [p1; p2] = String.split " -> " line
        let [x1; y1] = String.split "," p1 |> List.map int
        let [x2; y2] = String.split "," p2 |> List.map int
        (x1, y1, x2, y2)

    let countAllOverlaps lines =
        let allCells = lines |> List.collect getPoints
        let counts = allCells |> List.countBy id
        let intersections = counts |> List.where (Tuples.second >> ((<)1))
        List.length intersections

    let run input =
        input |> List.map parseLine |> List.where isGridAligned |> countAllOverlaps
    
    let runPart2 input =
        input |> List.map parseLine |> countAllOverlaps
