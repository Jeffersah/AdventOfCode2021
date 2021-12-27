namespace AdventOfCode2021
open Utils
open FSharp.Collections.ParallelSeq

module Day21 =
    
    type Result = { winnerScore: int; loserScore: int; turnCount: int; }

    let mod1 v m =
        (v-1) % m + 1

    let stepDice dice =
        (mod1 dice 100) + (mod1 (dice+1) 100) + (mod1 (dice+2) 100), mod1 (dice + 3) 100

    let stepState dice player score =
        let move, dice = stepDice dice
        let player = mod1 (player + move) 10
        dice, player, score + player

    let runUntilScore p0 p1 score =
        let rec run dice p0 s0 p1 s1 turnCount =
            let dice, p0, s0 = stepState dice p0 s0
            if s0 >= score then
                { winnerScore = s0; loserScore = s1; turnCount = turnCount + 1}
            else run dice p1 s1 p0 s0 (turnCount + 1)
        run 1 p0 0 p1 0 0

    let parseLine (line: string) =
        int (line.[28..])

    let run (lines:string list) =
        let p0 = parseLine lines.[0]
        let p1 = parseLine lines.[1]
        let result = runUntilScore p0 p1 1000
        result.turnCount * result.loserScore * 3

    // // // // // PART 2 // // // // //
    // Optimizations implemented:
    //  Do a depth-first search and cache the results (we'll probably hit the same states a lot)
    //  Don't simulate each game individually, instead include a "number of universes" in the game state.

    let diceOutput = [3,1UL; 4,3UL; 5,6UL; 6,7UL; 7,6UL; 8,3UL; 9,1UL];
    
    type GameState = { index0: int; index1: int; score0: int; score1: int }
    let wincounts = Array.init (10*10*21*21) (fun _ -> None)

    let indexFromGameState ({ index0 = i0; index1 = i1; score0 = s0; score1 = s1}) =
        ((i0-1)) + ((i1-1) * 10) + (s0 * 10 * 10) + (s1 * 10 * 10 * 21)

    let rec applyToState ({ index0 = i0; index1 = i1; score0 = s0; score1 = s1}) count (diceRoll, numUniverses) =
        let i0 = mod1 (i0 + diceRoll) 10
        let s0 = s0 + i0;
        if s0 >= 21 then (count * numUniverses, 0UL)
        else 
            let (a,b) = runDfs ({index0 = i1; score0 = s1; index1 = i0; score1 = s0}) (count * numUniverses)
            (b,a)
       
    and runDfs state (count:uint64) =
        let index = indexFromGameState state
        match wincounts.[index] with
        | Some (win,lose) -> win*count, lose*count
        | None -> 
            let tuples = List.map (applyToState state count) diceOutput
            let (rw,rl) = List.reduce (fun (a,b) (c,d) -> (a+c, b+d)) tuples
            Array.set wincounts index (Some (rw / count, rl / count))
            (rw, rl)

    let run2 (lines: string list) =
        let p0 = parseLine lines.[0]
        let p1 = parseLine lines.[1]
        let (win,loss) = runDfs {index0 = p0; index1 = p1; score0 = 0; score1 = 0} 1UL
        max win loss

module Day22 =
    
    let parseLine (line:string) =
        let (Some(grps)) = Regex.groups @"(on|off) x=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)" line
        let state = grps.[0].Value = "on"
        state, int grps.[1].Value, int grps.[2].Value, int grps.[3].Value, int grps.[4].Value, int grps.[5].Value, int grps.[6].Value


    let getCells x0 x1 y0 y1 z0 z1 =
        List.allPairs [y0..y1] [z0..z1] |> List.allPairs [x0..x1] |> List.map (fun (x, (y, z)) -> (x, y, z))

    let runInstr onCells (set, x0, x1, y0, y1, z0, z1) =
        if x0 > 50 || x1 < -50 || y0 > 50 || y1 < -50 || z0 > 50 || z1 < -50 then onCells
        else
            let x0, y0, z0 = max x0 -50, max y0 -50, max z0 -50
            let x1, y1, z1 = min x1 50, min y1 50, min z1 50
            let fn = if set then Set.add else Set.remove
            getCells x0 x1 y0 y1 z0 z1 |> List.fold (fun s v -> fn v s) onCells

    let run (lines: string list) =
        let instrs = lines |> List.map parseLine
        instrs |> List.fold runInstr Set.empty |> Set.count

    // Well, I knew this was coming.
    // PART 2:

    type Range = int64*int64
    type Cuboid = { x: Range; y: Range; z: Range }

    let isRangeEmpty (min,max) = max<min
    let isEmpty ({x = x; y = y; z = z}) = isRangeEmpty x || isRangeEmpty y || isRangeEmpty z

    let rangeIntersection (min0, max0) (min1, max1) =
        let result = (max min0 min1), (min max0 max1)
        if isRangeEmpty result then None else Some result

    let rangeArea (minv:int64, maxv:int64) = maxv - minv + 1L

    let area ({ x = x; y = y; z = z }:Cuboid) = rangeArea x * rangeArea y * rangeArea z

    let rangeDifference (min0, max0) (min1, max1) =
        match rangeIntersection (min0, max0) (min1, max1) with
        | None -> [(min0, max0)]
        | Some (imin, imax) -> 
            [(min0, imin - 1L); (imax + 1L, max0)] |> List.where (isRangeEmpty >> not)

    let intersection (a:Cuboid) (b:Cuboid) =
        match (rangeIntersection a.x b.x, rangeIntersection a.y b.y, rangeIntersection a.z b.z) with
        | Some x, Some y, Some z -> Some { x=x; y=y; z=z }
        | _,_,_ -> None

    let difference (a:Cuboid) (b:Cuboid) = 
        match intersection a b with
        | None -> [a]
        | Some i ->
            let xCuboids = [for xr in rangeDifference a.x b.x do {a with x = xr}] 
            let yCuboids = [for yr in rangeDifference a.y b.y do {a with x = i.x; y = yr}]
            let zCuboids = [for zr in rangeDifference a.z b.z do {x = i.x; y = i.y; z = zr}]
            List.where (isEmpty >> not) (xCuboids @ yCuboids @ zCuboids)

    let parseLine2 (line:string) =
        let (Some(grps)) = Regex.groups @"(on|off) x=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)" line
        let state = grps.[0].Value = "on"
        state, { x = (int64 grps.[1].Value, int64 grps.[2].Value); y = (int64 grps.[3].Value, int64 grps.[4].Value); z = (int64 grps.[5].Value, int64 grps.[6].Value)}

    let rec applyActivationLine exclList onRegions cuboid =
        match exclList with
        | [] -> cuboid::onRegions
        | f::r ->
            let excl = difference cuboid f
            List.fold (applyActivationLine r) onRegions excl

    let applyLine (exclList, onRegions) (on, cuboid) =
        match on with
        | false -> (cuboid::exclList, onRegions)
        | true -> (cuboid::exclList, applyActivationLine exclList onRegions cuboid)

    let run2 lines =
        let toRun = lines |> List.rev |> List.map parseLine2
        let _, on = List.fold applyLine ([], []) toRun
        on |> List.map area |> List.sum

module Day23 =

    // I'll probably revisit this problem to try to find an actual programmable solution.
    // It's tricky, I have the general idea of how to solve them, but it's difficult to formalize. The idea is:
    //      1) If any amphipod can move to it's destination, do that.
    //      2) Otherwise, find the amphipod closest to it's destination, and:
    //          a) find every amphipod in the way of that movement
    //          b) move each of them so it's now out of the way*
    //          c) Move the closest amphipod to it's destination and go back to step 1.
    // 2b is the tricky part. You want to move everything the minimum amount possible, but also not block yourself in later.
    // For example, in part 2:
    // ...........
    //   C C A B  
    //   D C B A
    //   D B A C
    //   D D B A
    // The closest move to destination is this 'C':
    // ...........
    //   C[C]A B  
    //   D C B A
    //   D B A C
    //   D D B A
    // Which means it must offset ABAB. The least energy costly way to do that (without blocking [C]) is:
    // .A.A...B.B.
    //   C[C]. B  
    //   D C . A
    //   D B . C
    //   D D . A
    // And then we get
    // .A.A...B.B.
    //   C . . B  
    //   D . . A
    //   D B C C
    //   D D C A
    // Then we are stuck. We'd have to move everybody more out of the way to get more moves:
    // AA.....B.B.
    //   . . . B  
    //   D . C A
    //   D B C C
    //   D D C A
    // And then we're stuck again, because we want to swap B and D, and it seems like D should always move towards it's destination (since it costs so much to move)
    // But we actually want to move it the other way to avoid a deadlock
    // AA.D.B.B.B.
    //   . . . B  
    //   D . C A
    //   D . C C
    //   D . C A
    // Then fill in the B's...
    // AA.D.......
    //   . B . .  
    //   D B C A
    //   D B C C
    //   D B C A
    // Displace A for the C move
    // AA.D.....A.
    //   . B C .  
    //   D B C .
    //   D B C .
    //   D B C A
    // Displace A again for the D move (but we should've moved the first A further)
    // AA.......AA
    //   . B C .  
    //   D B C .
    //   D B C .
    //   D B C D
    // Repeat step 1:
    // ...........
    //   A B C D  
    //   A B C D
    //   A B C D
    //   A B C D
    // I think a programmable solution like that would work, but I'm not even sure if that works in the general case: It seems to more-or-less discard the actual costs of moving each type of amphipod.
    // Not sure though.


    // Some notes:
    // Each amphipod moves at most twice (Either start->destination or start->hallway->destination)
    // Vertical distances don't matter. All that matters is left-to-right
    //  Similarly, distance to the room doesn't matter. All that matters is moves AWAY from the destination, or moves PAST the destination in the hallway.
    // Once an amphipod moves to it's destination, it never moves again. 

    type State = { leftStack: int list; rightStack: int list; rooms: int list list}

    // Moves that we care about are:
    // 1) Move from a room to the left stack
    // 2) Move from a room to the right stack
    // 3) Move from a room to a room
    // 4) Move off the left stack to a room
    // 5) Move off the right stack to a room
    type Move = RoomToLeft of int | RoomToRight of int | RoomToRoom of int*int | LeftToRoom of int | RightToRoom of int

    let isFinished { leftStack = leftStack; rightStack = rightStack; rooms = rooms} =
        match leftStack, rightStack, rooms with
        | [], [], [[]; []; []; []] -> true
        | _ -> false

    let getAccessableRooms leftStack rightStack =
        let minRoom = List.length leftStack - 2 |> max 0
        let maxRoom = 3 - (List.length rightStack - 2 |> max 0)
        [minRoom..maxRoom]
     
    let rec simplifyState state (moves: Move list) =
        let { leftStack = leftStack; rightStack = rightStack; rooms = rooms} = state
        let accessible = getAccessableRooms leftStack rightStack
        match leftStack, rightStack with 
        | a::l, _ when List.contains a accessible && rooms.[a] = [] ->  // Left stack can move to its target room
            simplifyState { state with leftStack = l } (LeftToRoom a::moves)
        | _, b::r when List.contains b accessible && rooms.[b] = [] -> // Right stack can move to its target room
            simplifyState { state with rightStack = r;} (RightToRoom b::moves)
        | _, _ ->
            let validMoves = accessible |> List.tryFind (fun i ->
                match rooms.[i] with
                | f::r when List.contains f accessible && rooms.[f] = [] -> true
                | _ -> false
            )
            match validMoves with
            | None -> (state, moves |> List.rev)
            | Some (i) -> 
                let f::r = rooms.[i]
                simplifyState ({ state with rooms = List.replace i r rooms }) ((RoomToRoom (i,f))::moves)
            
    let initialSimplify state =
        let { rooms = rooms } = state
        let rooms = rooms |> List.mapi (fun index items -> List.rev items |> List.skipWhile ((=)index) |> List.rev)
        {state with rooms = rooms}

    //   C C A B  
    //   D C B A
    //   D B A C
    //   D D B A

    let inputState = { leftStack = []; rightStack = []; rooms = [[1;0];[2;3];[1;2];[3;0]] }
    let inputState2 = { leftStack = []; rightStack = []; rooms = [[2;3];[2;3];[0;1];[1;0]] }
    //let inputState3 = { leftStack = []; rightStack = []; rooms = [[1;0];[2;3];[1;2];[3;0]] }
    let inputState4 = { leftStack = []; rightStack = []; rooms = [[2;3;3;3];[2;2;1;3];[0;1;0;1];[1;0;2;0]] }

    let toString { leftStack = leftStack; rightStack = rightStack; rooms = rooms } =
        let toChar digit = match digit with | 0 -> "A" | 1 -> "B" | 2 -> "C" | 3 -> "D" | _ -> "."
        let lstr = leftStack |> List.map toChar |> List.rev |> String.concat ""
        let rstr = rightStack |> List.map toChar |> String.concat ""
        let padStr = System.String('.', 8 - lstr.Length - rstr.Length)
        let roomString i = rooms |> List.map (List.tryGet i >> Option.map toChar >> Option.defaultValue ".") |> String.concat ""
        lstr + padStr + rstr + "\r\n  " + (List.map roomString [0..3] |> String.concat "\r\n  ")

    let getOutwardMoves state =
        let { leftStack = leftStack; rightStack = rightStack; rooms = rooms} = state
        let accessible = getAccessableRooms leftStack rightStack
        accessible |> 
            List.collect (fun index ->
                match rooms.[index] with
                | [] -> []
                | f::r -> 
                    let newState = { state with rooms = List.replace index r rooms }
                    [(RoomToLeft index, {newState with leftStack = f::leftStack}); (RoomToRight index,{newState with rightStack = f::rightStack})]) |> 
            List.map (fun (move, state) -> 
                let simpState, simpMoves = simplifyState state []
                move::simpMoves, simpState)

    let rec dfs state (moves:Move list) =
        //printfn "%A" moves
        //printfn "%s" (toString state)
        //System.Console.ReadKey(true)
        if isFinished state then [moves]
        else 
            let outMoves = getOutwardMoves state
            match outMoves with
            | [] -> []
            | l -> 
                List.collect (fun (mv, newState) -> dfs newState (moves@mv)) l

    let scoreForMove i =
        match i with
        | 0 -> 1
        | 1 -> 10
        | 2 -> 100
        | 3 -> 1000

    type ScoreState = { state: State; leftOffset: int option; rightOffset: int option }

    let getMoveScore move state =
        let { state = { leftStack = leftStack; rightStack = rightStack; rooms = rooms }; leftOffset = left; rightOffset = right} = state
        // TODO
        0,state

    let rec score state moveList scoreSoFar =
        match moveList with
        | [] -> scoreSoFar
        | move::rest ->
            let mvScore,state = getMoveScore move state
            score state rest (scoreSoFar + mvScore)

    let getScore state moveList =
        score { state = state; leftOffset = None; rightOffset = None } moveList 0

    let run _ =
        let input = inputState;
        let solutions = dfs (initialSimplify input) [] |> List.sortBy List.length
        printfn "Got %i solution(s)" (List.length solutions)
        let minSolution = List.minBy (fun moveList -> getScore input moveList) solutions
        let minScore = getScore input minSolution
        (minScore, minSolution)
        
module Day24 =

    type Op = Add | Mul | Div | Mod | Eq
    type OpRecord = { operation: Op; left: Instruction; right: Instruction; minValue: int64; maxValue: int64 }
    and Instruction = Input of int | Value of int64 | Operation of OpRecord
    type Variable = X | Y | Z | W

    let initialState = Map.ofSeq [X, Value 0; Y, Value 0; Z, Value 0; W, Value 0]

    let getVar (str:string) =
        match str with
        | "x" -> X
        | "y" -> Y
        | "z" -> Z
        | "w" -> W

    let getVarOrValue (str: string) = 
        match str with
        | "x" -> Ok X
        | "y" -> Ok Y
        | "z" -> Ok Z
        | "w" -> Ok W
        | _ -> Error (int str)

    let getTreeOrInput state str =
        match getVarOrValue str with
        | Ok v -> Map.find v state
        | Error i -> Value i

    let getMinMax tree =
        match tree with
        | Value v -> v,v
        | Input _ -> 1,9
        | Operation { minValue = min; maxValue = max } -> min,max
    
    let simplifyTree op left right =
        // Here's all the simplifications we can perform...
        let minl, maxl = getMinMax left
        let minr, maxr = getMinMax right

        let reduced = 
            match op,left,right with
            // Any op where both sides are values can be pre-calculated
            | Add, Value x, Value y -> Some (Value (x + y))
            | Mul, Value x, Value y -> Some (Value (x * y))
            | Div, Value x, Value y -> Some (Value (x / y))
            | Mod, Value x, Value y -> Some (Value (x % y))
            | Eq, Value x, Value y -> Some (Value (if x = y then 1L else 0L))
            // Addition: Anything + 0 is itself
            | Add, Value 0L, v
            | Add, v, Value 0L -> Some v
            // Multiplication: Anything * 0 is 0, Anything * 1 is itself.
            | Mul, Value 0L, _
            | Mul, _, Value 0L -> Some (Value 0L)
            | Mul, Value 1L, v
            | Mul, v, Value 1L -> Some (v)
            // Division: Anything / 1 is itself. 0 / anything is 0.
            | Div, v, Value 1L -> Some (v)
            | Div, Value 0L, _ -> Some (Value 0L)
            // Mod: Anything % 1 is 0. 0 % anything is 0, 1 % anything is 0.
            | Mod, Value 0L, _
            | Mod, Value 1L, _
            | Mod, _, Value 1L -> Some(Value 0L)
            // Can't do any value optimizations for Eq unfortunately.

            // Range optimizations:
            // Anything mod a number bigger than itself is itself
            | Mod, left, Value v when minl >= 0 && minl < v -> Some(left)
            // If the two ranges don't intersect, they cannot equal 1.
            | Eq, _, _ when minl > maxr || minr > maxl -> Some(Value 0L)
            // (x = v) = 1 is just (x=v)
            | Eq, v, Value 1L when minl = 0 && maxl = 1 -> Some v
            | Eq, Value 1L, v when minr = 0 && maxr = 1 -> Some v
            | _ -> None

        match reduced with
        | Some r -> r
        | None -> 
            let minV, maxV = match op with
                             | Add -> minl + minr, maxl + maxr
                             | Mul -> 
                                let products = List.allPairs [minl;maxl] [minr;maxr] |> List.map (fun (a,b) -> a*b)
                                List.min products, List.max products
                             | Div -> 
                                let products = List.allPairs [minl;maxl] [minr;maxr] |> List.map (fun (a,b) -> a/b)
                                List.min products, List.max products
                             | Mod ->
                                let v = max (abs minr) (abs maxr)
                                (if minl < 0 then -v else 0),(if maxl > 0 then v else 0)
                             | Eq -> 0,1
            if minV = maxV 
            then Value minV
            else Operation { operation = op; left = left; right = right; minValue = minV; maxValue = maxV }

    // Determines whether a tree is "dangerous," IE: Could produce an x / 0 or x % 0
    let rec isTreeDangerous tree =
        match tree with
        | Operation { operation = Div; left = left; right = right; } ->
            let minr, maxr = getMinMax right
            if minr <= 0 && maxr >= 0 then true
            else isTreeDangerous left || isTreeDangerous right
        | Operation { operation = Mod; left = left; right = right; } ->
            let minr, _ = getMinMax right
            if minr <= 0 then true
            else isTreeDangerous left || isTreeDangerous right
        | Operation { left = left; right = right } ->
            isTreeDangerous left || isTreeDangerous right
        | _ -> false

    let buildParseTree (intrs:string list) =
        let tree, _ = ((initialState, 0),intrs) ||> List.fold (fun (state, nextDigit) instr ->
                let rgx = Regex.groups "^(...) ([^ ]+)( [^ ]+)?$" instr
                match rgx with
                | Some [Some "inp"; Some v1; None] ->
                   let var = getVar v1
                   Map.change var (fun _ -> Some (Input nextDigit)) state, nextDigit + 1
                | Some [Some op; Some v1; Some v2] ->
                    let op = match op with
                        | "add" -> Add
                        | "mul" -> Mul
                        | "div" -> Div
                        | "mod" -> Mod
                        | "eql" -> Eq
                    let resVar = getVar v1
                    let leftTree = getTreeOrInput state v1
                    let rightTree = getTreeOrInput state (v2.[1..])
                    Map.change resVar (fun _ -> Some (simplifyTree op leftTree rightTree)) state, nextDigit
                | _ -> failwithf "Unrecognized instruction %s" instr
            )
        tree


    let rec toString tree = 
        match tree with
        | Input i -> sprintf "input[%i]" i
        | Value v -> string v
        | Operation { operation = op; left = left; right = right } ->
            let opchr = match op with | Add -> "+" | Mul -> "*" | Div -> "/" | Mod -> "%" | Eq -> "=="
            sprintf "(%s %s %s)" (toString left) opchr (toString right)

    let rec replaceInput inputIndex value tree =
        let replaceRecursive = replaceInput inputIndex value
        match tree with
        | Input i when i = inputIndex -> Value value
        | Operation { operation = op; left = left; right = right } ->
            let left,right = replaceRecursive left, replaceRecursive right
            simplifyTree op left right
        | t -> t

    let rec tryFindValue vlist tree inputIndex valuesSoFar = 
        match tree with
        | Value v when v = 0 -> Some (valuesSoFar)
        | Value v when v <> 0 -> None
        | _ ->
            let min,max = getMinMax tree
            if 0L > max || 0L < min then None
            else if isTreeDangerous tree then None
            else 
                printfn "%s" (valuesSoFar |> List.rev |> List.map string |> String.concat "" |> String.padRight 14 '.')
                vlist |>
                    List.tryPick (fun value -> tryFindValue vlist (replaceInput inputIndex value tree) (inputIndex + 1) (value::valuesSoFar))

    let run (lines:string list) =
        printfn "Building tree..."
        let trees = buildParseTree lines
        match tryFindValue (List.rev [1..9]) trees.[Z] 0 [] with
        | None -> failwith "No solutions!"
        | Some (l) -> List.rev l |> List.map string |> String.concat ""

    
    let run2 (lines:string list) =
        printfn "Building tree..."
        let trees = buildParseTree lines
        match tryFindValue [1..9] trees.[Z] 0 [] with
        | None -> failwith "No solutions!"
        | Some (l) -> List.rev l |> List.map string |> String.concat ""

module Day25 =
    type Cell = Empty | Right | Down

    let halfstep fromBoard getTgt toBoard =
        Array2D.fill Empty toBoard
        fromBoard |> Array2D.mapi (fun x y dir -> 
            match getTgt x y dir fromBoard with
            | Some (tx, ty) ->
                match fromBoard.[tx,ty] with
                | Empty ->
                    Array2D.set toBoard tx ty dir
                    true
                | _ ->
                    Array2D.set toBoard x y dir
                    false
            | None -> 
                false
        ) |> Array2D.toSeq |> Seq.exists id

    let getRightTarget x y d board =
        match d with
        | Right -> Some((x + 1) % Array2D.length1 board, y)
        | Down -> Some (x,y)
        | _ -> None
        
    let getDownTarget x y d board =
        match d with
        | Down -> Some(x, (y + 1) % Array2D.length2 board)
        | Right -> Some (x,y)
        | _ -> None

    let step fromBoard toBoard =
        let hl = halfstep fromBoard getRightTarget toBoard
        let hr = halfstep toBoard getDownTarget fromBoard
        hl||hr

        
    let printBoard board =
        let w,h = Array2D.size board
        [0..h-1] |> List.iter (fun y ->
            [0..w-1] |> List.iter (fun x ->
                printf "%c" (match board.[x,y] with | Right -> '>' | Down -> 'V' | Empty -> '.'))
            printfn ""
        ) 

    let runUntilNoChange board =
        let tmpBoard = Array2D.copy board
        let rec runRec fromBoard toBoard iter =
            //printfn "Step %i: " iter
            //printBoard fromBoard
            if step fromBoard toBoard
            then runRec fromBoard toBoard (iter+1)
            else iter
        runRec board tmpBoard 1


    let run lines =
        let board = lines |> List.map (fun str -> str |> List.ofSeq |> List.map (fun ch -> match ch with | '>' -> Right | 'V' | 'v' -> Down | _ -> Empty)) |> array2D |> Array2D.transpose
        runUntilNoChange board