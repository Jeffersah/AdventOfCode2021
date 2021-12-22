namespace AdventOfCode2021
open Utils

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