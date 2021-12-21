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