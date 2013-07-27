namespace TuringDrawings

[<AutoOpen>]
module Utils =
    /// Generate a random integer within [a, b]
    let randomInt =
        let rand = System.Random()
        fun (a,b) -> a + rand.Next(b-a+1)

[<AutoOpen>]
module States =
    let [<Literal>] ACTION_LEFT  = 0
    let [<Literal>] ACTION_RIGHT = 1
    let [<Literal>] ACTION_UP    = 2
    let [<Literal>] ACTION_DOWN  = 3
    let [<Literal>] NUM_ACTIONS  = 4

type Program(numStates, numSymbols, mapWidth, mapHeight) =
    do if numStates < 1 then invalidArg "numStates" "must have at least 1 state"
    do if numSymbols < 2 then invalidArg "numSymbols" "must have at least 2 symbols"
    /// Transition table
    let table = Array.zeroCreate (numStates * numSymbols * 3)
    /// Map (2D tape)
    let map = Array.zeroCreate (mapWidth * mapHeight) 

    let setTrans (st0, sy0, st1, sy1, ac1) =
        let idx = (numStates * sy0 + st0) * 3
        table.[idx+0] <- st1
        table.[idx+1] <- sy1
        table.[idx+2] <- ac1

    // Generate random transitions
    do for st = 0 to numStates-1 do
        for sy = 0 to numSymbols-1 do
            setTrans(
                st,
                sy,
                randomInt(0, numStates - 1),
                randomInt(1, numSymbols - 1),
                randomInt(0, NUM_ACTIONS - 1))

    let mutable state = 0
    let mutable xPos = 0
    let mutable yPos = 0
    let mutable itrCount = 0

    let reset () =
        // Start state
        state <- 0
        // Top-left corner
        xPos <- 0;
        yPos <- 0;
        // Iteration count
        itrCount <- 0;
        // Initialize the image
        Array.fill map 0 map.Length 0

    // Initialize the state
    do reset()

    let iteration () = 
        let sy = map.[mapWidth * yPos + xPos]
        let st = state

        let idx = (numStates * sy + st) * 3
        let st = table.[idx + 0]
        let sy = table.[idx + 1]
        let ac = table.[idx + 2]

        // Update the current state
        state <- st

        // Write the new symbol
        map.[mapWidth * yPos + xPos] <- sy

        // Perform the transition action
        match ac with
        | ACTION_LEFT ->
            xPos <- xPos + 1
            if xPos >= mapWidth
            then xPos <- xPos - mapWidth
        | ACTION_RIGHT ->
            xPos <- xPos - 1
            if (xPos < 0)
            then xPos <- xPos + mapWidth     
        | ACTION_UP ->
            yPos <- yPos - 1
            if (yPos < 0)
            then yPos <- yPos + mapHeight    
        | ACTION_DOWN ->
            yPos <- yPos + 1
            if yPos >= mapHeight
            then yPos <- yPos - mapHeight   
        | _ ->
            failwith (sprintf "invalid action: %d" ac)

    let update (numItrs) =
        for i = 0 to numItrs-1 do iteration ()
        itrCount <- itrCount + numItrs

    member program.Reset() = reset ()
    member program.Update(n) = update n
    member program.Map = map
