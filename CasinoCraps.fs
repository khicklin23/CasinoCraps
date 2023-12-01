open System

type GameState =
    | ComeoutRoll
    | PointPhase of int

let mutable gameState = ComeoutRoll
let mutable playerBalance = 500.00
let minBet = 10.00
let maxBet = 200.00
let mutable passLineBet = 0.00
let mutable dontPassLineBet = 0.00
let mutable comeLineBet = 0.00
let mutable dontComeLineBet = 0.00

let mutable comeLinePoint = 0

let mutable passLinePoint = 0

let mutable firstRoll = true

let mutable ComePointSet = false

let mutable dontComeBetting = false

let mutable comeBetting = false

// Function to display dice in ASCII art
let displayDice value =
    match value with
    | 1 ->
        printfn "+-------+"
        printfn "|       |"
        printfn "|   *   |"
        printfn "|       |"
        printfn "+-------+"
    | 2 ->
        printfn "+-------+"
        printfn "| *     |"
        printfn "|       |"
        printfn "|     * |"
        printfn "+-------+"
    | 3 ->
        printfn "+-------+"
        printfn "| *     |"
        printfn "|   *   |"
        printfn "|     * |"
        printfn "+-------+"
    | 4 ->
        printfn "+-------+"
        printfn "| *   * |"
        printfn "|       |"
        printfn "| *   * |"
        printfn "+-------+"
    | 5 ->
        printfn "+-------+"
        printfn "| *   * |"
        printfn "|   *   |"
        printfn "| *   * |"
        printfn "+-------+"
    | 6 ->
        printfn "+-------+"
        printfn "| *   * |"
        printfn "| *   * |"
        printfn "| *   * |"
        printfn "+-------+"
    | _ -> printfn "Invalid dice value"

// Function to simulate dice roll and display dice
let rollDiceAndDisplay() =
    let rand = Random()
    let dice1 = rand.Next(1, 7)
    let dice2 = rand.Next(1, 7)
    printfn "\n\nDice 1:"
    displayDice dice1
    printfn "Dice 2:"
    displayDice dice2
    (dice1, dice2)

// Function to place a bet
let placeBet betType =
    printfn "\nEnter your bet amount:\n"
    let betAmount = Double.Parse(Console.ReadLine())

    if betAmount < minBet || betAmount > maxBet || betAmount > playerBalance then
        printfn "Invalid bet. It must be between $%.2f and $%.2f and within your balance." minBet maxBet
        false
    else
        playerBalance <- playerBalance - betAmount
        match betType with
        | "Pass Line" -> passLineBet <- betAmount
        | "Don't Pass Line" -> dontPassLineBet <- betAmount
        | "Come" -> comeLineBet <- betAmount
        | "Don’t Come" -> dontComeLineBet <- betAmount
        | _ -> ()
        true

// Function to process the comeout roll
let processComeoutRoll rollSum =
    match rollSum with
    | 7 | 11 ->
        playerBalance <- playerBalance + passLineBet + passLineBet
        if passLineBet > 0 then
          printfn "\nPass Line wins! +$%.2f\n" passLineBet
        if dontPassLineBet > 0 then
          printfn "\nDon't Pass Line loses! -$%.2f\n" dontPassLineBet
        passLineBet <- 0.0
        dontPassLineBet <- 0.0
        passLinePoint <- 0
        gameState <- ComeoutRoll
    | 2 | 3 | 12 ->
        playerBalance <- playerBalance + dontPassLineBet + dontPassLineBet
        if dontPassLineBet > 0 then
          printfn "\nDon't Pass Line wins!! +$%.2f\n" dontPassLineBet
        if passLineBet > 0 then
          printfn "\nPass Line loses!! -$%.2f\n" passLineBet
        passLinePoint <- 0  
        passLineBet <- 0.0
        dontPassLineBet <- 0.0
        gameState <- ComeoutRoll
    | _ ->
        passLinePoint <- rollSum
        gameState <- PointPhase rollSum
        printfn "\nThe point is now %d" rollSum

// Function to process the point phase roll
let processPointPhaseRoll point rollSum =
    if (comeLineBet > 0 || dontComeLineBet > 0) then
        if firstRoll = true then
            if rollSum = 7 || rollSum = 11 then
                printfn "\nCome bet wins! +$%.2f\n" comeLineBet
                playerBalance <- playerBalance + comeLineBet + comeLineBet
                comeLineBet <- 0.0
                dontComeLineBet <- 0.0
            elif rollSum = 2 || rollSum = 3 || rollSum = 12 then
                printfn "\nCome bet loses! -$%.2f\n" dontComeLineBet
                playerBalance <- playerBalance + dontComeLineBet + dontComeLineBet
                comeLineBet <- 0.0
                dontComeLineBet <- 0.0
            else
                printfn "The Come point is set to %d" rollSum
                firstRoll <- false
                ComePointSet <- true
                comeLinePoint<- rollSum
        elif ComePointSet = true then
            if rollSum = comeLinePoint then
                if comeLineBet > 0 then
                    printfn "\nCome bet wins! +$%.2f\n" comeLineBet
                    comeLineBet <- 0.0
                    ComePointSet <- false
                    comeLinePoint <- 0
                if dontComeLineBet > 0 then
                    printfn "\nDon't Come bet loses! -$%.2f\n" dontComeLineBet
                    ComePointSet <- false
                    comeLinePoint <- 0
                    dontComeLineBet <- 0.0

    if rollSum = point then
        if passLineBet > 0.00 then
            playerBalance <- playerBalance + passLineBet + passLineBet
            printfn "Pass Line wins! +$%.2f" passLineBet
            passLineBet <- 0.0
            passLinePoint <- 0
            dontPassLineBet <- 0.0
            gameState <- ComeoutRoll
    if rollSum = 7 then       
        if comeLineBet > 0 then
            printfn "\nCome bet loses! -$%.2f\n" comeLineBet
        if dontComeLineBet > 0 then
            printfn "\nDon't Come bet Wins! +$%.2f\n" dontComeLineBet 
        if dontPassLineBet > 0 then
            printfn "\nDon't Pass bet Wins! +$%.2f\n" dontPassLineBet
            playerBalance <- playerBalance + dontPassLineBet + dontPassLineBet
        passLineBet <- 0.0
        passLinePoint <- 0
        dontPassLineBet <- 0.0
        ComePointSet <- false
        comeLineBet <- 0.0
        dontComeLineBet <- 0   
        gameState <- ComeoutRoll    
    else
        printfn "\nRoll again..." 

// Betting Menu
let bettingMenu() =
    printfn "\nPlayer Balance: $%.2f" playerBalance
    match gameState with
    | ComeoutRoll ->
        printfn "\n1. Pass Line Bet\n2. Don’t Pass Line Bet\n3. Roll the Dice\n0. Quit Game"
        Console.ReadLine()
    | PointPhase point ->
        printfn "                 Don't"
        printfn "   Point   Come   Come"
        printfn "   =====  =====  ====="
        let pointsList = ["4"; "5"; "6"; "8"; "9"; "10"]
        let pointAsString = string point

        for p in pointsList do
            if p = pointAsString then
                printfn "On     %s" p
            else
                printfn "       %s" p
        if comeLinePoint <> 0 then
            printfn "\n\nThe Current Come Line Point is: %d" comeLinePoint
        printfn "\n\n1. Come Line Bet\n2. Don’t Come Line Bet\n3. Roll the Dice\n0. Quit Game"
        Console.ReadLine()

// Main game loop
let rec gameLoop() =
    if playerBalance <= 0.0 then
        printfn "Game Over. You've run out of funds."
    elif passLinePoint = 0 then
        let choice = bettingMenu()
        match choice with
        | "1" ->
            if placeBet "Pass Line" then gameLoop() else ()
        | "2" ->
            if placeBet "Don't Pass Line" then gameLoop() else ()
        | "3" ->
            let (dice1, dice2) = rollDiceAndDisplay()
            let rollSum = dice1 + dice2
            printfn "Total roll: %d" rollSum
            match gameState with
            | ComeoutRoll -> processComeoutRoll rollSum; gameLoop()
            | PointPhase point -> processPointPhaseRoll point rollSum; gameLoop()
        | "0" -> // Quit game option
            printfn "Game Over. Final Balance: $%.2f" playerBalance
        | _ ->
            printfn "Invalid choice. Please select a valid option."
            gameLoop()
    else
        let choice = bettingMenu()
        match choice with
        | "1" ->
            if placeBet "Come" then 
                comeBetting <- true
                gameLoop() 
            else ()
        | "2" ->
            if placeBet "Don’t Come" then 
                dontComeBetting <- true
                gameLoop() 
            else ()
        | "3" ->

            let (dice1, dice2) = rollDiceAndDisplay()
            let rollSum = dice1 + dice2
            printfn "Total roll: %d" rollSum
            match gameState with
            | ComeoutRoll -> processComeoutRoll rollSum; gameLoop()
            | PointPhase point -> processPointPhaseRoll point rollSum; gameLoop()
        | "0" -> // Quit game option
            printfn "Game Over. Final Balance: $%.2f" playerBalance
        | _ ->
            printfn "Invalid choice. Please select a valid option."
            gameLoop()
// Start the game
gameLoop()
