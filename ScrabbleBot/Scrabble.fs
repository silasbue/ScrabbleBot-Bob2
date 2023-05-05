namespace Bob2

open System.Text.RegularExpressions
open ScrabbleLib
open ScrabbleUtil
open ScrabbleUtil.Dictionary
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint
// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?"
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |>
        Seq.map
            (fun t ->
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State =

    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board         : Parser.board
        dict          : ScrabbleUtil.Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        // parsedBoard   : coord -> bool
        boardTiles    : Map<coord, char>
    }

    let mkState b d pn h bt = {board = b; dict = d;  playerNumber = pn; hand = h; boardTiles = bt }

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    let boardTiles st    = st.boardTiles

module Scrabble =
    open System.Threading
    
    let uintToChar (u: uint32) =
        match u with
        | u -> char (u + 64u)

    let charNumberToPoints (ch: int) = 
        match ch with
        | 0                                             -> 0
        | 1 | 5 | 9 | 12 | 14 | 15 | 18 | 19 | 20 | 21  -> 1
        | 4 | 7                                         -> 2
        | 2 | 3 | 13 | 16                               -> 3
        | 6 | 8 | 22 | 23 | 25                          -> 4
        | 11                                            -> 5
        | 10 | 24                                       -> 8
        | 17 | 26                                       -> 10
        | _                                             -> failwith "Not valid character index"
        
    
    let charToUint (c: char) =
        match c with
        | c -> uint32 c - 64u
        
    let charToInt (c: char) =
        match c with
        | c -> int c - 64
     
    let playableWord (letters: char list) (d: Dict) =
        let rec aux (letters: char list) (d: Dict) (word: string) =
            match letters with 
            | x::xs -> match step x d with
                       | Some (b, d) when b = true -> (word + x.ToString())
                       | Some (b, d) -> aux xs d (word + x.ToString())
                       | None -> aux (xs @ [x]) d word
            | [] -> failwith "todo"
        aux letters d ""
        
    let playableWordWithPrefix (prefixChar: char) (letters: char list) (d: Dict) =
        match step prefixChar d with
        | Some (_, d) -> playableWord letters d
        | None -> failwith "None"
                   
    let rec wordToFirstMove (word: string) (coord: int * int) output =
         match word with
         | s when s <> "" -> wordToFirstMove s[1..] (fst coord, (snd coord + 1)) $"{output} {fst coord} {snd coord} {charToInt s[0]}{s[0]}{charNumberToPoints (charToInt s[0])}"
         | "" -> RegEx.parseMove output

    let rec wordToMove (word: string) (coord : int * int) (isVertical: bool) output =
        match word with
        | s when s <> "" && isVertical -> wordToMove s[1..] (fst coord, (snd coord + 1)) true  $"{output} {fst coord} {snd coord} {charToInt s[0]}{s[0]}{charNumberToPoints (charToInt s[0])}"
        | s when s <> "" -> wordToMove s[1..] (fst coord + 1, (snd coord)) false  $"{output} {fst coord} {snd coord} {charToInt s[0]}{s[0]}{charNumberToPoints (charToInt s[0])}"
        | "" -> RegEx.parseMove output

    let checkRight (st: State.state) =
        st.boardTiles |> Map.filter (fun x _ -> 
            not ((st.boardTiles.ContainsKey (fst x + 1, (snd x)) ||
            st.boardTiles.ContainsKey (fst x + 1, (snd x + 1)) ||
            st.boardTiles.ContainsKey (fst x + 1, (snd x - 1))) || 
            st.boardTiles.ContainsKey (fst x - 1, (snd x))))

    let checkDown (st: State.state) = 
        st.boardTiles |> Map.filter (fun x _ ->
        not(st.boardTiles.ContainsKey (fst x, (snd x + 1)) ||
            st.boardTiles.ContainsKey (fst x - 1, (snd x + 1)) ||
            st.boardTiles.ContainsKey (fst x + 1, (snd x + 1)) ||
            st.boardTiles.ContainsKey (fst x, (snd x - 1))))

    
            
            

    let playGame cstream pieces (st : State.state) =
        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            debugPrint $"current state: {st}\n\n"
            debugPrint $"squares: {st.board.squares}\n\n"
            debugPrint $"hand multiset: {st.hand}\n\n"
            
            let chList = MultiSet.toList st.hand |> List.fold (fun acc x -> acc @ [uintToChar x]) []
            
            debugPrint $"chList: {chList}\n\n"
            let check2 = checkRight st
            let check3 = checkDown st
            debugPrint $"check: {check2}\n\n {check3}\n\n"

            // TODO IMPLEMENT MOVE
            // let move =
            //     match Map.isEmpty st.boardTiles with
            //     | true -> playableWord chList st.dict ""
            //     | false -> 
            
            
            let move = playableWord chList st.dict
            // debugPrint $"Move: {move}\n\n"
            let firstMove = wordToMove move (0,0) false ""
            // debugPrint $"First move: {firstMove}\n\n"
            
            let input =  System.Console.ReadLine()
            // let move = RegEx.parseMove input
            let move = firstMove

            debugPrint $"MOVE: {move}"

            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream (SMPlay move)

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                debugPrint $"\n\nMOVES: {ms}\nPOINTS: {points}\nNEWPIECES: {newPieces}\n\n BOARDTILES: {st.boardTiles}\n\n"
                // hand after removing used pieces
                let removedHand = ms |> List.fold (fun acc x -> MultiSet.removeSingle (fst (snd x)) acc) st.hand
                // new hand after adding new pieces
                let newHand = newPieces |> List.fold (fun acc x -> MultiSet.addSingle (fst x) acc) removedHand
                // board after adding word
                let newBoardTiles = ms |> List.fold (fun acc x -> Map.add (fst x) (fst (snd (snd x))) acc) st.boardTiles

                let st' = State.mkState st.board st.dict st.playerNumber newHand newBoardTiles// This state needs to be updated

                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st


        aux st

    let startGame
            (boardP : boardProg)
            (dictf : bool -> Dictionary.Dict)
            (numPlayers : uint32)
            (playerNumber : uint32)
            (playerTurn  : uint32)
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option)
            (cstream : Stream) =
        debugPrint
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP

        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand
        let boardTiles = Map.empty

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet boardTiles)

