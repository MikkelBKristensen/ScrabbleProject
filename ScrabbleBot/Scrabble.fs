﻿namespace LetterRip

open MultiSet
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO
open StateMonad
open ScrabbleUtil.DebugPrint
open LetterRip.Util



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
    // Currently, it only keeps track of your hand, your player number, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board            : Parser.board
        dict             : ScrabbleUtil.Dictionary.Dict
        playerNumber     : uint32
        hand             : MultiSet.MultiSet<uint32>
        playerAmount     : uint32
        playersTurn      : uint32
        forfeitedPlayers : Set<uint32>
        playedTiles    : Map<coord,char >
    }
    let mkState b d pn h pa pt fp  pl =
        {board = b; dict = d;  playerNumber = pn; hand = h; playerAmount = pa; playersTurn = pt; forfeitedPlayers = fp; playedTiles = pl }
    
    let updateTurn (pid:uint32) (pAmount:uint32)  = ((pid + 1u) % pAmount) + 1u

    // Removes the used pieces from the hand
    let removeUsedPieces (hand : MultiSet.MultiSet<uint32>) (ms : (coord * (uint32 * (char * int))) list) =
        List.fold (fun acc x -> MultiSet.removeSingle (fst (snd x)) acc) hand ms

    // Adds the new pieces to the hand
    let addNewPieces (newPieces : (uint32 * uint32) list) (hand : MultiSet.MultiSet<uint32>) =
        List.fold (fun acc x -> MultiSet.addSingle (fst x) acc) hand newPieces
    
    // Updates the hand using the methods above
    let updateHand (hand : MultiSet.MultiSet<uint32>) (ms: (coord * (uint32 * (char * int))) list) (newPieces : (uint32 * uint32) list) =
        // First remove the used pieces, then add the new ones to updated hand
        removeUsedPieces hand ms |> addNewPieces newPieces
    
    let updatePlayedLetters (PL : Map<coord,char >)  (ms : (coord * (uint32 * (char * int))) list)=
        List.fold (fun acc (x, y) -> Map.add (x) (fst (snd y)) acc) PL ms

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    let playerAmount st  = st.playerAmount
    let playersTurn st   = st.playersTurn
    let forfeitedPlayers st = st.forfeitedPlayers
    let playedLetters st = st.playedTiles


module FindMove =
    
    (*
        The method to find a word to start the game of with, is buggy -
        cIDlist is not backtracked properly, so we are limiting ourselves in possibilities of spelling a new word,
        if the first "try" isn't correct.
    
    *)
    let FindBestWordOnHand (st : State.state) =

        let rec removeTail list =
            match list with
            | [] -> []
            | [_] -> [] // If there's only one element, return an empty list
            | head :: tail -> head :: removeTail tail

        
        let rec tryAssembleWord (permCIdList: uint32 list) (cIdList: uint32 list) (hand: MultiSet.MultiSet<uint32>) (dict: Dictionary.Dict) (word: (uint32 *(char * int)) list) =
            match cIdList with
            | [] -> List.Empty //When calling method and this is the result, we either swap or pass
            | head :: rest ->
                let step = Dictionary.step (multisetUtil.cIdToChar head) dict // Perform step
                match step with
                | None -> // No word with head char; Try again with rest
                    match tryAssembleWord permCIdList rest hand dict word with
                    | [] ->
                        if word.IsEmpty then
                            match permCIdList with
                            | h :: t ->
                                if head < h then
                                    tryAssembleWord (List.append t [h]) (List.append t [h]) st.hand dict []
                                else
                                    List.Empty
                        else
                            tryAssembleWord permCIdList (List.append rest [head]) hand dict (removeTail word) // Retry with the rest of the list and clear the current word
                    | result -> result // Return the result obtained from the recursive call
                | Some x ->
                    let word = List.append word [(head,(multisetUtil.cIdToChar head, multisetUtil.cIdToPV head))]
                    if fst x then // Indicates a finished word; Hand doesn't need updating internally
                        word
                    else
                        let hand = MultiSet.removeSingle head hand // Removes tile from hand
                        let cIdList = MultiSet.keys (hand) // Updates cIdList
                        tryAssembleWord permCIdList cIdList hand (snd x) word // Recursive call
                        
        
        let rec assignCoords (coord: (int * int)) (advance: (int * int)) (move: ((int * int) * (uint32 * (char * int))) list) (word: (uint32 *(char * int)) list) =
            match word with
            | [] -> move // return empty word for passing/swapping
            | head :: rest ->
                let move = List.append move [(coord, head)]
                let coord = ((fst coord)+(fst advance), (snd coord)+(snd advance))
                assignCoords coord advance move rest
                
        let cIdList = MultiSet.keys (st.hand)
        tryAssembleWord cIdList cIdList st.hand st.dict List.Empty |> assignCoords (0,0) (1,0) List.Empty
    
            
        

    
    let FindBestWordOnBoard (st : State.state) = failwith "not implemented"
    (*
    let FindWordFromHand st  : char list =
        
        let rec wordBuilder =
            let word = 
            let charSet = multisetUtil.handToCharMultiset st.hand
            
            List.fold (fun (word : string) ())
  
    
    let decisionStarter (st:State.state) =
        if st.playedTiles.IsEmpty then
            FindWordFromHand st
        else 
            FindBestWordOnBoard st
            
        // match st.playedTiles with
        // | Map.tryFind -> FindBestWordOnHand hand
        // | _  -> FindBestWordOnBoard playedLetters
        
        *)  
    
module Scrabble =
    open System.Threading

    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            forcePrint $"Playerturn {State.playersTurn st}\n"
            forcePrint $"Playernumber {State.playerNumber st}\n"

            if State.playerNumber st = State.playersTurn st then
                forcePrint "Your turn!\n"
                let word = FindMove.FindBestWordOnHand st // If empty word is returned, pass or swap
                //forcePrint (string word)
                //let input =  System.Console.ReadLine()
                let move = word //RegEx.parseMove input
                //If possible move
                send cstream (SMPlay move)
                //else
                //send cstream (SMPass)

            else
                forcePrint "Waiting for other player to play...\n"

            

            //debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            let msg = recv cstream
            //debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            let newTurn = State.updateTurn st.playersTurn st.playerAmount
            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let newHand = State.updateHand st.hand ms newPieces
                let newPlayedLetters = State.updatePlayedLetters st.playedTiles ms
                
                let st' = State.mkState st.board  st.dict st.playerNumber newHand st.playerAmount newTurn st.forfeitedPlayers newPlayedLetters // This state needs to be updated
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let newPlayedLetters = State.updatePlayedLetters st.playedTiles ms
                
                let st' = State.mkState st.board  st.dict st.playerNumber st.hand st.playerAmount newTurn st.forfeitedPlayers newPlayedLetters // This state needs to be updated
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = State.mkState st.board  st.dict st.playerNumber st.hand st.playerAmount newTurn st.forfeitedPlayers st.playedTiles// This state needs to be updated
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
            $"Starting game!
                      number of players = %d{numPlayers}
                      player id = %d{playerNumber}
                      player turn = %d{playerTurn}
                      hand =  %A{hand}
                      timeout = %A{timeout}\n\n"

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet numPlayers playerTurn Set.empty<uint32> Map.empty )
        