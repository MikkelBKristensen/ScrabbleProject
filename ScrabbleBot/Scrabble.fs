namespace LetterRip

open System
open System.Diagnostics
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
    type WordInfo = {
        Word: string
        LastLetterCoord: coord
        Direction: coord
    }
    
    type Move = {
        Coord       : coord
        TileId      : uint32
        Tile        : char
        PointValue  : int
    }
    
    type state = {
        board            : Parser.board
        dict             : ScrabbleUtil.Dictionary.Dict
        playerNumber     : uint32
        hand             : MultiSet.MultiSet<uint32>
        playerAmount     : uint32
        playersTurn      : uint32
        forfeitedPlayers : Set<uint32>
        playedTiles      : list<coord * (uint32 * (char * int))>
        wordList         : list<WordInfo>
    }
    let mkState b d pn h pa pt fp pl wl =
        {board = b; dict = d;  playerNumber = pn; hand = h; playerAmount = pa; playersTurn = pt
         forfeitedPlayers = fp; playedTiles = pl; wordList = wl; }
    let updateTurn (pid:uint32) (pAmount:uint32)  = ((pid + 1u) % pAmount) + 1u

    // Removes the used pieces from the hand
    let removeUsedPieces (hand : MultiSet.MultiSet<uint32>) (ms : (coord * (uint32 * (char * int))) list) =
        List.fold (fun acc x -> MultiSet.removeSingle (fst (snd x)) acc) hand ms

    let removeUsedPiecesNoCoords (hand : MultiSet.MultiSet<uint32>) (ms : uint32 list) =
        List.fold (fun acc x -> MultiSet.removeSingle x acc) hand ms

    // Adds the new pieces to the hand
    let addNewPieces (newPieces : (uint32 * uint32) list) (hand : MultiSet.MultiSet<uint32>) =
        List.fold (fun acc x -> MultiSet.addSingle (fst x) acc) hand newPieces
    
    // Updates the hand using the methods above
    let updateHand (hand : MultiSet.MultiSet<uint32>) (ms: (coord * (uint32 * (char * int))) list) (newPieces : (uint32 * uint32) list) =
        // First remove the used pieces, then add the new ones to updated hand
        removeUsedPieces hand ms |> addNewPieces newPieces
    
    let updateHandNoCoords (hand : MultiSet.MultiSet<uint32>) (ms: uint32 list) (newPieces : (uint32 * uint32) list) =
        // First remove the used pieces, then add the new ones to updated hand
        removeUsedPiecesNoCoords hand ms |> addNewPieces newPieces
        
    let updatePlayedLetters (PL : list<coord * (uint32 * (char * int))>)  (ms : (coord * (uint32 * (char * int))) list) : list<coord * (uint32 * (char * int))> =
        List.fold (fun acc x -> List.append acc [x]) PL ms
        
    let findPiecesToSwap (hand : MultiSet.MultiSet<uint32>) : uint32 list =
        // Find duplicates in hand
        let duplicates = MultiSet.fold (fun acc x i -> if i > 1u then x :: acc else acc) [] hand
        
        // if duplicates is less than or equal to 3, return the duplicates, else return first 3 elements of hand
        // Create switch for different cases of duplicates length
        match duplicates.Length with
        | 0 -> MultiSet.toList hand |> List.take 3
        | 1 -> duplicates @ MultiSet.toList hand |> List.take 2
        | 2 -> duplicates @ MultiSet.toList hand |> List.take 1
        | 3 -> duplicates
        | _ -> duplicates |> List.take 3
            

    let board st            = st.board
    let dict st             = st.dict
    let playerNumber st     = st.playerNumber
    let hand st             = st.hand
    let playerAmount st     = st.playerAmount
    let playersTurn st      = st.playersTurn
    let forfeitedPlayers st = st.forfeitedPlayers
    let playedLetters st    = st.playedTiles


module FindMove =
    // Finds the word(s) a tile is a part of by looking in all directions from the tile and
    // returning the horizontal and vertical word(s) the tile is a part of.
     
    let findWordFromTile (playedTiles: list<coord * (uint32 * (char * int))>) (tile: coord) : string * string =
        let rec findWordInDirection (direction: coord) (currentTile: coord) : char seq =
            match List.tryFind (fun (coord, _) -> coord = currentTile) playedTiles with
            | Some (_, (_, (letter, _))) ->
                let nextTile = (fst currentTile + fst direction, snd currentTile + snd direction)
                Seq.append (Seq.singleton letter) (findWordInDirection direction nextTile)
            | None -> Seq.empty

        let getHorizontalWord () =
            let leftWord = findWordInDirection (-1, 0) tile
            let rightWord = findWordInDirection (1, 0) tile |> Seq.skip 1 // Skip the original tile's character
            let horizontalLetters = Seq.append (Seq.rev leftWord) rightWord |> Seq.toArray
            if Array.length horizontalLetters > 1 then String(horizontalLetters) else null // Exclude single-letter words

        let getVerticalWord () =
            let upWord = findWordInDirection (0, -1) tile
            let downWord = findWordInDirection (0, 1) tile |> Seq.skip 1 // Skip the original tile's character
            let verticalLetters = Seq.append (Seq.rev upWord) downWord |> Seq.toArray
            if Array.length verticalLetters > 1 then String(verticalLetters) else null // Exclude single-letter words

        (getHorizontalWord (), getVerticalWord ())
    let getStartCoordFromWord (word : string) (currentCoord : (int*int)) (direction : (int*int)) (playedTiles : list<(int * int) * (uint32 * (char * int))>) =
        
        let rec findCoordsFromWord (direction) (currentCoord) : list<(int*int)>=
            match List.tryFind (fun (coord, _) -> coord = currentCoord) playedTiles with
            | Some (coordinate, (_, (_, _))) ->
                let nextTile = (fst currentCoord + fst direction, snd currentCoord + snd direction)
                List.append [coordinate] (findCoordsFromWord direction nextTile)
            | None -> List.empty
        
        let pluscoords = findCoordsFromWord direction currentCoord
        let oppositeDirection = (-1 * (fst direction), -1 * (snd direction))
        let minuscoords = findCoordsFromWord oppositeDirection currentCoord
        List.append minuscoords pluscoords |> List.min 
            
        
    
    let addWordIfNotEmpty (wordCoordMap: list<State.WordInfo>) (word: string) (lastLetterCoord: coord) (direction : coord) : list<State.WordInfo> =
        if word <> null then
            { Word = word; LastLetterCoord = lastLetterCoord; Direction = direction } :: wordCoordMap
        else
            wordCoordMap

    let findAllWords (playedTiles: list<coord * (uint32 * (char * int))>) : list<State.WordInfo> =
        List.fold (fun acc (coord, _) ->
            let (horizontalWord, verticalWord) = findWordFromTile playedTiles coord
            
            // Extract the last letter coordinate
            let lastLetterCoord = coord

            let horizontalWords = addWordIfNotEmpty acc horizontalWord lastLetterCoord (1, 0)
            let verticalWords = addWordIfNotEmpty horizontalWords verticalWord lastLetterCoord (0, 1)
            
            verticalWords
        ) [] playedTiles
    
    (*
        The method to find a word to start the game of with, is buggy -
        cIDlist is not backtracked properly, so we are limiting ourselves in possibilities of spelling a new word,
        if the first "try" isn't correct.
    
    *)
    let rec removeTail list =
            match list with
            | [] -> []
            | [_] -> [] // If there's only one element, return an empty list
            | head :: tail -> head :: removeTail tail
    
    let rec assignCoords (coord: (int * int)) (advance: (int * int)) (move: ((int * int) * (uint32 * (char * int))) list) (word: (uint32 *(char * int)) list) =
            match word with
            | [] -> move // return empty word for passing/swapping
            | head :: rest ->
                let move = List.append move [(coord, head)]
                let coord = ((fst coord)+(fst advance), (snd coord)+(snd advance))
                assignCoords coord advance move rest
    
    let getPrefixCharListFromString (word : string) =
        let charList = word.ToCharArray() |> List.ofArray
        List.foldBack (fun x acc ->
             let charId = multisetUtil.charToCId x
             let charValue = multisetUtil.cIdToPV charId
             (charId, (x, charValue )) :: acc
             ) (charList) []
   
    
        
    // Mostly works! It just finds the first word. Likely only two letters long
    let rec assembleWord (cIdList: uint32 list) (dict: Dictionary.Dict) (word: list<(uint32 * (char * int))>) =
        match cIdList with
        | [] -> []
        | head :: rest ->
            let step = Dictionary.step (multisetUtil.cIdToChar head) dict // Perform step
            match step with
            | None -> // No word with head char; Try again with rest
                match rest with
                | [] -> [] // more cases
                | _  when head > rest.Head -> []
                | _ -> assembleWord (List.append rest [head]) dict word
            | Some x -> 
                let newWord = List.append word [(head,(multisetUtil.cIdToChar head, multisetUtil.cIdToPV head))]
                if fst x then
                    newWord
                else
                    let newNewWord = assembleWord rest (snd x) newWord
                    match newNewWord with
                    | [] when head > rest.Head -> []
                    | [] -> assembleWord (List.append rest [head]) dict word
                    | _ -> newNewWord
                        
    let FindWordFromHand (st : State.state) =         
        let cIdList = MultiSet.toList (st.hand)
        assembleWord cIdList st.dict List.Empty |> assignCoords (0,0) (1,0) List.Empty
        
    let assembleFromPrefix (cIdList: uint32 list) (dict: Dictionary.Dict) (word: list<(uint32 * (char * int))>) =
        let rec updateDict word (updatedDict : Dictionary.Dict) =
            match word with
            |[] -> updatedDict
            | head :: tail ->
                let char = fst (snd head) 
                let step = Dictionary.step char updatedDict
                match step with
                | None -> failwith "shouldn't happen"
                | Some x -> 
                    updateDict tail (snd x)
                    
        let newDict = updateDict word dict
        assembleWord cIdList newDict []
    
    (* 
       Checks each tile of a word, and returns of the list that are not valid
       Meaning if the list is empty, the word is a valid play
    *)
    
    let findDuplicates list1 list2 =
        let duplicates = List.filter (fun (x, _) -> List.exists (fun (y, _) -> x = y) list2) list1
        not duplicates.IsEmpty
    
    let checkTiles (word : list<((int*int)*(uint32 * (char * int)))>) (playedTiles : list<coord * (uint32 * (char * int))>) (direction : coord) (dict : Dictionary.Dict) =
       let boardWithWord = playedTiles @ word
        
       if findDuplicates word playedTiles then
            ["duplicates found"]
       else
       
       match direction with
       | (1,0) -> //Horizontal word, check vertical
           List.foldBack (fun (coordinates, tile) acc ->
                        let placedWords = findWordFromTile boardWithWord coordinates
                        let verticalWord = snd placedWords
                        let horizontalWord = fst placedWords
                        
                        forcePrint $"\n{verticalWord}\n"
                        forcePrint $"{horizontalWord}\n"
                        
                        if verticalWord= null || Dictionary.lookup verticalWord dict && (horizontalWord = null || Dictionary.lookup horizontalWord dict) then
                            acc
                         else
                            List.append acc [verticalWord]
                          ) word List.Empty
           
       | (0,1) -> //Vertical word, check horizontal
            List.foldBack (fun (coordinates, tile) acc ->
                        let placedWords = findWordFromTile boardWithWord coordinates
                        let verticalWord = snd placedWords
                        let horizontalWord = fst placedWords
                        
                        forcePrint $"\n{verticalWord}\n"
                        forcePrint $"{horizontalWord}\n"
                        
                        if   horizontalWord= null || Dictionary.lookup horizontalWord dict && (verticalWord = null || Dictionary.lookup verticalWord dict) then
                            acc
                         else
                            List.append acc [horizontalWord]
                          ) word List.Empty
       
       
  
    let FindWordOnBoard (st : State.state) =
        (*
            words stores vertical and horizontal words related to a tile, the boolean if false indicates
            that we should try the horizontal word, while if it is true we try the vertical
        *)
        let rec investigateWordsFromCoord (words : bool *(string * string)) (coord : coord) (direction : coord) (playedTiles : list<coord * (uint32 * (char * int))>) (uncheckedTiles : Map<coord, (uint32 * (char * int))>)  =
            //Missing way to iterate coords
            match words with
            | (false, (horizontal, _)) -> // Explore the horizontal word
                let startChar = Map.find coord uncheckedTiles
                let cIdList = MultiSet.toList (st.hand)
                
                if horizontal = null then //Null indicates empty word
                    let newWord = assembleFromPrefix cIdList st.dict [startChar] // if this is empty then we advance our tries
                    if List.isEmpty newWord then
                        
                        investigateWordsFromCoord (true, (findWordFromTile playedTiles coord)) coord (0,1) playedTiles uncheckedTiles
                    else
                        
                        let startCoord = (fst coord + fst direction, snd coord + snd direction)
                        //return word, without head because head is already placed on board
                        
                        //assign coords and check if all tiles create valid words else - progress with new try
                        let wordWithCoords = assignCoords startCoord direction List.Empty newWord  
                        let invalidWords = checkTiles wordWithCoords st.playedTiles direction st.dict
                        if invalidWords.Length = 0 then
                            wordWithCoords
                        else
                            //let updateUncheckedTiles = Map.remove coord uncheckedTiles
                            //let newCoord = fst (Map.minKeyValue updateUncheckedTiles)
                            investigateWordsFromCoord (true, findWordFromTile playedTiles coord) coord (0,1) playedTiles uncheckedTiles
                        
                else
                    //Convert string to uint32 * (char * int) list, before assembling word
                    let prefixCharList = getPrefixCharListFromString horizontal
                    let newWord = assembleFromPrefix cIdList st.dict prefixCharList
                    
                    if List.isEmpty newWord then
                        investigateWordsFromCoord (true, (findWordFromTile playedTiles coord)) coord (0,1) playedTiles uncheckedTiles
                    else
                       //Take account that we dont know the true start coordinate of the word at this point
                       let startCoord =
                            getStartCoordFromWord horizontal coord direction playedTiles
                            
                       let startPlacingPoint = (fst startCoord  + fst direction * horizontal.Length, snd startCoord+ snd direction)
                       let wordWithCoords = assignCoords  startPlacingPoint direction List.Empty newWord  
                       let invalidWords = checkTiles wordWithCoords st.playedTiles direction st.dict
                       if invalidWords.Length = 0 then
                            wordWithCoords
                       else
                            //let updateUncheckedTiles = Map.remove coord uncheckedTiles
                            //let newCoord = fst (Map.minKeyValue updateUncheckedTiles)
                            investigateWordsFromCoord (true, findWordFromTile playedTiles coord) coord (0,1) playedTiles uncheckedTiles
                       
                        
            | (true, (_, vertical)) -> //Explore the vertical word
                let startChar = Map.find coord uncheckedTiles
                let cIdList = MultiSet.toList (st.hand)
                
                if vertical = null then
                    let newWord = assembleFromPrefix cIdList st.dict [startChar]
                    if List.isEmpty newWord then
                        //Advance with newCoord (but we need to find som logic to find out exactly what coords that should be)
                        let updateUncheckedTiles = Map.remove coord uncheckedTiles
                        if updateUncheckedTiles.IsEmpty then
                            []
                        else
                            let newCoord = fst (Map.minKeyValue updateUncheckedTiles)
                            investigateWordsFromCoord (false, findWordFromTile playedTiles newCoord) newCoord (1,0) playedTiles updateUncheckedTiles
                    else
                        let startCoord = (fst coord  + fst direction, snd coord + snd direction)
                        //assign coords and check if all tiles create valid words else - progress with new try
                        let wordWithCoords = assignCoords  startCoord direction List.Empty newWord  
                        let invalidWords = checkTiles wordWithCoords st.playedTiles direction st.dict
                        if invalidWords.Length = 0 then
                            wordWithCoords
                        else
                            let updateUncheckedTiles = Map.remove coord uncheckedTiles
                            if updateUncheckedTiles.IsEmpty then
                                []
                            else
                                let newCoord = fst (Map.minKeyValue updateUncheckedTiles)
                                investigateWordsFromCoord (false, findWordFromTile playedTiles newCoord) newCoord (1,0) playedTiles updateUncheckedTiles
                        
                else
                    //Convert string to suitable list, before giving it to tryAssembleWord
                    let prefixCharList = getPrefixCharListFromString vertical
                    let newWord = assembleFromPrefix cIdList st.dict prefixCharList
                    
                    if List.isEmpty newWord then
                       //Advance with newCoord (but we need to find som logic to find out exactly what coords that should be)
                        
                        let updateUncheckedTiles = Map.remove coord uncheckedTiles
                        if updateUncheckedTiles.IsEmpty then
                            []
                        else 
                            let newCoord = fst (Map.minKeyValue updateUncheckedTiles)
                            investigateWordsFromCoord (false, findWordFromTile playedTiles newCoord) newCoord (1,0) playedTiles updateUncheckedTiles
                    else
                        //Take account that we dont know the true start coordinate of the word at this point
                        
                        let startCoord =
                            getStartCoordFromWord vertical coord direction playedTiles 
                        
                        let startPlacingPoint = (fst startCoord  + fst direction, snd startCoord + snd direction * vertical.Length)
                        
                        //assign coords and check if all tiles create valid words else - progress with new try
                        let wordWithCoords = assignCoords  startPlacingPoint direction List.Empty newWord  
                        let invalidWords = checkTiles wordWithCoords st.playedTiles direction st.dict
                        if invalidWords.Length = 0 then
                            wordWithCoords
                        else
                            let updateUncheckedTiles = Map.remove coord uncheckedTiles
                            if updateUncheckedTiles.IsEmpty then
                                []
                            else 
                                let newCoord = fst (Map.minKeyValue updateUncheckedTiles)
                                investigateWordsFromCoord (false, findWordFromTile playedTiles newCoord) newCoord (1,0) playedTiles updateUncheckedTiles
                        
                        
                        
        let word = investigateWordsFromCoord (false, findWordFromTile st.playedTiles (0,0)) (0,0) (1,0) st.playedTiles (Map.ofList st.playedTiles)
        
        word
        
        
    let decisionStarter (st:State.state) =
        if st.playedTiles.IsEmpty then
            FindWordFromHand st
        else 
            FindWordOnBoard st
    
       
module Scrabble =
    open System.Threading

    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            debugPrint $"Playerturn {State.playersTurn st}\n"
            debugPrint $"Playernumber {State.playerNumber st}\n"
            
            let tilesToBeSwapped = State.findPiecesToSwap st.hand

            if State.playerNumber st = State.playersTurn st then
                debugPrint "Your turn!\n"
                let word = FindMove.decisionStarter st // If empty word is returned, pass or swap
                let move = word //RegEx.parseMove input
                
                
                if List.isEmpty move then
                    //Change tiles to use tilefinder logic
                    send cstream (SMChange tilesToBeSwapped)
                    //Missing logic to abort swap and pass instead
                    //send cstream (SMPass)
                else
                    send cstream (SMPlay move)

            else
                debugPrint "Waiting for other player to play...\n"

            //debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            let msg = recv cstream
            //debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            let newTurn = State.updateTurn st.playersTurn st.playerAmount
            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let newHand = State.updateHand st.hand ms newPieces
                let newPlayedLetters = State.updatePlayedLetters st.playedTiles ms
                let wordsPlayed = FindMove.findAllWords newPlayedLetters
                
                let st' = State.mkState st.board  st.dict st.playerNumber newHand st.playerAmount newTurn st.forfeitedPlayers newPlayedLetters wordsPlayed // This state needs to be updated
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                
                // Make playedLetters -> list<coord * uint32>
                let newPlayedLetters = State.updatePlayedLetters st.playedTiles ms
                
                // Make wordsPlyed -> list<WordInfo>
                let wordsPlayed = FindMove.findAllWords newPlayedLetters
                
                let st' = State.mkState st.board  st.dict st.playerNumber st.hand st.playerAmount newTurn st.forfeitedPlayers newPlayedLetters wordsPlayed // This state needs to be updated
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                
                let st' = State.mkState st.board  st.dict st.playerNumber st.hand st.playerAmount newTurn st.forfeitedPlayers st.playedTiles st.wordList // This state needs to be updated
                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM (CMChangeSuccess( newTiles) ) ->
                let newHand = State.updateHandNoCoords st.hand tilesToBeSwapped newTiles
                let st' = State.mkState st.board  st.dict st.playerNumber newHand st.playerAmount newTurn st.forfeitedPlayers st.playedTiles st.wordList // This state needs to be updated
                aux st'
            
                
            | RCM (CMPassed _) ->
                aux st
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> debugPrint "Gameplay Error:\n%A";  aux st


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

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet numPlayers playerTurn Set.empty<uint32> list.Empty list.Empty )
        