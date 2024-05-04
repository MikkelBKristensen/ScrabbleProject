module LetterRip.Dictionary


open System.Collections.Generic

type Dict =
    { IsWord: bool
      Children: Dictionary<char, Dict> }

let empty () =
    { IsWord = false
      Children = Dictionary<char, Dict>() }

let rec insert (word: string) (dict: Dict) =
    match word with
    | "" -> { dict with IsWord = true }
    | _ ->
        let head = word.[0]
        let tail = word.[1..]
        let child =
            match dict.Children.TryGetValue head with
            | true, node -> node
            | false, _ -> empty ()
        let updatedChild = insert tail child
        dict.Children.[head] <- updatedChild
        dict

let rec lookup (word: string) (dict: Dict) =
    match word with
    | "" -> dict.IsWord
    | _ ->
        let head = word.[0]
        let tail = word.[1..]
        match dict.Children.TryGetValue head with
        | true, node -> lookup tail node
        | false, _ -> false

let step (c: char) (dict: Dict) =
    match dict.Children.TryGetValue c with
    | true, node -> Some (node.IsWord, node)
    | false, _ -> None
  
// let tupleExtract (res : (bool * Dict) option) =
//     match res with
//     | Some (a, b) -> (a, b)
//     | None -> false, empty ()
    
// let charExtract (node : (bool * Dict)) =
//     match node with
//     | (_, n) -> n.Children.Keys |> Seq.toList
    
let stepToTuple (c: char) (dict: Dict) =
    match step c dict with
    //If true and dict is empty = is word
    | Some (a, b) -> (a, b)
    //If false and dict is false and empty, then wrong step
    | None -> false, empty ()
    