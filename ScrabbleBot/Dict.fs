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
