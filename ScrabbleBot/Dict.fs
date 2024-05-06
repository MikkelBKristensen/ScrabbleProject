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
        { dict with Children = dict.Children }

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
    
// This method finds all words with a given prefix in the trie
let rec findWordsWithPrefix (prefix: string) (dict: Dict) =
    match prefix with
    | "" -> [] 
    | _ ->
        let rec traverseToLastNode (prefix: string) (node: Dict) =
            match prefix with
            | "" -> node // Reached the end of the prefix
            | _ ->
                let head = prefix.[0]
                let tail = prefix.[1..]
                match step head node with
                | Some(_, nextNode) -> traverseToLastNode tail nextNode
                | None -> empty ()

        let lastNode = traverseToLastNode prefix dict

        let rec traverseChildren (node: Dict) (prefixSoFar: string) (acc: string list) =
            let mutable words = acc

            // Iterate over child nodes
            for kvp in node.Children do
                let char, child = kvp.Key, kvp.Value
                let newPrefix = prefixSoFar + string char
                if child.IsWord && newPrefix <> prefix then
                    // If the node represents a word (excluding the original prefix), add it to the list
                    words <- newPrefix :: words
                // Recursively traverse child nodes
                words <- traverseChildren child newPrefix words
            words

        // Start traversing children from the last node representing the prefix
        traverseChildren lastNode prefix []
