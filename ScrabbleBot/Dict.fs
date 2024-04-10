module LetterRip.Dict

    type Dict =
        | Leaf of bool
        | Node of bool * Map<char, Dict>
        
    type dMap = Map<char,Dict>
    
    let empty () = Leaf false
    
    let rec insert (s: string) (dict: Dict) =
        let insertHelper (word: string) (node: Dict) (index: int) =
            match index with
            | i when i = word.Length -> Leaf true
            | Node (_, dic) when word.Length = 0 -> Node (true, dic)
            | Leaf b ->
                let temp = dMap ()
                let char = word.[0]
                temp[c] <- insert word[1..] (empty ())
                Node(b, temp)
            | Node(b, dic) ->
                let c = word[0]
                match dic.TryFind c with
                | (true, value) ->
                    dic[c] <- insert word[1..] value
                    Node(b, dic)
                | (false, _) ->
                    dic[c] <- insert word[1..] (empty())
                    Node(b, dic)