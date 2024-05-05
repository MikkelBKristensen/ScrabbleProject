module internal LetterRip.Util

    open LetterRip.Dictionary
    module internal dictUtil =

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
            
            
         
        let Alpha = ['_';'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'n'; 'o';
                                'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z';]
    
        let getItem (list: 'a list) (i : uint32) =
            let rec aux l i j =
                match l with
                | a :: b -> if j = i then a else aux b i (j + 1u)
                | [] -> failwith "Index out of bounds"
            aux list i 0u

    module multisetUtil =
        let cIdToChar id = dictUtil.getItem dictUtil.Alpha id
        
        //Takes hand a makes 
        //let handToCharMultiset = MultiSet.map (fun x -> [cIdToChar x])
        
        

