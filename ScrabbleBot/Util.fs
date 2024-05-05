


module internal LetterRip.Util
    open MultiSet
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
            
            
         
        let Alpha = ['_';'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L'; 'M'; 'N'; 'O'; 'P'; 'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z']

        let PointValue = [0; 1; 3; 3; 2; 1; 4; 2; 4; 1; 8; 5; 1; 3; 1; 1; 3; 10; 1; 1; 1; 4; 4; 8; 4; 10]
    
        let getItem (list: 'a list) (i : uint32) =
            let rec aux l i j =
                match l with
                | a :: b -> if j = i then a else aux b i (j + 1u)
                | [] -> failwith "Index out of bounds"
            aux list i 0u

    module multisetUtil =
        let cIdToChar id = dictUtil.getItem dictUtil.Alpha id
        
        let cIdToPV id = dictUtil.getItem dictUtil.PointValue id

        //Takes hand a makes 
        //let handToCharMultiset (set: MultiSet<uint32>) = MultiSet.map (fun x -> [cIdToChar x])
        
        

