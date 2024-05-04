


module LetterRip.Util

    open Dictionary
    module dictUtil =

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
    



