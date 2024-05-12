module internal MultiSet

    open System

    type MultiSet<'a when 'a : comparison> = R of Map<'a, uint32>

    let empty = R Map.empty<'a, uint32>

    let isEmpty (a : MultiSet<'a>) = 
        match a with
        | R map -> Map.isEmpty map

    let rec size (R map) =
        Map.fold (fun acc _ count -> acc + count) 0u map
    
    let contains (item: 'a) (set : MultiSet<'a>) =
        match set with
        | R map -> Map.containsKey item map 

   
    let numItems (item : 'a) (set : MultiSet<'a>) =
        let value =
            match set with
            | R map -> Map.tryFind item map
        match value with
        | None -> 0u
        | Some count -> count


    let rec add (item : 'a) (no : uint32) (set : MultiSet<'a>) : MultiSet<'a> =
        match set with
        | R map ->
            match no with
            | 0u -> R map
            | _ ->
                let count =
                    match Map.tryFind item map with
                    | Some c -> c + 1u
                    | None -> 1u
                add item (no-1u) (R (Map.add item count map))
            

    let addSingle (item : 'a) (set : MultiSet<'a>) : MultiSet<'a> =
        match set with
        | R map ->
                let count =
                    match Map.tryFind item map with
                    | Some c -> c + 1u
                    | None -> 1u
                R (Map.add item count map)
            
    
    let rec remove (item : 'a) (no : uint32) (set : MultiSet<'a>) : MultiSet<'a> =
        match set with
        | R map ->
            match no with
            | 0u -> R map
            | _ ->
                let newMap =
                    match Map.tryFind item map with
                    | None -> R map
                    | Some c when c <= 1u -> R (Map.remove item map)
                    | Some c ->  remove item (no-1u) (R (Map.add item (c - 1u) map))
                newMap
            

    let removeSingle (item : 'a) (set : MultiSet<'a>) : MultiSet<'a> =
        remove item 1u set


    let fold (func : 'a -> 'b -> uint32 -> 'a) (acc : 'a) (set : MultiSet<'b>) : 'a =
        match set with
        | R map ->
            Map.fold func acc map


    let keys (set : MultiSet<'a>) =
        match set with
        | R map -> Map.keys map |> List.ofSeq
        
    let foldBack (func : 'a -> uint32 -> 'b -> 'b) (set : MultiSet<'a>) (acc : 'b) =
        match set with
        | R map ->
            Map.foldBack func map acc
    
    let ofList (ls : 'a list) : MultiSet<'a> = List.fold (fun acc item -> addSingle item acc) empty ls
    
    let toList (set : MultiSet<'a>) : 'a list =
        foldBack (fun item count acc ->
        List.init (int count) (fun _ -> item) @ acc
        ) set []


    let map (func : 'a -> 'b) (set : MultiSet<'a>) : MultiSet<'b> = ofList (List.map func (toList set))

    let union (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = empty
    let sum (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = empty
    
    let subtract (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = empty
    
    let intersection (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = empty
       
    