module internal LetterRip.Util

    open LetterRip
    open MultiSet
    open Dictionary
    
    module internal dictUtil =
        val stepToTuple : char -> Dict -> bool * Dict
        val Alpha : char list
        val getItem : 'a list -> uint32 -> 'a
    
    module multisetUtil =
        val cIdToChar : uint32 -> char
        //val handToCharMultiset : MultiSet<uint32> -> MultiSet<char list>