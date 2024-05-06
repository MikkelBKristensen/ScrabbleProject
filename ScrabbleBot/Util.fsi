module internal LetterRip.Util
    open LetterRip
    open MultiSet
    open Dictionary
    module internal dictUtil =
        val stepToTuple : char -> Dict -> bool * Dict
        val Alpha : char list
        val PointValue: int list
        val getItem : 'a list -> uint32 -> 'a
    
    module internal multisetUtil =
        val cIdToChar : uint32 -> char
        val charToCId : char -> uint32
        val cIdToPV : uint32 -> int
        //val handToCharMultiset : MultiSet<uint32> -> MultiSet<char list>