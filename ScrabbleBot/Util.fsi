namespace LetterRip.Util

    module internal dictUtil =
        val stepToTuple : char -> LetterRip.Dictionary.Dict -> bool * LetterRip.Dictionary.Dict
        val Alpha : char list
        val getItem : 'a list -> uint32 -> 'a
    
    module multisetUtil =
        val cIdToChar : uint32 -> char