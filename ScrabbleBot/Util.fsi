module LetterRip.Util
    module dictUtil =
        val stepToTuple : char -> LetterRip.Dictionary.Dict -> bool * LetterRip.Dictionary.Dict
        val Alpha : char list
        val getItem : 'a list -> uint32 -> 'a
