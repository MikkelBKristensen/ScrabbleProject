// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modules Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open StateMonad
    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"
    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"
    
    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy System.Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

    let spaces         = many whitespaceChar <?> "space"
    
    let spaces1        = many1 whitespaceChar <?> "space1"

    let (.>*>.) x y =
        x .>> spaces .>>. y
    let (.>*>) x y  =
        x .>> spaces .>> y
    let (>*>.) x y  =
        x >>. spaces >>. y
    let (>**>.) x y  =
        x >>. spaces1 >>. y

    let parenthesise p =
       pchar '(' >*>. p .>*> pchar ')'
    let tuborgKlamme p =
       pchar '{' >*>. p .>*> pchar '}'
   
    //Inspired by charlistToStr from JParsec.fs
    let pid : Parser<string> =
        pletter <|> pchar '_' .>>. many (palphanumeric <|> pchar '_') |>> (fun (x, xs) -> string(List.toArray (x :: xs)))
        
    let unop op a =
        op >*>. a
        
    let binop op a b =
        a .>*> op .>*>. b

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    
    
    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [SubParse; AddParse; ProdParse]
    
    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]

    let NParse   = pint32 |>> N <?> "Int"
    let VParse = pid |>> V <?> "Var"
    let ParParse = parenthesise TermParse
    let NegParse = unop (pchar '-') AtomParse |>> (fun x -> Mul (N -1, x)) <?> "Neg"
    let PVParse =  pPointValue >*>. parenthesise TermParse |>> PV <?> "PV"
    

    
    do aref := choice [PVParse; NegParse; VParse; NParse; ParParse]
    
    
    let AexpParse = TermParse 
    
    
    let cTermParse , cref = createParserForwardedToRef<cExp>()
    
    let CParse =  between (pchar ''') (pchar ''') (palphanumeric <|> whitespaceChar) |>> C <?> "Char"
    let CVParse = unop pCharValue (parenthesise AexpParse) |>> CV <?> "CV"
    let IntToCharParse = unop pIntToChar (parenthesise AexpParse) |>> IntToChar <?> "IntToChar"
    let ToUpperParse = unop pToUpper (parenthesise cTermParse) |>> ToUpper <?> "ToUpper"
    let ToLowerParse = unop pToLower (parenthesise cTermParse) |>> ToLower <?> "ToLower"
    
    do cref := choice [ CVParse;  IntToCharParse; ToUpperParse; ToLowerParse; CParse]
    
    let CharToIntParse = unop pCharToInt (parenthesise cTermParse) |>> CharToInt <?> "CharToInt"
    do aref := choice [CharToIntParse; NegParse; PVParse; VParse; NParse; ParParse]
    let CexpParse = cTermParse

    let bTermParse, bTref = createParserForwardedToRef<bExp>()
    let bProdParse, bPref = createParserForwardedToRef<bExp>()
    let bAtomParse, bAref = createParserForwardedToRef<bExp>()
    
    let AndParse = binop (pstring "/\\") bProdParse bTermParse |>> Conj <?> "And"
    let OrParse = binop (pstring "\\/") bProdParse bTermParse |>> (fun (x,y) -> x .||. y) <?> "Or"
    do bTref := choice [AndParse; OrParse; bProdParse]
    
   
    let EqParse = binop (pchar '=') AexpParse AexpParse |>> AEq <?> "Eq"
    let NEqParse = binop (pstring "<>") AexpParse AexpParse |>> (fun(a,b) -> a.<>.b) <?> "Not Equal"
    let LTParse = binop (pchar '<') AexpParse AexpParse |>> (fun(a,b) -> a .<. b) <?> "Less Than"
    let LTEQParse = binop (pstring "<=") AexpParse AexpParse |>> (fun(a,b) -> a .<=. b) <?> "Less Than or Equal"
    let GTParse = binop (pchar '>') AexpParse AexpParse |>> (fun(a,b) -> a .>. b) <?> "Greater Than"
    let GTEQParse = binop (pstring ">=") AexpParse AexpParse |>> (fun(a,b) -> a .>=. b) <?> "Greater Than or Equal"
    
    do bPref := choice [EqParse; NEqParse; LTParse; LTEQParse; GTParse; GTEQParse; bAtomParse]
    
    let NegBParse = unop (pchar '~') bAtomParse |>> (fun x -> ~~ x) <?> "Neg"
    let IsLetterParse = unop pIsLetter (parenthesise cTermParse) |>> IsLetter <?> "IsLetter"
    let IsVowelParse = unop pIsVowel (parenthesise cTermParse) |>> IsVowel <?> "IsVowel"
    let IsDigitParse = unop pIsDigit (parenthesise cTermParse) |>> IsDigit <?> "IsDigit"
    let FalseParse = pFalse |>> (fun _ -> FF) <?> "False"
    let TrueParse = pTrue |>> (fun _ -> TT) <?> "True"
    let ParBParse = parenthesise bTermParse
    
    do bAref := choice [TrueParse; FalseParse; NegBParse; IsLetterParse; IsVowelParse; IsDigitParse; ParBParse]
    let BexpParse = bTermParse
    
    let sSeqParse, sSegRef = createParserForwardedToRef<stmnt>()
    let sAtomParse, sAref = createParserForwardedToRef<stmnt>()
    
    let SeqParse = binop (pchar ';') sAtomParse sSeqParse |>> Seq <?> "Sequence"
    do sSegRef := choice [SeqParse; sAtomParse]
    
    let DeclParse = pdeclare >**>. pid|>> Declare <?> "Declare"
    let AssignParse = binop (pstring ":=") pid AexpParse|>> Ass <?> "Assign"
    let ITEParse = unop pif (parenthesise BexpParse) .>*>.
                   unop pthen (tuborgKlamme sSeqParse) .>*>.
                   unop pelse sAtomParse |>>
                   (fun ((cond,iff),elsee) -> ITE (cond, iff, elsee)) <?> "If-then-else"
    let ITParse = unop pif (parenthesise BexpParse) .>*>.
                  unop pthen (tuborgKlamme sSeqParse) |>>
                  (fun(cond,iff) -> ITE (cond, iff, Skip)) <?> "If-then"
    let WhileParse = unop pwhile (parenthesise BexpParse) .>*>.
                     unop pdo (tuborgKlamme sSeqParse)|>> While <?> "While-loop"
                     
    do sAref := choice [AssignParse; DeclParse; ITEParse; ITParse; WhileParse]                 
    let stmntParse = sSeqParse

    (* The rest of your parser goes here *)
    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    
    type boardFun2 = coord -> Result<square option, Error>
        
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }

    let parseSquareProg (sqp:squareProg) : square =
        Map.map (fun _ -> run stmntParse >> getSuccess >> stmntToSquareFun) sqp
    
    // Default (unusable) board in case you are not implementing a parser for the DSL.
    // let mkBoard : boardProg -> board = fun _ -> {center = (0,0); defaultSquare = Map.empty; squares = fun _ -> Success (Some Map.empty)}
    let parseBoardProg (s:string) (sqs: Map<int, square>) :boardFun2 =
        s |> run stmntParse |> getSuccess |> stmntToBoardFun <| sqs
        
    //let parseBoardProg _ = failwith "not implemented"

    let mkBoard (bp : boardProg) : board =
            let squaresMap = bp.squares
            let squares = Map.map (fun _ squareProg -> parseSquareProg squareProg) squaresMap
            let defaultSquare = Map.find bp.usedSquare squaresMap
            {
                center = bp.center
                defaultSquare = parseSquareProg defaultSquare
                squares = parseBoardProg bp.prog squares
            }