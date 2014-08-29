namespace monadsfsharp 

module Parser = 
    type ParserResult<'a> =
        | Success of 'a * list<char>
        | Failure
        
    type Parser<'a> = list<char> -> ParserResult<'a>

    let Either (p1: Parser<'a>) (p2: Parser<'a>) : Parser<'a> =
        let p stream =
            match p1 stream with
              | Failure -> p2 stream
              | res -> res
        in p
        
    let Righter (p1: Parser<'a>) (p2: Parser<'b>) : Parser<'b> =
        let p stream =
            let _ = p1 stream
            p2 stream
        in p
    let Lefter (p1: Parser<'a>) (p2: Parser<'b>) : Parser<'a> =
        let p stream =
            let pa1 = p1 stream
            let _ = p2 stream
            pa1
        in p

    let Return (x:'a):Parser<'a> =
        let p stream = Success(x,stream)
        in p
        
    let Bind (p:Parser<'a>) (f: 'a -> Parser<'b>) : Parser<'b> =
        let q stream =
           match p stream with
             | Success(x, rest) -> (f x) rest
             | Failure -> Failure
        in q
             
    type ParserBuilder() =
       member x.Bind(p, f) = Bind p f
       member x.Return(y) = Return y
       
    let parse = new ParserBuilder()
        
    let CharParser (c:char) : Parser<char> =
        let p stream =
            match stream with
            | x::xs when x = c -> Success(x,xs)
            | _ -> Failure
        in p
        
    let AnyCharParser : Parser<char> =
        ['A'..'z']
        |> List.map CharParser
        |> List.reduce Either
        
    let DigitParser : Parser<char> =
        ['0'..'9']
        |> List.map CharParser
        |> List.reduce Either

    let (<|>) = Either

    let rec Many p : Parser<list<'a>> =
       parse {
           let! x = p
           let! xs = (Many p)
           return x :: xs
       } <|> Return []

    let rec Many1 p : Parser<list<'a>> =
       parse {
           let! x = p
           let! xs = (Many p)
           return x :: xs
       } 
       
    let Space : Parser<char> = CharParser ' '
    let Spaces : Parser<list<char>> = Many(Space)
    
    let Word : Parser<string> = 
        parse {
            let! chars = Many1(AnyCharParser)
            return chars |>  List.map (fun x -> x.ToString()) |> List.reduce (+)
        } 
        
    let FloatParser : Parser<float> = 
        parse {
            let! s = (CharParser '+' <|> CharParser '-') <|> Return '+'
            let! l = (parse {
                let! l = Many1 DigitParser
                let! pd = (parse {
                    let! p = CharParser '.'
                    let! d = Many DigitParser
                    return p::d
                } <|> Return [])
                return l @ pd
            } <|> parse {
                        let! l = Many1 DigitParser
                        let! p = CharParser '.'
                        let! d = Many1 DigitParser
                        return l @ p :: d
                  })
            let! e = (parse {
                let! e = CharParser 'e' <|> CharParser 'E'
                let! s = (CharParser '+' <|> CharParser '-') <|> Return '+'
                let! x = Many1 (DigitParser)
                return e::s::x
            } <|> Return [])
            return float( new System.String(s::(l @ e) |> List.toArray))
        }
    
    type Atom = Str of string | Digit of float
    type SExpr = Atom of Atom | Sexprs of list<SExpr> | EList
    
    let (%>) = Righter
    let (<%) = Lefter
    
    let NextWord : Parser<Atom> =
        parse {
            let! s = Spaces
            let! w = Word
            let! t = Spaces
            return Str w 
        } 
    let NextDigit : Parser<Atom> =
        parse {
            let! s = Spaces
            let! w = FloatParser
            let! t = Spaces
            return Digit w 
        } 
        
    let AtomParser : Parser<Atom> = NextDigit <|> NextWord
    
#nowarn "40"
module Parse = 
    open Parser
    let rec SexprParser : Parser<SExpr> =
        let EmptyList() = parse { let! _ = Spaces 
                                  return EList }
        let AnAtom() = parse { let! expre = AtomParser
                               return Atom expre }
        let SExprP() = parse {
                            let! i = Many ( AnAtom() <|> SexprParser )
                            return Sexprs i
                        }
        parse {
            let! _ = Spaces 
            let! _ = CharParser '('
            let! t = EmptyList()
            let! _ = CharParser ')'
            let! _ = Spaces 
            return t
        } <|> parse {
            let! _ = Spaces 
            let! _ = CharParser '('
            let! t = SExprP() 
            let! _ = CharParser ')'
            let! _ = Spaces 
            return t
        }
        
    let callSexpParser s = 
        s |> Seq.toList |> SexprParser
 
