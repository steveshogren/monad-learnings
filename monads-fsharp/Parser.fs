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
    let Spaces : Parser<list<char>> = Many(CharParser ' ')
    let Word : Parser<string> = 
        parse {
            let! chars = Many1(AnyCharParser)
            return chars |>  List.map (fun x -> x.ToString()) |> List.reduce (+)
        } <|> Return ""
        
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
    
    type Ident = string
    type Atom = string
    type SExpr = A of Atom | Comb of list<Atom>
    
    let (%>) = Righter
    let (<%) = Lefter
    
    let NextWord : Parser<string> =
        parse {
            let! _ = Spaces
            let! w = Word
            let! _ = Spaces
            return w
        } 
  
    let SexprParser : Parser<SExpr> =
        parse {
            let! _ = CharParser '(' 
            let! expre = NextWord
            let!  _ = CharParser ')'
            return A expre
        } <|> parse {
            let! _ = CharParser '(' 
            let! expre = Many NextWord 
            let!  _ = CharParser ')'
            return Comb expre
        }
 
