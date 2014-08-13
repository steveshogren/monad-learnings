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