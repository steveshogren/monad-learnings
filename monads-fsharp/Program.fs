
// NOTE: If warnings appear, you may need to retarget this project to .NET 4.0. Show the Solution
// Pad, right-click on the project node, choose 'Options --> Build --> General' and change the target
// framework to .NET 4.0 or .NET 4.5.

module monadsfsharp.Main

open System

let someFunction x y = x + y

let ldeposit account amount = account + amount
let lwithdraw account amount = account - amount

let lacceptable account = 
    let a1 = lwithdraw account 100
    if (a1 > 0) then
       let a2 = ldeposit a1 200
       if (a2 > 0) then
          let a2 = lwithdraw a2 100
          if (a2 > 0) then
            true
          else false
       else false
    else false
    
let deposit amount account = Some(account + amount)
let withdraw amount account = 
    let acc = account - amount    
    if acc < 0 
    then None
    else Some acc
    
let (>>=) m f = Option.bind f m



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

let (<|>) = Either

let rec Many p : Parser<list<'a>> =
    parse {
        let! x = p
        let! xs = (Many p)
        return x :: xs
   } // <|> Return []
   
type Lister<'a> =
    | Any of 'a * Lister<'a>
    | Empty

type A =
   static member appl (l:Lister<'a>, f: 'a -> 'b) =
           match l with
              | Any(x, xs) -> Any (f x, A.appl(xs, f)) 
              | Empty -> Empty 
   static member app(l:'a option, f) = 
           match l with
              | Some x -> Some (f x) 
              | None -> None 
              
let acceptable account : int option =
     withdraw 100 account
     >>= deposit 200
     >>= withdraw 100
     

let rec printer = function 
      | Any(a, ls) -> a.ToString() + ", " + printer ls
      | Empty -> ""

[<EntryPoint>]
let main args = 
    let lists = Any (1, Any(2, Empty))
    
    let maybe = Some 1
    //let result2 = maybe <*> (+ 1)
    //Console.WriteLine("Account at 100: " + lacceptable(101).ToString())
    //Console.WriteLine("Account at 50: " + lacceptable(50).ToString())
    //Console.WriteLine("Account at 100: " + acceptable(101).ToString())
    //Console.WriteLine("Account at 50: " + acceptable(50).ToString())
    0

