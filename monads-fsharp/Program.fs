
// NOTE: If warnings appear, you may need to retarget this project to .NET 4.0. Show the Solution
// Pad, right-click on the project node, choose 'Options --> Build --> General' and change the target
// framework to .NET 4.0 or .NET 4.5.

module monadsfsharp.Main

open System
open monadsfsharp.Parser

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
    //let lists = Any (1, Any(2, Empty))
    //let maybe = Some 1
    //let result2 = maybe <*> (+ 1)
    // printfn "%A" (FloatParser ("-1.23e45" |> Seq.toList))
    printfn "%A" (SexprParser ("(a)" |> Seq.toList))
    //Console.WriteLine(FloatParser)
    //Console.WriteLine("Account at 50: " + lacceptable(50).ToString())
    //Console.WriteLine("Account at 100: " + acceptable(101).ToString())
    //Console.WriteLine("Account at 50: " + acceptable(50).ToString())
    0