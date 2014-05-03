
// NOTE: If warnings appear, you may need to retarget this project to .NET 4.0. Show the Solution
// Pad, right-click on the project node, choose 'Options --> Build --> General' and change the target
// framework to .NET 4.0 or .NET 4.5.

module monadsfsharp.Main

open System
open TestPatterns

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
    
let acceptable account : int option =
     withdraw 100 account
     >>= deposit 200
     >>= withdraw 100
     
let convert(lang, number) = function
        | Language.English, 0 -> "zero" 
        | Language.English, 1 -> "one"
        | Language.English, _ -> "..."
        | Language.Spanish, 0 -> "zero" 
        | Language.Spanish, 1 -> "uno" 
        | Language.Spanish, _ -> "~~~"
        
     
[<EntryPoint>]
let main args = 
    //Console.WriteLine("Converted to " +  TestPatterns.convert(1, Language.English))
    Console.WriteLine("Converted to " +  convert(Language.English, 1))
    Console.WriteLine("Account at 100: " + lacceptable(101).ToString())
    Console.WriteLine("Account at 50: " + lacceptable(50).ToString())
    Console.WriteLine("Account at 100: " + acceptable(101).ToString())
    Console.WriteLine("Account at 50: " + acceptable(50).ToString())
    0

