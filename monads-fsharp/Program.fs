
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
    

[<EntryPoint>]
let main args = 
    Console.WriteLine("Account at 100: " + lacceptable(101).ToString())
    Console.WriteLine("Account at 50: " + lacceptable(50).ToString())
    0

