module TestsTests.ConcStackTests

open ConcStack
open NUnit.Framework
open FsCheck
open FsUnit
open System.Threading

[<Test>]
let ``Simple Push and Pop test`` () =
    let stack = ConcStack<int> ()
    stack.Push 10
    stack.TryPop |> should equal (Some(10))

[<Test>]
let ``Multiple Push and Pop test`` () =
    let stack = ConcStack<int> ()
    stack.Push 10
    stack.Push 20
    stack.Push 30
    stack.Push 40
    stack.Push 50
    stack.TryPop |> should equal (Some(50))
    stack.TryPop |> should equal (Some(40))
    stack.TryPop |> should equal (Some(30))
    stack.TryPop |> should equal (Some(20))
    stack.TryPop |> should equal (Some(10))
    stack.TryPop |> should equal None

[<Test>]
let ``Simple concurrent Push test`` () =
    let stack = ConcStack<int> ()

    let thread1 = System.Threading.Thread (fun () ->
        stack.Push(73))
        
    let thread2 = System.Threading.Thread (fun () ->  
        stack.Push(42))
    
    thread1.Start()
    thread2.Start()
    System.Threading.Thread.Sleep 1000
    stack.TryPop |> should not' (equal None)
    stack.TryPop |> should not' (equal None)
    stack.TryPop |> should equal None




