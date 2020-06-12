module CustomLazy.LockFreeLazyTests

open System.Threading
open NUnit.Framework
open FsUnit
open CustomLazy


[<Test>]
let ``Lockfree lazy returns the same value``() =
    let mutable counter = 0
    let customLazy = LazyFactory.CreateLockFreeLazy(fun () -> 
        counter <- counter + 1
        counter)
    let resetEvent = new ManualResetEvent(false)
    let expected = customLazy.Get()
    let threads = [ for i in 1 .. 10 do yield Thread (fun () ->
        resetEvent.WaitOne() |> ignore
        for j in 1 .. 1 do
            customLazy.Get() |> should equal 1)]
    threads |> List.iter (fun t -> t.Start())
    resetEvent.Set() |> ignore
    threads |> List.iter (fun t -> t.Join())