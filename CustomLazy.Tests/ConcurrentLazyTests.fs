module CustomLazy.ConcurrentLazyTests

open System.Threading
open NUnit.Framework
open FsUnit
open CustomLazy

type IConcurrentLazyTester =
    abstract member SingleGetTest: unit -> unit
    abstract member MultipleGetTest: unit -> unit
    abstract member ConcurrentGetTest: unit -> unit

type ConcurrentLazyTester<'a>(supplier: unit -> 'a, expected: 'a, threadAmount: int, getAmount: int) =

    let mutable customLazy: ILazy<'a> Option = None

    interface IConcurrentLazyTester with

        member this.SingleGetTest() =
            customLazy <- Some <| LazyFactory.CreateConcurrentLazy(supplier)
            customLazy.Value.Get() |> should equal expected

        member this.MultipleGetTest() =
            let mutable count = 0
            customLazy <- Some <| LazyFactory.CreateConcurrentLazy(fun () ->
                count <- count + 1
                supplier())
            for i in 0..20 do
                customLazy.Value.Get() |> should equal expected
                count |> should equal 1

        member this.ConcurrentGetTest() =
            let resetEvent = new ManualResetEvent(false)
            let mutable count = 0
            let customLazy = Some <| LazyFactory.CreateConcurrentLazy(fun () ->
                count <- count + 1
                supplier())
            let threads = [ for i in 1 .. threadAmount do yield Thread (fun () ->
                resetEvent.WaitOne()
                for j in 1 .. getAmount do
                    customLazy.Value.Get() |> should equal expected)]
            threads |> List.iter (fun t -> t.Start())
            Thread.Sleep(10)
            resetEvent.Set() |> ignore
            threads |> List.iter (fun t -> t.Join())
            count |> should equal 1

[<TestCaseSource("concurrentTestCases")>]
let ``Single get test``(tester: IConcurrentLazyTester) =
    tester.SingleGetTest()

[<TestCaseSource("concurrentTestCases")>]
let ``Multiple get test``(tester: IConcurrentLazyTester) =
    tester.MultipleGetTest()

[<TestCaseSource("concurrentTestCases")>]
let ``Concurrent get test``(tester: IConcurrentLazyTester) =
    tester.ConcurrentGetTest()

let concurrentTestCases() =
    let cases: IConcurrentLazyTester List = [
        ConcurrentLazyTester<int>((fun () -> 42), 42, 1, 1);
        ConcurrentLazyTester<int>((fun () -> 42), 42, 2, 1);
        ConcurrentLazyTester<int>((fun () -> 42), 42, 3, 1);
        ConcurrentLazyTester<int>((fun () -> 42), 42, 4, 1);
        ConcurrentLazyTester<int>((fun () -> 42), 42, 8, 1);
        ConcurrentLazyTester<int>((fun () -> 42), 42, 2, 3);
        ConcurrentLazyTester<int>((fun () -> 42), 42, 4, 3);
        ConcurrentLazyTester<int>((fun () -> 42), 42, 8, 3);
        ConcurrentLazyTester<int>((fun () -> 42), 42, 1, 1);
        ConcurrentLazyTester<int>((fun () -> 42), 42, 4, 10);
        ConcurrentLazyTester<int>((fun () -> 42), 42, 8, 10);
        ConcurrentLazyTester<string>((fun () -> "ololo"), "ololo", 1, 1);
        ConcurrentLazyTester<string>((fun () -> "ololo"), "ololo", 2, 1);
        ConcurrentLazyTester<string>((fun () -> "ololo"), "ololo", 3, 1);
        ConcurrentLazyTester<string>((fun () -> "ololo"), "ololo", 4, 1);
        ConcurrentLazyTester<string>((fun () -> "ololo"), "ololo", 8, 1);
        ConcurrentLazyTester<string>((fun () -> "ololo"), "ololo", 2, 3);
        ConcurrentLazyTester<string>((fun () -> "ololo"), "ololo", 4, 3);
        ConcurrentLazyTester<string>((fun () -> "ololo"), "ololo", 8, 3);
        ConcurrentLazyTester<string>((fun () -> "ololo"), "ololo", 1, 1);
        ConcurrentLazyTester<string>((fun () -> "ololo"), "ololo", 4, 10);
        ConcurrentLazyTester<string>((fun () -> "ololo"), "ololo", 8, 10)
    ]
    cases |> List.map (fun t -> TestCaseData(t))
