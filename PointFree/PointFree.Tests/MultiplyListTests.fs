module MultiplyList.Tests

open NUnit.Framework
open FsCheck
open MultiplyList

[<Test>]
let ``Functions return equal results`` () = 
    Check.QuickThrowOnFailure (fun x l ->
        MultiplyList x l = MultiplyList2 x l &&
        MultiplyList2 x l = MultiplyList3 x l &&
        MultiplyList3 x l = MultiplyListPointFree x l)