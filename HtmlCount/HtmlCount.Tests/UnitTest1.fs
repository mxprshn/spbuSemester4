module HtmlCount.Tests

open NUnit.Framework

[<Test>]
let Test1 () =
    let ololo = HtmlProcessor (HtmlLoader ())
    Async.RunSynchronously (ololo.CountLinkPagesSymbols "http://hwproj.me/courses/34")
    Assert.Pass()
