module HtmlCount.Tests

open NUnit.Framework
open FsUnit
open Foq

[<Test>]
let ``Smoke test`` () =
    let loaderMock = Mock<IHtmlLoader>().Setup(fun x -> <@ x.LoadHtmlAsync(any()) @>).Returns(async {
        return "<a href=\"https://www.ololo.com/\" />" }).Create()
    let processor = HtmlProcessor(loaderMock)
    let result = Async.RunSynchronously(processor.CountLinkedPagesSymbols "ololo")
    result.Length |> should equal 1
    result |> should contain ("https://www.ololo.com/", 35)

[<Test>]
let ``Multiple pages`` () =
    let workflow1 = async { return "1" }
    let workflow2 = async { return "12" }
    let workflow3 = async { return "123" }
    let workflow4 = async { return "1234" }
    let workflow5 = async { return "12345" }
    let html = "<a href=\"https://www.first.com/\" />
        <a href=\"https://www.second.com/\" />
        <a href=\"https://www.third.com/\" />
        <a href=\"https://www.fourth.com/\" />
        <a href=\"https://www.fifth.com/\" />"
    let baseWorkflow = async { return html }

    let loaderMock =
        Mock<IHtmlLoader>()
            .Setup(fun x -> <@ x.LoadHtmlAsync("base") @>).Returns(baseWorkflow)
            .Setup(fun x -> <@ x.LoadHtmlAsync("https://www.first.com/") @>).Returns(workflow1)
            .Setup(fun x -> <@ x.LoadHtmlAsync("https://www.second.com/") @>).Returns(workflow2)
            .Setup(fun x -> <@ x.LoadHtmlAsync("https://www.third.com/") @>).Returns(workflow3)
            .Setup(fun x -> <@ x.LoadHtmlAsync("https://www.fourth.com/") @>).Returns(workflow4)
            .Setup(fun x -> <@ x.LoadHtmlAsync("https://www.fifth.com/") @>).Returns(workflow5)
            .Create()
    let processor = HtmlProcessor(loaderMock)
    let result = Async.RunSynchronously(processor.CountLinkedPagesSymbols "base")
    result.Length |> should equal 5
    result |> should contain ("https://www.first.com/", 1)
    result |> should contain ("https://www.second.com/", 2)
    result |> should contain ("https://www.third.com/", 3)
    result |> should contain ("https://www.fourth.com/", 4)
    result |> should contain ("https://www.fifth.com/", 5)

[<Test>]
let ``No links on the base page`` () =
    let loaderMock = 
        Mock<IHtmlLoader>().Setup(fun x -> <@ x.LoadHtmlAsync(any()) @>).Returns(async {
            return "<div class=\"col-md-8 page-heading\">
              <h1> Программирование </h1>
              <h3>
                244 группа
              </h3>            
            </div>" }).Create()
    let processor = HtmlProcessor(loaderMock)
    let result = Async.RunSynchronously(processor.CountLinkedPagesSymbols "ololo")
    result.Length |> should equal 0

[<Test>]
let ``Not an http/https link`` () =
    let loaderMock = Mock<IHtmlLoader>().Setup(fun x -> <@ x.LoadHtmlAsync(any()) @>).Returns(async {
        return "<a href=\"/ololo/bebebe\" />" }).Create()
    let processor = HtmlProcessor(loaderMock)
    let result = Async.RunSynchronously(processor.CountLinkedPagesSymbols "ololo")
    result.Length |> should equal 0

[<Test>]
let ``No href attribute`` () =
    let loaderMock = Mock<IHtmlLoader>().Setup(fun x -> <@ x.LoadHtmlAsync(any()) @>).Returns(async {
        return "<a ololo=\"bebebe\" />" }).Create()
    let processor = HtmlProcessor(loaderMock)
    let result = Async.RunSynchronously(processor.CountLinkedPagesSymbols "ololo")
    result.Length |> should equal 0