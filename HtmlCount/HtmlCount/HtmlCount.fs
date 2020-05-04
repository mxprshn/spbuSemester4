module HtmlCount

open AngleSharp
open System.Net
open System.IO

type IHtmlLoader =
    abstract member LoadHtmlAsync : url: string -> string Async

type HtmlLoader() =
    interface IHtmlLoader with
        member this.LoadHtmlAsync url =
            async {
                let request = WebRequest.Create url
                use! response = request.AsyncGetResponse ()
                use stream = response.GetResponseStream ()
                use reader = new StreamReader (stream)
                return! Async.AwaitTask <| reader.ReadToEndAsync ()
            }

type HtmlProcessor(loader: IHtmlLoader) =
    let context = BrowsingContext.New Configuration.Default

    let findLinks (htmlText: string) =
        async {
            let! document = htmlText |> context.OpenAsync |> Async.AwaitTask;
            return document.QuerySelectorAll "a" |> Seq.map (fun e -> e.GetAttribute "href")
        }

    member this.CountLinkPagesSymbols (url: string) =
        async {
            let! loading = url |> loader.LoadHtmlAsync |> Async.StartChild
            let! baseHtml = loading
            let! links = baseHtml |> findLinks
            do links |> Seq.iter (fun s -> printfn "%s" s)
        }

let ololo = HtmlProcessor (HtmlLoader ())
Async.RunSynchronously (ololo.CountLinkPagesSymbols "http://hwproj.me/courses/34")