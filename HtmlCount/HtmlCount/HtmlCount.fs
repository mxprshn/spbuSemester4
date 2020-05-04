module HtmlCount

open FSharp.Data
open System.Net
open System.IO
open System.Text.RegularExpressions

/// Interface for loading html webpages.
type IHtmlLoader =

    /// Loads a web page as html string.
    abstract member LoadHtmlAsync : url: string -> string Async

/// Class for loading html webpages.
type HtmlLoader() =

    interface IHtmlLoader with
    /// Loads a web page as html string.
        member this.LoadHtmlAsync url =
            async {
                let request = WebRequest.Create url
                use! response = request.AsyncGetResponse()
                use stream = response.GetResponseStream()
                use reader = new StreamReader(stream)
                return! Async.AwaitTask <| reader.ReadToEndAsync()
            }

/// Class for performing operations with html webpages.
type HtmlProcessor(loader: IHtmlLoader) =

    let urlRegex = "^(https:|http:)\/\/\S*"

    let findLinks(htmlText: string) =
        async {
            let document = HtmlDocument.Parse htmlText
            let links = 
                document.Descendants ["a"] |>
                Seq.choose (fun n -> n.TryGetAttribute "href") |>
                Seq.map (fun a -> a.Value ()) |>
                Seq.filter (fun s -> Regex.IsMatch(s, urlRegex))
            return links
        }
        
    let countSymbols(url: string) =
        async {
            let! loading = url |> loader.LoadHtmlAsync |> Async.StartChild
            let! html = loading
            return url, html.Length
        }

    /// Loads all pages linked from the specified one and counts the number of symbols on them.
    member this.CountLinkedPagesSymbols(url: string) =
        async {            
            let! loading = url |> loader.LoadHtmlAsync |> Async.StartChild
            let! baseHtml = loading
            let! links = baseHtml |> findLinks
            let! results = links |> Seq.map countSymbols |> Async.Parallel
            results |> Seq.iter (fun r ->
                match r with
                | (u, c) -> printfn "%s --- %i" u c)   
            return results
        }