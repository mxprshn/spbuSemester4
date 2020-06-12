namespace CustomLazy

open System.Threading

type ILazy<'a> =
    abstract member Get: unit -> 'a

type SingleThreadedLazy<'a>(supplier: unit -> 'a) =
    let mutable supplier = Some(supplier)
    let mutable result = None
    interface ILazy<'a> with
        member this.Get() =
            if (result.IsNone) then
                result <- Some(supplier.Value())
                supplier <- None
            result.Value

type ConcurrentLazy<'a>(supplier: unit -> 'a) =
    let locker = obj()
    let mutable supplier = Some(supplier)
    let mutable result = None
    interface ILazy<'a> with
        member this.Get() =
            match result with
            | Some value -> value 
            | None -> lock locker (fun () ->
                match result with
                | Some value -> value
                | None ->
                    result <- Some(supplier.Value())
                    supplier <- None
                    result.Value)
                        

type LockFreeLazy<'a>(supplier: unit -> 'a) =
    let mutable supplier = Some(supplier)
    let mutable result = None
    interface ILazy<'a> with
        member this.Get() =
            // let mutable startValue = result
            let evaluated = Some(supplier.Value())
            Interlocked.CompareExchange(ref result, evaluated, None) |> ignore
            result.Value

type LazyFactory<'a>() =
    static member CreateSingleThreadedLazy(supplier: unit -> 'a) =
        SingleThreadedLazy supplier :> ILazy<'a>
    static member CreateConcurrentLazy(supplier: unit -> 'a) =
        ConcurrentLazy supplier :> ILazy<'a>
    static member CreateLockFreeLazy(supplier: unit -> 'a) =
        LockFreeLazy supplier :> ILazy<'a>

            
