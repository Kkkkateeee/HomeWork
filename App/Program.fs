type countMsg =
    | Die
    | Incr of int
    | Fetch of AsyncReplyChannel<int>
    
type counter() =
    let innerCounter =
        MailboxProcessor.Start(fun inbox ->
            let rec loop n =
                async { let! msg = inbox.Receive()
                        match msg with
                        | Die -> return ()
                        | Incr x -> return! loop(n + x)
                        | Fetch(reply) ->
                            reply.Reply(n);
                            return! loop n }
            loop 0)
            
    member this.Incr(x) = innerCounter.Post(Incr x)
    member this.Fetch() = innerCounter.PostAndReply((fun reply -> Fetch(reply)), timeout = 2000)
    member this.Die() = innerCounter.Post(Die)