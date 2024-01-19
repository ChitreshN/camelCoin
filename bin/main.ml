open Blockchain;;

let mine chainRef req = 
    let open Lwt.Syntax in

    let* body = Dream.body req in
    chainRef := Chain.addBlock body !chainRef; (*side effects*)

    Dream.redirect req "/blocks" ;;

let sendChain oldChaiRef req = 
    let open Lwt.Syntax in

    let* body   = Dream.body req in
    let chain   = Chain.blockChain_of_yojson @@ Yojson.Safe.from_string body in
    oldChaiRef := Chain.replaceChain !oldChaiRef chain; 

    Dream.json 
    @@ Yojson.Safe.to_string
    @@ Chain.yojson_of_blockChain !oldChaiRef;;

let isChainValid chainRef _ = 
    match Chain.isValidChain !chainRef with
    | true  -> Dream.respond  "Valid"
    | false -> Dream.respond "Not valid"

let () = 
    let chain = ref (Chain.initChain()) in

    let server = 
    Dream.serve 
    @@ Dream.logger
    @@ Dream.router [
        Dream.get "/isChainValid"
        @@ isChainValid chain ;
        Dream.get "/blocks" (fun _ -> Dream.json 
        @@ Yojson.Safe.to_string 
        @@ Chain.yojson_of_blockChain !chain);
        Dream.post "/mine" 
        @@ mine chain;
        Dream.post "/sendChain"
        @@ sendChain chain;
    ] in

    Lwt_main.run server;;
