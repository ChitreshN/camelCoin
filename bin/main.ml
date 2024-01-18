open Blockchain;;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;;

let respondPost chainRef req = 
    let open Lwt.Syntax in
    let* body = Dream.body req in
    chainRef := Chain.addBlock body !chainRef; (*side effects*)
    Dream.redirect req "/blocks" ;;

let isChainValid chainRef _ = 
    match Chain.isValidChain !chainRef with
    | true -> Dream.json @@ Yojson.Safe.to_string @@ yojson_of_string "Valid"
    | false -> Dream.json @@ Yojson.Safe.to_string @@ yojson_of_string "Not valid"

let () = 
    let chain = ref (Chain.initChain()) in
    let server = 
    Dream.serve 
    @@ Dream.logger
    @@ Dream.router [
        Dream.get "/blocks" (fun _ -> Dream.json 
        @@ Yojson.Safe.to_string 
        @@ Chain.yojson_of_blockChain !chain);
        Dream.post "/addblock" 
        @@ respondPost chain;(*partial application*)
        Dream.get "/isChainValid"
        @@ isChainValid chain ;(*partial application*)
    ] in
    let pr = Lwt.return @@ print_endline "server started" in
    let jobs = Lwt.join [server;pr] in
    Lwt_main.run jobs;;
