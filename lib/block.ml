open Ppx_yojson_conv_lib.Yojson_conv.Primitives;;

type block = {
    timestamp         : float;
    transaction       : string;
    prev_block_hash   : string;
    hash              : string;
    height            : int;
    nounce            : int;
}[@@deriving yojson]
;;

let diff = 4;;

let getHash block = 
    block.hash;;

let getPrevHash block = 
    block.prev_block_hash;;

let initBlock timestamp prev_hash hash data height nounce  = {
    timestamp       = timestamp;
    transaction     = data;
    prev_block_hash = prev_hash;
    hash            = hash;
    height          = height;
    nounce          = nounce;
}

let hash inp = 
    let open Digestif.SHA256 in
    let rawHash = digest_string inp in
    to_hex rawHash;;

let blockHash blk   = hash (string_of_float blk.timestamp 
                        ^ blk.prev_block_hash 
                        ^ blk.transaction 
                        ^ string_of_int blk.nounce)

let genesisBlock()  = initBlock 0.0 "" (hash(string_of_float 0.0 ^ "" ^ "") ) "" 0 0;;

let rec mineBlock lastBlock data nounce = 

    let lastHash    = lastBlock.hash in
    let timeStamp   = Unix.time() in
    let height      = lastBlock.height + 1 in

    let hash        = hash (string_of_float timeStamp ^ lastHash ^ data ^ string_of_int nounce) in

    match String.sub hash 0 diff with 
    (*do something to generate strings of length diff instead of hardcoding 0's*)
    | "0000"    -> initBlock timeStamp lastHash hash data height nounce
    | _         -> mineBlock lastBlock data (nounce + 1)

let yojson_of_block block = yojson_of_block block;;

let block_of_yojson block = block_of_yojson block;;
