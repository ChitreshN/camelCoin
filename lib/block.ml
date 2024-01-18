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

let getHash block = 
    block.hash;;

let getPrevHash block = 
    block.prev_block_hash;;

let initBlock timestamp prev_hash hash data height  = {
    timestamp       = timestamp;
    transaction     = data;
    prev_block_hash = prev_hash;
    hash            = hash;
    height          = height;
    nounce          = 1;
}

let hash inp = 
    let open Digestif.SHA256 in
    let rawHash = digest_string inp in
    to_hex rawHash;;

let blockHash blk   = hash (string_of_float blk.timestamp 
                        ^ blk.prev_block_hash 
                        ^ blk.transaction)

let genesisBlock()  = initBlock 0.0 "" (hash(string_of_float 0.0 ^ "" ^ "") ) "" 0;;

let mineBlock lastBlock data = 
    let lastHash    = lastBlock.hash in
    let timeStamp   = Unix.time() in
    let height      = lastBlock.height + 1 in
    let hash        = hash (string_of_float timeStamp ^ lastHash ^ data) in
    initBlock timeStamp lastHash hash data height ;;

let yojson_of_block block = yojson_of_block block;;

let block_of_yojson block = block_of_yojson block;;
