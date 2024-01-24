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

let firstBits = String.make 4 '0'

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

let blockHash blk   = Utils.hash (string_of_float blk.timestamp 
                        ^ blk.prev_block_hash 
                        ^ blk.transaction 
                        ^ string_of_int blk.nounce)

let genesisBlock()  = initBlock 0.0 "" (Utils.hash(string_of_float 0.0 ^ "" ^ "") ) "" 0 0;;

let rec mineBlock lastBlock data nounce = 

    let lastHash    = lastBlock.hash in
    let timeStamp   = Unix.time() in
    let height      = lastBlock.height + 1 in

    let hash        = Utils.hash (string_of_float timeStamp 
                                ^ lastHash 
                                ^ data 
                                ^ string_of_int nounce) in

    if String.sub hash 0 diff <> firstBits then 
        initBlock timeStamp lastHash hash data height nounce
    else mineBlock lastBlock data (nounce + 1);;



let yojson_of_block block = yojson_of_block block;;

let block_of_yojson block = block_of_yojson block;;

