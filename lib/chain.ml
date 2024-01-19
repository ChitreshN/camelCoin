open Block;;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;;

type blockChain = (block list)[@@deriving yojson];;

let initChain () = 
    [genesisBlock()];;

let addBlock data chain = 
    let lastBlock = List.hd chain in
    mineBlock lastBlock data 0 :: chain;;

let rec isValidChain chain = 
    match chain with
    | []    -> false;
    | [x]   -> if x = genesisBlock() then true else false;
    | x::xs -> if (getPrevHash x <> getHash (List.hd xs)) || (getHash x <> blockHash x) then false
                else isValidChain xs;;

let replaceChain oldChain newChain = 
    match List.length oldChain < List.length newChain with
    | true    -> if isValidChain newChain then newChain else oldChain
    | false   -> oldChain;;

let yojson_of_blockChain chain = yojson_of_blockChain chain;;

let blockChain_of_yojson json = blockChain_of_yojson json;;
