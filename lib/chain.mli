type blockChain;;

val initChain : unit -> blockChain;;

val addBlock : string -> blockChain -> blockChain;;

val isValidChain : blockChain -> bool;;

val replaceChain : blockChain -> blockChain -> blockChain;;

val yojson_of_blockChain : blockChain -> Yojson.Safe.t;;

val blockChain_of_yojson : Yojson.Safe.t -> blockChain;;
