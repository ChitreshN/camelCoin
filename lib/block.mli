type block ;;

val getHash : block -> string;;

val getPrevHash : block -> string;;

val genesisBlock :unit -> block;;

val blockHash : block -> string;;

val mineBlock : block -> string -> int -> block;;

val yojson_of_block : block -> Yojson.Safe.t;;

val block_of_yojson : Yojson.Safe.t -> block;;
