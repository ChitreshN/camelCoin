type wallet;;

type publicKey;;

type transaction;;

val initWallet : unit -> wallet;;

val transaction : wallet-> wallet -> int -> transaction;;

val verifyTransaction : publicKey -> string -> string -> string;;

val signTransaction : string -> wallet -> string;;
