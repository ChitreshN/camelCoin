type wallet;;

type transaction;;

val initWallet : unit -> wallet;;

val transaction : wallet-> wallet -> int -> transaction;;

val signTransaction : transaction -> wallet -> string;;
