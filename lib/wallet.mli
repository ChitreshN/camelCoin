type wallet;;

type pubWallet;;

type transaction;;

val initWallet : unit -> wallet;;

val transaction : wallet-> wallet -> int -> transaction;;

val sign : Cryptokit.RSA.key -> string -> string;;
