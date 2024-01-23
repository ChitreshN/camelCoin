type wallet;;

type pubWallet;;

val initWallet : unit -> wallet;;

val transaction : wallet-> wallet -> int -> pubWallet*pubWallet;;

val sign : string -> Cryptokit.RSA.key -> string;;
