open Ppx_yojson_conv_lib.Yojson_conv.Primitives;;

type publicKey = {
    size : int;
    n : string; (*modulo*)
    e : string;  (*public exponent*)
}[@@deriving yojson]

type wallet = {
    balance     : int;
    keyPair     : Cryptokit.RSA.key;
    publicKey   : publicKey;
}  

type pubWallet = {
    balance     : int;
    publicKey   : publicKey;
}[@@deriving yojson] 

type transaction = {
    id      : string;
    outputs : pubWallet * pubWallet;
    signature: string
}[@@deriving yojson]


let initWallet() = 
    let open Cryptokit.RSA in
    let keyPair = new_key 100 in
    {
        balance = 500;
        keyPair = keyPair;
        publicKey = {
            size = keyPair.size;
            n = keyPair.n;
            e = keyPair.e
        }
    };;

let signTransaction id sender = 
    let stringRepr = id in
    let hashed = Utils.hash stringRepr in
    Utils.sign sender.keyPair hashed;;

let verifyTransaction key signature dataHash = 
    let size    = key.size in
    let n       = key.n in
    let e       = key.e in

    match dataHash = Utils.verifySignature size n e signature with
    true    -> "Valid"
    | false -> "In valid";;


let transaction (sender:wallet) (reciever:wallet) amount = 
    let pubSender = {
        balance = sender.balance; publicKey = sender.publicKey
    }
    in

    let pubReciever = {
        balance = reciever.balance; publicKey = reciever.publicKey
    }
    in

    let _transaction sender reciever amount = 
        match amount >= sender.balance with
    true ->
        let newSender = {
            balance     = sender.balance - amount;
            publicKey   = sender.publicKey
    } in
        let newReciever = {
            balance     = reciever.balance + amount;
            publicKey   = reciever.publicKey
        } in
        (newSender,newReciever)
    | false -> print_endline "no cash mate";(sender,reciever) in

    let pubSender,pubReciever = _transaction pubSender pubReciever amount in
    let timeStamp   = string_of_float @@ Unix.time() in
    let id = timeStamp in
    {
        id = timeStamp;
        outputs = pubSender, pubReciever;
        signature = signTransaction id sender
    }

