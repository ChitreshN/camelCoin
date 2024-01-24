open Ppx_yojson_conv_lib.Yojson_conv.Primitives;;

type publicKey = {
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
}[@@deriving yojson]


let initWallet() = 
    let open Cryptokit.RSA in
    let keyPair = new_key 100 in
    {
        balance = 500;
        keyPair = keyPair;
        publicKey = {
            n = keyPair.n;
            e = keyPair.e
        }
    };;

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
        {
            id = timeStamp;
            outputs = pubSender, pubReciever
        }

let signTransaction trans sender = 
    let stringRepr = Yojson.Safe.to_string @@ yojson_of_transaction trans in
    let hashed = Utils.hash stringRepr in
    Utils.sign sender.keyPair hashed   
