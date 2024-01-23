type publicKey = {
    n : string; (*modulo*)
    e : string  (*public exponent*)
}

type wallet = {
    balance     : int;
    keyPair     : Cryptokit.RSA.key;
    publicKey   : publicKey
}  

type pubWallet = {
    balance     : int;
    publicKey   : publicKey
}  


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

let _transaction sender reciever amount = 
    (*this should only return the balance and publicKey*)
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
    | false -> print_endline "no cash mate";(sender,reciever);;

let transaction (sender:wallet) (reciever:wallet) amount = 
    let pubSender = {
        balance = sender.balance; publicKey = sender.publicKey
        }
    in

    let pubReciever = {
        balance = reciever.balance; publicKey = reciever.publicKey
        }
    in

    _transaction pubSender pubReciever amount;;

let sign dataHash keyPair = Utils.sign keyPair dataHash

