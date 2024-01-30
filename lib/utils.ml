(*utils*)
let hash inp = 
    let open Digestif.SHA256 in
    let rawHash = digest_string inp in
    to_hex rawHash;;

let sign keyPair data = 
    let open Cryptokit.RSA in
    let hash = hash data in
    sign keyPair hash;;

let verifySignature size n e = 
    let (key: Cryptokit.RSA.key) = {
        size = size;
        n = n;
        e = e;
        d = "";
        p = "";
        q = "";
        dp = "";
        dq = "";
        qinv = "";
    } in
    Cryptokit.RSA.unwrap_signature key
