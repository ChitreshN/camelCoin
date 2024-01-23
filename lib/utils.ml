(*utils*)
let hash inp = 
    let open Digestif.SHA256 in
    let rawHash = digest_string inp in
    to_hex rawHash;;

let sign keyPair data = 
    let open Cryptokit.RSA in
    let hash = hash data in
    sign keyPair hash
