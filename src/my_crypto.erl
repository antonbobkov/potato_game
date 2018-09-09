-module(my_crypto).

-export([hash/1, sign/2, verify/3, read_file_key/2]).

hash(Bin) -> crypto:hash(sha256, Bin).

sign(Hash, Key) -> public_key:sign(Hash, none, Key).
verify(Hash, Signature, PubKey) -> public_key:verify(Hash, none, Signature, PubKey).
    

read_file_key(private, FileName) ->
    {ok, PemBin} = file:read_file(FileName),
    [RSAEntry] = public_key:pem_decode(PemBin),
    Key = public_key:pem_entry_decode(RSAEntry),
    Key;

read_file_key(public, FileName) ->
    {ok, Bin} = file:read_file(FileName),
    [{Key, _}] = public_key:ssh_decode(Bin, public_key),
    Key.  
