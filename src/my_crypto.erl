-module(my_crypto).

-export([hash/1, sign/2, verify/3, read_file_key/2]).

-spec hash(iodata()) -> binary().
hash(Bin) -> crypto:hash(sha256, Bin).

-spec sign(binary(), public_key:private_key()) -> binary().
sign(_Hash, _PrivateKey) -> <<>>. %%public_key:sign(Hash, none, PrivateKey).

-spec verify(binary(), binary(), public_key:public_key()) -> boolean().
verify(_Hash, _Signature, _PubKey) -> true. %%public_key:verify(Hash, none, Signature, PubKey).

-spec read_file_key(private, string()) -> _.
read_file_key(private, FileName) ->
    {ok, PemBin} = file:read_file(FileName),
    [RSAEntry] = public_key:pem_decode(PemBin),
    _Key = public_key:pem_entry_decode(RSAEntry),
    %% Key;
    FileName;

read_file_key(public, FileName) ->
    {ok, Bin} = file:read_file(FileName),
    [{_Key, _}] = public_key:ssh_decode(Bin, public_key),
    %% Key;
    FileName.
