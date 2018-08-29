-module(sign).

-export([cmd/0]).

-include_lib("stdlib/include/assert.hrl").
-include_lib("public_key/include/public_key.hrl"). 

read_key(private, Name) ->
    {ok, PemBin} = file:read_file(Name),
    [RSAEntry] = public_key:pem_decode(PemBin),
    Key = public_key:pem_entry_decode(RSAEntry),
    Key;

read_key(public, Name) ->
    {ok, Bin} = file:read_file(Name),
    [{Key, _}] = public_key:ssh_decode(Bin, public_key),
    % [{Key, _}] <- spent like literally an hour figuring that one out, so stupid ಠ_ಠ
    Key.


cmd() ->
    % HASH

    Message = "hi",
    BinMessage = list_to_binary(Message),
    MessageHash = crypto:hash(sha256, BinMessage),
    L = binary_to_list(MessageHash),
    io:format("string ~p hash_sz: ~p hash: ~p ~n", [Message, length(L), L]),

    % RSA PRIVATE
    Key = read_key(private, "rsa_private"),

    % RSA PUBLIC
    PubKey = read_key(public, "rsa_private.pub"),

    %% io:format("rsa private key ~p~n", [Key]),

    Signature = public_key:sign(MessageHash, none, Key),
    VerifyResult = public_key:verify(MessageHash, none, Signature, PubKey),
    
    io:format("signature ~p verify ~p ~n", [Signature, VerifyResult]),
    [Key, PubKey].
