-module(my_crypto_test).

-include_lib("eunit/include/eunit.hrl").

-import(my_crypto, [hash/1, sign/2, verify/3, read_file_key/2]).

-include_lib("stdlib/include/assert.hrl").
-include_lib("public_key/include/public_key.hrl").

my_crypto_test() ->
    % HASH

    Message = "hi",
    BinMessage = list_to_binary(Message),
    MessageHash = my_crypto:hash(BinMessage),
    _ = binary_to_list(MessageHash),
    %% io:format("string ~p hash_sz: ~p hash: ~p ~n", [Message, length(L), L]),

    % RSA PRIVATE
    Key = my_crypto:read_file_key(private, "key1.prv"),

    % RSA PUBLIC
    PubKey = my_crypto:read_file_key(public, "key1.pub"),

    %% io:format("rsa private key ~p~n", [Key]),

    Signature = my_crypto:sign(MessageHash, Key),
    VerifyResult = my_crypto:verify(MessageHash, Signature, PubKey),

    ?assertEqual(VerifyResult, true, "verify didn't work"),

    %% Key2 = my_crypto:read_file_key(private, "key2.prv"),
    %% PubKey2 = my_crypto:read_file_key(public, "key2.pub"),

    %% Signature2 = my_crypto:sign(MessageHash, Key2),
    %% ?assertEqual(my_crypto:verify(MessageHash, Signature, PubKey2), false, "verify shouldn't have worked"),
    %% ?assertEqual(my_crypto:verify(MessageHash, Signature2, PubKey), false, "verify shouldn't have worked"),

    %% io:format("signature ~p verify ~p ~n", [Signature, VerifyResult]),
    [Key, PubKey].

