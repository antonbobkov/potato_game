-module(my_crypto).

-include_lib("eunit/include/eunit.hrl").

-export([hash/1, sign/2, verify/3, read_file_key/2, potato_key/0, is_public_key/1]).

%% -define(MY_CRYPTO_DEBUG, true).

-type public_key() :: crypto:rsa_public().
-type private_key() :: crypto:rsa_private().
-type key_pair() :: {public_key(), private_key()}.

-spec is_public_key(any()) -> boolean().
is_public_key(Key) ->
  case Key of
    [_E, _N] -> true; %% TODO do validation
    _ -> false
  end.

%% Actual function implementations

hash_full(Bin) -> 
    Full = crypto:hash(sha256, Bin),

    %% Copied this online :cat_scream:
    HexString = lists:flatten([io_lib:format("~2.16.0B",[X]) || <<X:8>> <= Full ]),

    HexString.

%% hash_full(Bin) -> 
%%     crypto:hash(sha256, Bin).

sign_full(Hash, PrivateKey) -> 
    {ok, PemBin} = file:read_file(PrivateKey),
    [RSAEntry] = public_key:pem_decode(PemBin),
    ActualPrivateKey = public_key:pem_entry_decode(RSAEntry),
    public_key:sign(binary:list_to_bin(Hash), none, ActualPrivateKey).

%% sign_full(Hash, PrivateKey) -> 
%%     public_key:sign(Hash, none, PrivateKey).

verify_full(Hash, Signature, PubKey) -> 
    public_key:verify(binary:list_to_bin(Hash), none, Signature, PubKey).

read_file_key_full(private, FileName) ->
    {ok, _PemBin} = file:read_file(FileName),
    FileName;

%% read_file_key_full(private, FileName) ->
%%     {ok, PemBin} = file:read_file(FileName),
%%     [RSAEntry] = public_key:pem_decode(PemBin),
%%     Key = public_key:pem_entry_decode(RSAEntry),
%%     Key;


read_file_key_full(public, FileName) ->
    {ok, Bin} = file:read_file(FileName),
    [{Key, _}] = public_key:ssh_decode(Bin, public_key),
    Key.

-spec potato_key() -> key_pair().
potato_key() ->
  crypto:generate_key(rsa, {23434, 65537}).

%% Debugging simplified functions

%% cut hash down to six bytes
hash_debug(Bin) ->
    Full = crypto:hash(sha256, Bin),
    Part = binary:list_to_bin(binary:bin_to_list(Full, 0, 6)),

    %% Copied this online :cat_scream:
    HexString = lists:flatten([io_lib:format("~2.16.0B",[X]) || <<X:8>> <= Part ]),

    HexString.

drop_ext(FileName) ->
    [Out | _] = string:split(FileName, "."),
    Out.

sign_debug(Hash, AnyKey) -> Hash ++ " " ++ drop_ext(AnyKey).
verify_debug(Hash, Signature, PublicKey) -> Signature == sign_debug(Hash, PublicKey).
read_file_key_debug(private, FileName) -> FileName;
read_file_key_debug(public, FileName) -> FileName.

-ifdef(MY_CRYPTO_DEBUG).

hash(Bin) -> hash_debug(Bin).
sign(Hash, PrivateKey) -> sign_debug(Hash, PrivateKey).
verify(Hash, Signature, PubKey) -> verify_debug(Hash, Signature, PubKey).
read_file_key(private, FileName) -> read_file_key_debug(private, FileName);
read_file_key(public, FileName) -> read_file_key_debug(public, FileName).

-else.

hash(Bin) -> hash_full(Bin).
sign(Hash, PrivateKey) -> sign_full(Hash, PrivateKey).
verify(Hash, Signature, PubKey) -> verify_full(Hash, Signature, PubKey).
read_file_key(private, FileName) -> read_file_key_full(private, FileName);
read_file_key(public, FileName) -> read_file_key_full(public, FileName).

-endif.

%% -spec hash(iodata()) -> binary().
%% -spec sign(binary(), public_key:private_key()) -> binary().
%% -spec verify(binary(), binary(), public_key:public_key()) -> boolean().
%% -spec read_file_key(private, string()) -> _.

my_crypto_test() ->
    my_crypto_test_gen(fun hash_debug/1, fun sign_debug/2, fun verify_debug/3, fun read_file_key_debug/2),
    my_crypto_test_gen(fun hash_full/1, fun sign_full/2, fun verify_full/3, fun read_file_key_full/2),

    ok.

my_crypto_test_gen(HashFn, SignFn, VerifyFn, ReadFn) ->
    % HASH

    Message = "hi",
    BinMessage = list_to_binary(Message),
    MessageHash = HashFn(BinMessage),
    %% _ = binary_to_list(MessageHash),
    %% io:format("string ~p hash_sz: ~p hash: ~p ~n", [Message, length(L), L]),

    % RSA PRIVATE
    Key = ReadFn(private, "key1.prv"),

    % RSA PUBLIC
    PubKey = ReadFn(public, "key1.pub"),

    %% io:format("rsa private key ~p~n", [Key]),

    Signature = SignFn(MessageHash, Key),
    VerifyResult = VerifyFn(MessageHash, Signature, PubKey),

    ?assertEqual(VerifyResult, true, "verify didn't work"),

    Key2 = ReadFn(private, "key2.prv"),
    PubKey2 = ReadFn(public, "key2.pub"),

    Signature2 = SignFn(MessageHash, Key2),
    ?assertEqual(VerifyFn(MessageHash, Signature, PubKey2), false, "verify shouldn't have worked"),
    ?assertEqual(VerifyFn(MessageHash, Signature2, PubKey), false, "verify shouldn't have worked"),

    %% io:format("signature ~p verify ~p ~n", [Signature, VerifyResult]),
    %% [Key, PubKey].
    ok.
