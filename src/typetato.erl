-module(typetato).

-include_lib("stdlib/include/assert.hrl").
-include("potato_records.hrl").

-export([pack_signed/2, pack_unsigned/2, unpack/1]).

-type game_id() :: integer().
-type verifier_id() :: my_crypto:public_key().

%% message types
-type msg_sig() :: binary().
-type msg_data() :: term(). %% TODO not any
-type signed_msg() :: {{my_crypto:public_key(), msg_data()}, msg_sig()}.
-type unsigned_msg() :: any().
-type network_data() :: {game_id, game_id()} | {targeted, {game_id(), verifier_id()}}.

-spec pack_signed(network_data(), signed_msg()) -> erlang:ext_binary().
pack_signed(NetData, {{PubKey, Data}, Sig}) ->
  term_to_binary({NetData, {PubKey, Data}, Sig}).

-spec pack_unsigned(network_data(), unsigned_msg()) -> erlang:ext_binary().
pack_unsigned(NetData, Data) ->
  term_to_binary({NetData, Data}).

-spec validate_netdata(any()) -> boolean().
validate_netdata(NetData) ->
  case NetData of
    {game_id, Id} when is_integer(Id) -> true;
    {targeted, {Id, VerId}} when is_integer(Id) ->
      my_crypto:is_public_key(VerId);
    _ -> false
  end.

-spec unpack(binary()) -> {ok, {network_data(), signed_msg() | unsigned_msg()}} | fail.
unpack(Binary) ->
  try binary_to_term(Binary, [safe]) of
    %% TODO do more validation on data
    {NetData, Stuff} ->
      NetDataOk = validate_netdata(NetData),
      case NetDataOk of
        true -> {ok, {NetData, Stuff}};
        _ -> fail
      end;
    _ -> fail
  catch
    error:badarg -> fail
  end.
