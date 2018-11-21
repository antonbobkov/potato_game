-module(messages).

-include_lib("stdlib/include/assert.hrl").
-include("potato_records.hrl").


-export([pack_signed/3, pack_unsigned/2, unpack/1]).

pack_signed(GameId, {Address, Data}, Sig) ->
  term_to_binary({GameId, {Address, Data}, Sig}).

pack_unsigned(GameId, Data) ->
  term_to_binary({GameId, Data}).

unpack(Binary) ->
  try binary_to_term(Binary) of
    %% TODO do more validation on data
    {GameId, {Address, Data}, Sig} when is_integer(GameId) ->
      {GameId, {Address, Data}, Sig};
    {GameId, Data} when is_integer(GameId) ->
      {GameId, Data};
    _ -> fail
  catch
    error:badarg -> fail
  end.
