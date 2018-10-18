-module(messages).

-include_lib("stdlib/include/assert.hrl").
-include("potato_records.hrl").


-export([pack/2, unpack/1]).

pack(GameId, Data) ->
  term_to_binary({GameId, Data}).

unpack(Binary) ->
  try binary_to_term(Binary) of
    %% TODO do more validation on data
    {GameId, Data} when is_integer(GameId) ->
      {GameId, Data};
    _ -> fail
  catch
    error:badarg -> fail
  end.
