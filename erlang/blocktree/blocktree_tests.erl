-module(blocktree_tests).

-include("blocktree.hrl").

cmd() ->
    VD = #verifier_data{transaction_map=map:new()},
    T = #transaction{nonce=0, player_id=p1},
    blocktree:add_new_transaction(T, VD).

