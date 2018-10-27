-module(pop_chain).

-export([
	 new/1,
	 add_block_in_order/3,
	 add_transaction/2,
	 generate_new_block/2,

	 find_block_by_id/2,
	 get_genisys_block/1,
	 get_head_block/1,

	 get_verfier_next_block_time/2,
	 resolve_fork/3,

	 compute_block_hash/1,
	 compute_transaction_hash/1,

	 apply_block_signature/2,
	 apply_transaction_signature/2
	]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("potato_records.hrl").

map_key_match_assert(Map, KeyList) ->
    K1 = lists:sort(maps:keys(Map)),
    K2 = lists:sort(KeyList),
    ?assertEqual(K1, K2, {"key mismatch", K1, K2}),
    ok.

transaction_map_structure_assert(T) when is_map(T) ->
    map_key_match_assert(T, ?transaction),
    
    CD = maps:get(consensus_data, T),
    map_key_match_assert(CD, ?consensus_transaction_data),
    
    ok.

check_block_map_structure(B) when is_map(B) ->
    map_key_match_assert(B, ?block),
    
    CD = maps:get(consensus_data, B),
    map_key_match_assert(CD, ?consensus_block_data),
    
    TL = maps:get(transactions, B),
    lists:map(fun transaction_map_structure_assert/1, TL),
    
    ok.
    
%% get entry in block's consensus data
get_block_cd(Entry, Block) -> maps:get(Entry, maps:get(consensus_data, Block)).

%% @doc Computes hash of the block.
compute_block_hash(Block) when is_map(Block) ->
    #{consensus_data := CD} = Block,
    CleanBlock = Block#{
	  this_id := undefined, 
	  consensus_data := CD#{signature := undefined}
	 },
    my_crypto:hash( my_serializer:serialize_object(CleanBlock) ).

%% @doc Computes hash of the transaction.
compute_transaction_hash(Transaction) when is_map(Transaction) ->
    CD = maps:get(consensus_data, Transaction),
    CleanTransaction = Transaction#{
	  consensus_data := CD#{signature := undefined}
	 },
    my_crypto:hash( my_serializer:serialize_object(CleanTransaction) ).

%% @doc Inserts signature into the block.
%% 
%% Checks that block's this_id is correct and that signature is correct.

apply_block_signature(Signature, Block) 
  when is_map(Block) ->
    Hash = maps:get(this_id, Block),
    PubKey = get_block_cd(verifier_pub_key, Block),

    ?assertEqual(Hash, compute_block_hash(Block)),
    
    ?assert(true == my_crypto:verify(Hash, Signature, PubKey)),

    CD = maps:get(consensus_data, Block),
    
    Block#{consensus_data := CD#{signature := Signature}}.


%% @doc Inserts signature into the transaction.
%% 
%% Checks that the signature is correct.

apply_transaction_signature(Signature, T) 
  when is_map(T) ->
    Hash = compute_transaction_hash(T),
    PubKey = maps:get(player_id, T),

    ?assert(true == my_crypto:verify(Hash, Signature, PubKey)),

    CD = maps:get(consensus_data, T),
    
    T#{consensus_data := CD#{signature := Signature}}.

check_transaction_correctness(Transaction, ChainId) when is_map(Transaction) ->
    transaction_map_structure_assert(Transaction),

    #{
      consensus_data := CD,
      player_id := PlrKey
     } = Transaction,

    #{
      signature := PlrSgn,
      chain_id := TrChainId
     } = CD,

    Hash = compute_transaction_hash(Transaction),

    %% verify signature's correctness
    ?assert(my_crypto:verify(Hash, PlrSgn, PlrKey), "transaction signature failed verification"),

    %% compare ChainId
    ?assertEqual(TrChainId, ChainId, "bad transaction ChainId"),

    ok.
    
%% @doc Adds block to blocktree.
%% 
%% The block should new and right after already existing block.
%% Block's structure is checked and error is generated if it is incorrect.

add_block_in_order(Block, CurrentTime, ProtocolData)
  when 
      is_record(ProtocolData, pop_chain), 
      is_map(Block)
      ->

    %% checks maps for block and transactions inside
    check_block_map_structure(Block),

    #pop_chain{
       pop_config_data = #pop_config_data{
			    verifiers_arr = VerifiersArr, 
			    time_between_blocks = TimeBetween, 
			    time_desync_margin = TimeDesyncMargin,
			    chain_id = MainChainId
			   },
       tree_data = TD0
      } = ProtocolData,

    

    #{
       previous_id := PrevId, 
       this_id := ThisId, 
       transactions := BlockTransactionsList,
       consensus_data := #{
			   signature := VerSgn,
			   verifier_pub_key := VerKey,
			   verifier_index := VerIndex,
			   timestamp := Tmp
			  }
      } = Block,
    
    
    %% check that signer is one of the verifiers
    #verifier_public_info{
       index = VerIndexChk, 
       public_key = PubKeyChk
      } = array:get(VerIndex, VerifiersArr),

    ?assertEqual(VerIndex, VerIndexChk, "indices don't match"),
    ?assertEqual(VerKey, PubKeyChk, "keys don't match"),


    %% check that the time is correct for this verifier
    %% (also check that it is not too far into the future)
    %% should be larger, and have the correct remainder
    
    PreviousBlock = blocktree:get_block_by_id(PrevId, TD0),
    PreviousBlockTimestamp = maps:get(timestamp, maps:get(consensus_data, PreviousBlock)),

    ?assert(PreviousBlockTimestamp < Tmp, "time should be larger than previous"),
    ?assert(Tmp - TimeDesyncMargin < CurrentTime, "block cannot be in the future"),
    
    VerNum = array:size(VerifiersArr),
    ?assertEqual(Tmp rem (TimeBetween * VerNum), TimeBetween * VerIndex, "bad time for that verifier"),
    
    %% check ThisId hash correctness
    Hash = ThisId,
    ?assertEqual(Hash, compute_block_hash(Block), "incorrect hash"),

    %% check signature's correctness
    ?assert(my_crypto:verify(Hash, VerSgn, VerKey), "signature failed verification"),

    %% (OPTIONAL) check sequence of different verifiers

    %% Verify transactions
    lists:map(fun(T) -> check_transaction_correctness(T, MainChainId) end, BlockTransactionsList),

    %% add this block to tree_data 
    %% this can trigger errors if the block if poorly formed
    %% also fails if block is orphan or already exists
    TD1 = blocktree:add_block_in_order(Block, TD0),

    %% current last block keeps track of the last block in the chain
    %% update it, if new block is the last one


    CurrentHeadBlock = ProtocolData#pop_chain.head_block,
    HeadBlock = resolve_fork(Block, CurrentHeadBlock, TD1),

    %% ?debugVal(CurrentHeadBlock),
    %% ?debugVal(Block),
    %% ?debugVal(HeadBlock),

    NewProtocolData = ProtocolData#pop_chain{tree_data = TD1, head_block = HeadBlock},

    NewProtocolData.

%% @doc Initialize protocol data.
%% 
%% Creates a tree with genesis block with a fixed timestamp.
%% Genesis block's id is <b>genesis</b>, it is the only block with non SHA hash id.
new(PopConfigData) 
  when is_record(PopConfigData, pop_config_data) ->

    CurrentTime = PopConfigData#pop_config_data.init_time,

    ?assert(CurrentTime >= 0),

    TD0 = blocktree:new(),
    B0 = blocktree:generate_new_block(undefined, TD0),
    B1 = B0#{
	     this_id := genesis,
	     consensus_data := #{
				 timestamp => CurrentTime,
				 signature => undefined, 
				 verifier_pub_key => undefined, 
				 verifier_index => undefined
				}
	    },
    check_block_map_structure(B1),
    TD1 = blocktree:add_block_in_order(B1, TD0),

    PC = #pop_chain{
	    pop_config_data = PopConfigData,
	    tree_data = TD1,
	    head_block = B1,
	    genisys_block = B1
	   },
    PC.

%% @doc Resolves a fork between two branches.
%% 
%% Block with larger height wins.
%% If heights are the same, then branches are traced to the first branching point,
%% and then block at branching point with earlier timestamp wins.
%% If those timestamps are equal, block with smaller hash wins.

resolve_fork(B1, B2, TreeData)
  when 
      is_map(B1),
      is_map(B2),
      is_record(TreeData, tree_data) ->

    H1 = maps:get(height, B1),
    H2 = maps:get(height, B2),

    if 
	H1 > H2 -> 
	    B1;

	H2 > H1 -> 
	    B2;

	H2 == H1 -> 
	    Branch = resolve_fork_select_branch(B1, B2, TreeData),
	    if 
		Branch == first ->
		    B1;
		Branch == second ->
		    B2
	    end
	end.

resolve_fork_select_branch(B1, B2, TreeData) ->
    Id1 = maps:get(previous_id, B1),
    Id2 = maps:get(previous_id, B2),
    
    if 
	Id1 == Id2 ->
	    resolve_fork_same_parent(B1, B2);
	Id1 /= Id2 ->
	    BB1 = blocktree:get_block_by_id(Id1, TreeData),
	    BB2 = blocktree:get_block_by_id(Id2, TreeData),
	    resolve_fork_select_branch(BB1, BB2, TreeData)
    end.

resolve_fork_same_parent(B1, B2) ->
    T1 = get_block_cd(timestamp, B1),
    T2 = get_block_cd(timestamp, B2),

    if 
	T1 < T2 ->
	    first;
	T2 < T1 ->
	    second;
	T1 == T2 ->
	    Id1 = maps:get(this_id, B1),
	    Id2 = maps:get(this_id, B2),
	    if 
		Id1 < Id2 ->
		    first;
		Id2 < Id1 ->
		    second
	    end
	    
    end.

%% @doc Get the next appropriate time for a verifier to make a block.

get_verfier_next_block_time(VerifierIndex, PC) 
  when is_record(PC, pop_chain) ->

    #pop_chain{
       pop_config_data = #pop_config_data{
	 verifiers_arr = VerifierArr,
	 time_between_blocks = TimeBetweenBlocks
	},
       head_block = HeadBlock
      } = PC,

    VerNum = array:size(VerifierArr),
    
    LastTime = get_block_cd(timestamp, HeadBlock),
    LastZeroTime = LastTime - (LastTime rem (VerNum * TimeBetweenBlocks)),
    NextTime = LastZeroTime + VerifierIndex * TimeBetweenBlocks,

    if 
	NextTime =< LastTime ->
	    NextTime + VerNum * TimeBetweenBlocks;
	NextTime > LastTime ->
	    NextTime
    end.
    
%% @doc Creates next block in the chain for a given verifier.
%% 
%% Created block is unsigned.
%% (Pending transactions are put into the block by blocktree.)

generate_new_block(VerifierIndex, PC)
  when is_record(PC, pop_chain) ->

    Time = get_verfier_next_block_time(VerifierIndex, PC),

    TD = PC#pop_chain.tree_data,
    
    VerData = array:get(VerifierIndex, PC#pop_chain.pop_config_data#pop_config_data.verifiers_arr),
    VerPub = VerData#verifier_public_info.public_key,

    LastId = maps:get(this_id, PC#pop_chain.head_block),

    B0 = blocktree:generate_new_block(LastId, TD),

    B1 = B0#{
	     consensus_data := #{
				 timestamp => Time,
				 signature => undefined, 
				 verifier_pub_key => VerPub, 
				 verifier_index => VerifierIndex
				}
	    },

    Hash = compute_block_hash(B1),

    B2 = B1#{this_id := Hash},

    check_block_map_structure(B2),

    B2.

%% @doc Finds block by id.
%% 
%% Only looks among non-orphan blocks.
%% Returns {ok, Block} or error.

find_block_by_id(Id, PC)
  when is_record(PC, pop_chain) ->
    maps:find(Id, PC#pop_chain.tree_data#tree_data.block_map).

%% @doc Gets first block.
get_genisys_block(PC) -> PC#pop_chain.genisys_block.

%% @doc Gets last block.
get_head_block(PC) -> PC#pop_chain.head_block.

%% @doc Adds transaction to the structure.
%% 
%% Checks transaction correctness first.
%% Returns {Status, Container} where Status is 
%% ignored_duplicate, updated_old, added_new
add_transaction(T, PC)
  when is_record(PC, pop_chain),
       is_map(T) ->
    
    check_transaction_correctness(T, PC#pop_chain.pop_config_data#pop_config_data.chain_id),

    TD = PC#pop_chain.tree_data,
    {Status, TD1} = blocktree:add_new_transaction(T, TD),
    
    {Status, PC#pop_chain{tree_data = TD1} }.
