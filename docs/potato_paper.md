# Potato Paper

## Proof-of-Potato
Proof-of-Potato (PoP) is a protocol for running decentralized sharded off-chain state machines.
PoP is a potato smart contract that manages potato shard ownership and tracks potato verifiers.
Each potato shard runs a blockchain based potato state machine using our novel Proof-of-Tater (PoT) potato consensus algorithm.
Other consensus protocols are possible too.
We choose PoT because it offers lightweight security, speed, simplicity, and energy-efficient consensus...
Just like a potato.

### Potato Contract
The potato contract is a potato smart contract.
The blockchain hosting the potato contract is called the potato main chain.
The potato contract can be hosted on either a fully decentralized blockchain like Ethereum or a trusted private or permissioned server.

**Potato Verifiers**:
 Users aspiring to be potato verifiers can stake in potatoes in the potato contract to become verifiers.
 The contract manages a sorted list of all stakes over all time and assigns each potato shard to 5 potato verifiers.
 Anyone can query the potato contract for the list of potato verifiers for any given shard at any point in time.


**Potato Shards**:
 To prevent sybil attacks and manage economies in cross-potato shard transaction enabled systems, it’s necessary to have a registry of all potato shards on the potato main chain.
 The potato shard registry can also be used to store and modify additional potato data such as who is the owner, hash of a recent plant state, list of users, etc.


### Potato Shards
Potato shards are operated by potato verifiers assigned by the potato contract.
Anyone can query the potato main chain for this info.
With this protocol, there is a lot of flexibility in state machine and consensus algorithm choice.


**Proof-Of-Tater**:
 There may be many potato shards either inactive or holding little value so we want a protocol where verifiers may be offline.
 This makes a consensus algorithm like PBFT inappropriate.
 Instead we use our own novel permissioned Proof-of-Tater consensus algorithms which tolerates offline potato verifiers.
 <TODO figure out how many can be offline and what are the tradeoffs>
 <TODO incorporate PoT doc here> https://docs.google.com/document/d/1veJ8_Ix6HhdwuQLo60s2BLyopK1RfYR5qQmNF_7SoNo/

**State machine restrictions**:
 The current potato implementation of PoT expects a blockchain based potato state machine.
 Each potato block contains a list of explicit potato state transitions (i.e. potato transactions or potato game actions).
 There may also be an implicit potato state update at the end of the potato block (e.g. potato verifier rewards or world update).
 The potato state machine state can be built by applying all transactions in all blocks in order.
 The potato block can optionally store a potato state hash in a potato merkle tree which is needed for secure cross-potato shard transactions.

**Cross-potato shard transactions**
 <TODO incorporate doc> https://docs.google.com/document/d/1RCwBOfJ023zHt-6VK8SEWGf9OAX4N7hroeAZKDRL3_0/

## Potato Game
Potato Game is the Proof-of-Potato flagship game.
Each potato shard in Potato Game is a giant potato plant in a patch of dirt.
Players collaborate to excavate the land and build creatively above and below ground.
With hard work, they may discover secrets of the mysterious giant potato plant!

### Terminology

**The Potato Plantation**:
 The official Potato Game potato contract (likely hosted on Ethereum).
**Potatoes**:
 Potato verifiers earn potatoes as they tend to the potato shards they are assigned to.
 Potatoes can be withdrawn with a cross-potato shard transaction between the pototao shard and the potato main chain (where they are converted to Potato ERC20 tokens).
 Potatoes can be invested into the potato contract (as stake) or used to buy hats or p2w your potato shards idk.

### Goals
Potato game is primarily a sandbox style game were creativity and collaboration is the main purpose of the game.
Some secrets hidden in the world may unlock still more secrets! But it’s a secret!





# SCRATCH SPACE

### secure cross potato shard transactions is the face of state loss

 In potato game, we expect many abandoned, inactive, or low volume games with verifiers and players losing state.
 In general, when a player connects to a game, they will contact the most recent verifiers in order to download the latest state.
 If a recent verifier is offline, then the player may download state from an older verifier and continue from there.
 If the recent verifier has newer state and comes back online, there will be a fork resolution between these two states.
 We expect this to happen infrequently but consider a natural state of the protocol that is caused by inattentive and not malicious verifiers.
 This poses a security risk for cross-potato shard transactions.

In particular, we would not want a player to commit a state transfer,
withdraw potato tokens on the potato main chain,
roll back past the state transfer on the potato shard,
and withdraw those potatoes again.


To solve the particular example given above, we propose 2 solutions which may not be mutually exclusive

1. state withdrawals on the potato main chain also commit potato state hash meaning it is impossible to roll back before this point.
 This also allows for secure potato shard-potato shard transactions albeit requiring state to be committed on the potato main chain.
 This runs the risk of players unable to continue their game if state is loss D:.
 If the potato shard world is not one of limited resources,
 then it could be acceptable to hold the player responsible for storing state and restarting the game in these cases.

2. for linear state withdrawals, i.e. state withdrawals that transfer all of some linear asset (e.g. all potato tokens),
 it's possible to checkmark on the potato the last block where state was withdraw for example block 5000.
 Should the game state fork prior to block 5000, the user can only withdraw state created after block 5000.
