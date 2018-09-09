# Potato Paper

## Proof-of-Potato

Proof-of-Potato (PoP) is a protocol for running decentralized sharded off-chain state machines.
PoP is managed by a potato smart contract hosted on a secure blockchain such as Ethereum.
The potato smart contract manages potato shard ownership and list of potato verifiers.
Each potato shard runs a blockchain based potato state machine using our novel Proof-of-Tater (PoT) potato consensus algorithm.
Other potato consensus protocols are possible too.
We choose PoT because it offers lightweight security, speed, simplicity, and energy-efficient consensus...
Just like a potato.

### Potato Contract

The potato contract is a potato smart contract.
 The blockchain hosting the potato contract is called the potato main chain.


**Potato Verifiers**:
 Users aspiring to be potato verifiers can stake in potatoes in the PC to become farmers.
 The contract manages a sorted list of all stakes over all time and has a public verifiable function assigning each potato shard to 5 potato verifiers.


**Potato Shards**:
 To prevent sybil attacks and manage economies in cross-potato shard transaction enabled systems,
 it’s necessary to have a registry of all potato shards on the potato main chain.
 The potato shard registry can also be used to store and modify additional potato data such as who is the owner, hash of a recent plant state, list of users, etc.


### Potato Shards

Potato Shards are operated by potato verifiers assigned by the potato contract.
 Anyone can query the potato main chain for this info.
 Shards have the benefits of permissioned blockchains, where the potato verifiers are determined by permissionless potato contract henc making each shard permissionless.
 With this protocol, there is a lot of flexibility in state machine and consensus algorithm choice.


**Proof-Of-Tater**:
 For an applications such as games, there may be many potato shards either inactive or holding little value so we expect for verifiers to be offline.
 This makes a consensus algorithm like PBFT inappropriate.
 Instead we use our own novel permissioned Proof-of-Tater consensus algorithms which tolerates offline potato verifiers.
 <TODO figure out how many can be offline and what are the tradeoffs>
 <TODO incorporate PoT doc here> https://docs.google.com/document/d/1veJ8_Ix6HhdwuQLo60s2BLyopK1RfYR5qQmNF_7SoNo/

**State machine restrictions**:
 The current potato implementation of PoT expects a blockchain based potato state machine.
 Each potato block contains a list of explicit potato state transitions
 (i.e. potato transactions or potato game actions)
 and the potato block itself indicates the potato state of all its potat state transitions applied to the previous potato block in order.
 There may also be an implicit potato state update at the end of the potato block (e.g. coinbase transaction, world update).
 The potato block can optionally store potato state hash in a potato merkle tree
 <potato tree?, it's a little different because we only need to store some of the state>
 which is needed for cross-potato shard transactions.

**Cross-potato shard transactions**
<TODO incorporate doc> https://docs.google.com/document/d/1RCwBOfJ023zHt-6VK8SEWGf9OAX4N7hroeAZKDRL3_0/

## Potato Game

Potato Game is the Proof-of-Potato flagship game.
 Each potato shard in Potato Game is a giant potato plant in a patch of dirt.
 Players collaborate to mine the dirt, build creative above and below ground,
 and ultimately unveil the secrets of the mysterious plant hidden underground.

### Terminology

**The Potato Plantation**:
 The official Potato plantation that will host Potato Game officially!
**Potatoes**:
 potato verifiers earn potatoes as they tend to the potato shards they are assigned to.
 Potatoes can be withdrawn with a cross-potato shard transaction between the ptotao shard and the potato main chain
 (where they are converted to official Potato ERC20 tokens).
 Potatoes can be invested into the potato contract (as stake) or used to buy hats or p2w your potato shards idk.

### Goals

Potato game is primarily a sandbox style game were creativity and collaborations is the main purpose of the game.
 Still, some secrets hidden in the world may unlock still more secrets! But it’s a secret!





# SCRATCH SPACE

### secure cross potato shard transactions is the face of state loss

in potato game, we expect many abandoned, inactive, or low volume games with verifiers and players losing state.
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
