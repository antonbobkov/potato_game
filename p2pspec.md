::: POTATO MESSAGES :::

::establishing connection::

Handshake on connection.
Not necessary?
Every message will be signed as is.
Handshake may be useful to prevent TCP connection spam,
and possible replay attacks for some messages.
Handshake is stateful.

- A->B connect request [conn_uuid, challenge, identity {A, B}]
  - spam when initiating connection
  - retry every 10 minutes
- B->A connect response [conn_uuid, B signed challenge, challenge]
  - B now stores {conn_uuid, A} in active connections
- A->B connect response response [conn_uuid, A signed challenge]
  - A now stores {conn_uuid, B} in active connections
- A<->B ping
  - periodic message to indicate the connection is still alive

:: Verifier to Verifier/Player ::

- send_blocks [Block]
  - sends one or more blocks
  - those may either be newly mined blocks, or after a request for block information
  
:: Verifier/Player to Verifier::

- request_block_range {MyHead, MyHead - 10, UnknownBlock}
  - requests a range of unknown blocks leading up to UnknownBlock
  - verifier should respond with send_blocks
  - verifier should calculate a range blocks are missing
  - if verifier doesn't know MyHead and MyHead - 10, the response is ALL the blocks starting from height 0

:: Player to Verifier::

- send_transactions [Transaction]
  - sends a list of transactions from a player to a verifier

- subscribe {AddressInfo}
  - stateful
  - player subscribes to verifier's messages of newly mined blocks
  - each subscriber is automatically unsubscribed after a minute after subsribe message
  - to keep the subscription, spam subscribe once in a while (kind of like ping command)


:::TCP p2p spec:::

1 connection per verifier/player per game. i.e. two verifiers may have more than one connection with each other

::supersocket (listener)::
- supersocket supervisor that spawns several supersocket_tcplistener gen_server actors to listen for new connections
- upon receiving new connection, passes connection off to handshake_reply

::superclient::
- superviosr that accepts requests to create new connection
- creates a connection. Tries to connect several times up to a timeout period
- upon making a connection, hands it off to handshaker

::handshaker::
- handshaker supervisor receives request to start new handshaker either from superclient or from supersocket
- sends identification + challenge to be signed
- receives identification + challenge, signs and sends back
- receives challenge response, verifies
- if any of the above step fails, close the socket and terminate
- if all succeed, pass socket on to connection supervisor

::connection supervisor::
- upon receiving new connection, check if it already exists, if so pick deterministically
- either close the socket or terminate the existing socket if one exists already
- create connection actor to manage the socket, store this in some priority sorted list or whatever
- to create the connection, use handshake data to determine which game it gets assigned
- if too many connection, remove one of lower priority

::connection::
- create connection with PID of game manager
- terminate connection in game manager dies
- pass all messages to game manager


:::game manager:::
TODO


:::web3:::
TODO
