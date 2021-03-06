
[![Build Status](https://travis-ci.org/antonbobkov/potato_game.svg?branch=master)](https://travis-ci.org/antonbobkov/potato_game)
[![codecov](https://codecov.io/gh/antonbobkov/potato_game/branch/master/graph/badge.svg)](https://codecov.io/gh/antonbobkov/potato_game)
![](https://img.shields.io/github/last-commit/antonbobkov/potato_game.svg?style=flat)
![](https://img.shields.io/github/issues-pr/antonbobkov/potato_game.svg?style=flat)

# Potato Game

Potato game is a proof-of-concept custom blockchain implementation.
The current design allows a small pool of verifiers maintain a blockchain that keep track of a video game server state.
Players can join the game and submit transactions.
The game state cannot be subverted unless the majority of verifiers in the pool are malicious.

See more details at: https://github.com/antonbobkov/potato_game/blob/master/docs/potato_paper.md
