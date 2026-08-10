# R.ummikub

**R** tools to understand and analyze the game of **Rummikub**.
The goal of this project is not to build a playable game with bots (for now),
but to write code that helps us understand the rules, probabilities and
strategies of the game.

## Game rules (summary)

- **Tile pool:** 106 tiles: 104 numbered (numbers 1 to 13, in 4 colours:
  red, blue, yellow and black; 2 copies of each combination) and 2 **jokers**.
- **Meld:** a valid combination of tiles on the table. There are two types:
  - **Run:** 3 or more consecutive tiles of the **same colour**
    (e.g. `4 5 6` blue).
  - **Group:** 3 or 4 tiles of the **same number** in **different colours**
    (e.g. `7` red, `7` blue, `7` black).
- **Opening play:** on the first turn, each player must place on the table
  one or more melds whose **point total is ≥ 30**. Jokers count as the value
  of the tile they represent.
- **Table manipulation:** on later turns you may play by breaking, splitting
  and rearranging the melds already on the table, as long as all combinations
  remain valid when the turn ends.
- **Drawing:** if you cannot (or do not want to) play, you draw a tile from
  the pool.

## Objectives

1. **Valid meld detection.** Functions that identify valid *runs* and
   *groups* from a set of tiles. As a first milestone, check that a given
   meld adds up to at least 30 points (opening-play rule).
2. **Probability of drawing a useful tile.** Given our hand, the tiles on the
   table and the `n` tiles the opponent has drawn, compute the probability
   that the next tile from the pool is useful (completes a run/group, helps
   reach 30, etc.).
3. **Board verifier.** Algorithm that checks that all combinations on the
   board are valid (every meld is a correct run or group and tiles are not
   repeated illegally).
4. **Board restructuring.** Algorithm that, from the existing melds, finds
   new positions to place tiles from the hand (splitting runs, joker swaps,
   merging melds, etc.).
5. **Board visualization.** Simple representation of the game state as a
   `data.frame` or plot, to "see" the table when analyzing games.

> Bots / automatic players are out of scope for now (future work).

## Repository structure

| File                                  | Contents                                                |
|---------------------------------------|---------------------------------------------------------|
| `scripts/00_pool_fichas.R`            | Tile pool definition (shared by all scripts)            |
| `scripts/01_melds_validos.R`          | Objective 1: runs, groups and the 30 rule               |
| `scripts/02_probabilidad_pesca.R`     | Objective 2: probability of drawing a useful tile       |
| `scripts/03_verificador_tablero.R`    | Objective 3: validate board combinations                |
| `scripts/04_reestructuracion_tablero.R` | Objective 4: rearrange existing combos                |
| `scripts/05_visualizacion_tablero.R`  | Objective 5: board as `data.frame` / plot               |
| `tests/`                              | `testthat` tests (`Rscript -e "testthat::test_file('tests/test_objetivo_01.R')"`) |

Scripts load the pool from `00_pool_fichas.R` via `source()`, so that all of
them use the same tile representation.

## Initial assumptions

- Tile representation: **colour** + **number** (or `joker`), with `number`
  as an ordered factor to make runs easy to compute.
- Standard game: **2 jokers**, counting **for the value of the tile they
  represent** (also in the opening 30-point meld).
- The opponent's tiles are unknown but reduce the pool: drawing is
  **sampling without replacement**.
- Objective 2 context: **2 players** (us + 1 opponent).
- Objective 2 uses **exact combinatorial calculation** (not Monte Carlo).

## Current status

- [x] Tile pool and simple probability examples (`Untitled.R`).
- [x] Project structure (README + scripts 00-05 without code).
- [x] **Objective 1**: meld detection (runs and groups, with jokers), point
      total and the 30 rule. Implemented and tested (32 tests).
- [x] **Objective 2**: probability of drawing a useful tile (exact
      combinatorics, configurable `es_util` criterion). Implemented and
      tested (19 tests).
- [x] **Objective 3**: board verifier (every meld valid + physically
      possible tiles). Implemented and tested (22 tests).
- [x] **Objective 4**: board restructuring (add to melds, joker swaps,
      moving tiles between melds, splitting runs; all validated with
      objective 3). Implemented and tested (43 tests).
- [x] **Objective 5**: board visualization (`tablero_a_datos()` data.frame,
      `graficar_tablero()` plot, `mostrar_tablero()` console output).
      Implemented and tested (17 tests).

All 5 objectives are implemented. **Total: 133 tests passing.**

## Open questions / to decide

- Any specific rules variant?
