# 02_draw_probability.R
#
# Objective 2: probability of drawing a useful tile.
#
# Context: we know our hand and the tiles visible on the table; the
# opponent (2 players) has drawn `n_opponent` unknown tiles.
#
# Model: sampling without replacement. The "available" pool is the complete
# pool minus the tiles we see (hand + table). The opponent's tiles are
# unknown: they only reduce the number of remaining tiles.
#
# KEY RESULT (exchangeability): for ONE draw, the probability that the drawn
# tile is useful does NOT depend on `n_opponent`. The tile we draw is
# uniform over the available pool (hand+table removed), with or without the
# opponent's tiles. `n_opponent` only reduces the remaining pool (it matters
# for several draws, not for one).
#
# "Useful" is a configurable criterion (`is_useful`). By default:
#   - joker: always useful
#   - group: same number as a tile in the hand, with a different colour
#   - run:   same colour as a tile in the hand and a consecutive number (±1)
# (heuristic of "improves the hand", not of "completes a meld").

# Loads 00_tile_pool.R by looking for it in scripts/ upwards from the
# working directory (works even when the wd is not the project root).
source_local <- function(rel) {
  dir_current <- normalizePath(getwd(), winslash = "/")
  repeat {
    candidate <- file.path(dir_current, "scripts", rel)
    if (file.exists(candidate)) {
      source(candidate)
      return(invisible(TRUE))
    }
    parent <- dirname(dir_current)
    if (parent == dir_current) break
    dir_current <- parent
  }
  stop("Cannot find scripts/", rel, " from ", getwd())
}

source_local("00_tile_pool.R")

# --- Available pool -------------------------------------------------
# Returns an inventory (number, colour, n); attribute `total_available`
# = tiles remaining after also removing the opponent's.
available_pool <- function(hand = NULL, board = NULL, n_opponent = 0) {
  if (is.null(hand)) hand <- tiles(integer(0), character(0))
  if (is.null(board)) board <- tiles(integer(0), character(0))
  counter <- count_tiles(tile_pool())
  counter <- subtract_tiles(counter, count_tiles(hand))
  counter <- subtract_tiles(counter, count_tiles(board))
  total <- sum(counter$n)
  if (n_opponent < 0 || n_opponent > total) {
    stop("n_opponent must be between 0 and ", total)
  }
  attr(counter, "total_available") <- total - n_opponent
  counter
}

# --- Useful-tile criterion (default) --------------------------------

# `tile`:  data.frame with 1 row and columns number, colour.
# `hand`:  data.frame with columns number, colour.
is_useful_tile <- function(tile, hand) {
  if (is_joker(tile$number, tile$colour)) return(TRUE)
  if (nrow(hand) == 0) return(FALSE)
  num <- tile$number
  col <- tile$colour
  hand_num <- hand$number
  hand_col <- hand$colour

  # group: same number, colour different from the hand's
  same_num <- !is.na(hand_num) & hand_num == num
  if (any(same_num) && !(col %in% unique(hand_col[same_num]))) return(TRUE)

  # run: same colour and consecutive number
  same_col <- hand_col == col & !is.na(hand_num)
  if (any(abs(hand_num[same_col] - num) == 1)) return(TRUE)

  FALSE
}

# --- Probability ----------------------------------------------------

# Tile types available in the pool that `is_useful` considers useful.
useful_tiles <- function(hand = NULL, board = NULL, n_opponent = 0,
                         is_useful = is_useful_tile) {
  if (is.null(hand)) hand <- tiles(integer(0), character(0))
  pool <- available_pool(hand, board, n_opponent)
  useful <- vapply(seq_len(nrow(pool)), function(i) {
    is_useful(pool[i, c("number", "colour"), drop = FALSE], hand)
  }, logical(1))
  pool[useful, , drop = FALSE]
}

# P(draw a useful tile) = number of useful copies / number of available copies.
prob_draw_useful <- function(hand = NULL, board = NULL, n_opponent = 0,
                             is_useful = is_useful_tile) {
  if (is.null(hand)) hand <- tiles(integer(0), character(0))
  pool <- available_pool(hand, board, n_opponent)
  total <- sum(pool$n)
  if (total == 0) return(NA_real_)
  useful <- vapply(seq_len(nrow(pool)), function(i) {
    is_useful(pool[i, c("number", "colour"), drop = FALSE], hand)
  }, logical(1))
  sum(pool$n[useful]) / total
}
