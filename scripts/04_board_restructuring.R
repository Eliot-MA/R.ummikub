# 04_board_restructuring.R
#
# Objective 4: rearrange the melds on the table to place tiles from the hand.
#
# In Rummikub, on later turns you can break and rearrange the combos on the
# table as long as everything remains valid when the turn ends. Here we
# model the atomic moves on a board:
#
#   1. `add_to_meld()`: place ONE tile (from the hand or from another meld)
#      on an existing meld (extend a run at its ends, fill a free colour of
#      a group, or use a joker).
#   2. `move_add_tile()`: moves that place a tile from the hand.
#   3. `move_swap_joker()`: replace a joker on the table with the real tile
#      it represents and move the freed joker to ANOTHER meld.
#      Rule: a wild never returns to the hand (only hand -> board or
#      board -> board).
#   4. `move_transfer_tile()`: move a tile from one meld to another (leaves
#      the source meld valid and the destination meld valid). Does not place
#      a tile from the hand: it prepares the ground for other moves.
#   5. `move_split_and_add()`: split a run into two valid runs and add
#      a tile from the hand to one of the parts.
#   6. `find_moves()`: gathers all the previous moves and returns them
#      deduplicated. Each move is a list with:
#        $board -> resulting board (list of melds)
#        $hand  -> resulting hand (data.frame)
#        $desc  -> readable description
#
# All moves are validated with `verify_board()` (objective 3).

# Loading of shared scripts (search in scripts/ upwards from the wd).
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
source_local("01_valid_melds.R")
source_local("03_board_verifier.R")

# --- Utilities ------------------------------------------------------

tile_label <- function(tile) {
  if (is_joker(tile$number, tile$colour)) {
    "J"
  } else {
    paste(tile$number, tile$colour)
  }
}

meld_type <- function(meld) {
  if (is_run(meld)) return("run")
  if (is_group(meld)) return("group")
  "invalid"
}

# Resolved melds (jokers with their canonical value), or NULL if invalid.
resolved_meld <- function(meld) {
  if (is_run(meld)) return(assign_run(meld))
  if (is_group(meld)) return(assign_group(meld))
  NULL
}

move <- function(board, hand, desc) {
  list(board = board, hand = hand, desc = desc)
}

# Replaces meld `m` of `board` with the melds in `new_melds` (1 or more).
replace_meld <- function(board, m, new_melds) {
  if (length(new_melds) == 1) {
    board[[m]] <- new_melds[[1]]
    return(board)
  }
  c(board[seq_len(m - 1)],
    new_melds,
    if (m < length(board)) board[(m + 1):length(board)])
}

# --- Adding a tile to a meld ----------------------------------------

# Tries to place `tile` (1 row) in `meld`. Returns the resulting meld if
# valid, or NULL if it cannot be done.
add_to_meld <- function(meld, tile) {
  type <- meld_type(meld)
  if (type == "invalid") return(NULL)
  res <- resolved_meld(meld)
  new_meld <- rbind(meld, tile)
  rownames(new_meld) <- NULL

  if (is_joker(tile$number, tile$colour)) {
    # A joker can extend a run or fill a colour in a group.
    limit <- if (type == "run") 13 else 4
    if (nrow(new_meld) <= limit && is_valid_meld(new_meld)) return(new_meld)
    return(NULL)
  }

  if (type == "run") {
    if (tile$colour != unique(res$colour)) return(NULL)
    num <- tile$number
    if (num < 1 || num > 13) return(NULL)
    if (num == min(res$number) - 1 || num == max(res$number) + 1) {
      if (is_valid_meld(new_meld)) return(new_meld)
    }
    return(NULL)
  }

  # group
  if (tile$number != unique(res$number)) return(NULL)
  if (tile$colour %in% res$colour) return(NULL)
  if (nrow(new_meld) <= 4 && is_valid_meld(new_meld)) return(new_meld)
  NULL
}

# --- Moves ----------------------------------------------------------

# Place a tile from the hand on an existing meld.
move_add_tile <- function(board, hand) {
  if (nrow(hand) == 0) return(list())
  moves <- list()
  for (h in seq_len(nrow(hand))) {
    tile <- hand[h, , drop = FALSE]
    for (m in seq_along(board)) {
      new_meld <- add_to_meld(board[[m]], tile)
      if (is.null(new_meld)) next
      board2 <- board
      board2[[m]] <- new_meld
      hand2 <- hand[-h, , drop = FALSE]
      if (verify_board(board2)) {
        moves <- append(moves, list(move(
          board = board2, hand = hand2,
          desc = sprintf("Add %s to meld %d", tile_label(tile), m)
        )))
      }
    }
  }
  moves
}

# Replace a joker on the table with a real tile from the hand and place the
# freed joker on ANOTHER meld on the table. The joker never goes to the hand.
move_swap_joker <- function(board, hand) {
  if (nrow(hand) == 0) return(list())
  moves <- list()
  for (m in seq_along(board)) {
    meld <- board[[m]]
    joker_positions <- which(is.na(meld$number))
    if (length(joker_positions) == 0) next
    for (p in joker_positions) {
      joker <- meld[p, , drop = FALSE]
      for (h in seq_len(nrow(hand))) {
        tile <- hand[h, , drop = FALSE]
        if (is_joker(tile$number, tile$colour)) next
        meld_without_joker <- rbind(meld[-p, , drop = FALSE], tile)
        rownames(meld_without_joker) <- NULL
        if (!is_valid_meld(meld_without_joker)) next
        for (m2 in seq_along(board)) {
          if (m2 == m) next
          target_meld <- add_to_meld(board[[m2]], joker)
          if (is.null(target_meld)) next
          board2 <- board
          board2[[m]] <- meld_without_joker
          board2[[m2]] <- target_meld
          hand2 <- hand[-h, , drop = FALSE]
          if (verify_board(board2)) {
            moves <- append(moves, list(move(
              board = board2, hand = hand2,
              desc = sprintf(
                "Replace the joker in meld %d with %s and move the joker to meld %d",
                m, tile_label(tile), m2)
            )))
          }
        }
      }
    }
  }
  moves
}

# Indices of the tiles in `meld` that can be removed leaving a valid meld.
movable_tiles <- function(meld) {
  if (nrow(meld) < 4) return(integer(0))
  res <- integer(0)
  for (i in seq_len(nrow(meld))) {
    remainder <- meld[-i, , drop = FALSE]
    if (nrow(remainder) >= 3 && is_valid_meld(remainder)) res <- c(res, i)
  }
  res
}

# Move a tile from one meld to another. Does not place tiles from the hand.
move_transfer_tile <- function(board, hand) {
  moves <- list()
  for (m in seq_along(board)) {
    for (idx in movable_tiles(board[[m]])) {
      tile <- board[[m]][idx, , drop = FALSE]
      remainder <- board[[m]][-idx, , drop = FALSE]
      for (m2 in seq_along(board)) {
        if (m2 == m) next
        new_meld <- add_to_meld(board[[m2]], tile)
        if (is.null(new_meld)) next
        board2 <- board
        board2[[m]] <- remainder
        board2[[m2]] <- new_meld
        if (verify_board(board2)) {
          moves <- append(moves, list(move(
            board = board2, hand = hand,
            desc = sprintf("Move %s from meld %d to meld %d",
                           tile_label(tile), m, m2)
          )))
        }
      }
    }
  }
  moves
}

# Splits of a run (without jokers) into two valid runs of >= 3 tiles.
split_run <- function(meld) {
  if (meld_type(meld) != "run" || any(is.na(meld$number))) return(list())
  nums <- sort(meld$number)
  col <- unique(meld$colour)
  n <- length(nums)
  if (n < 6) return(list())
  splits <- list()
  for (k in 3:(n - 3)) {
    splits <- append(splits, list(list(
      part1 = tiles(nums[seq_len(k)], rep(col, k)),
      part2 = tiles(nums[(k + 1):n], rep(col, n - k)),
      split_at = nums[k]
    )))
  }
  splits
}

# Split a run into two and add a tile from the hand to one of the parts.
move_split_and_add <- function(board, hand) {
  if (nrow(hand) == 0) return(list())
  moves <- list()
  for (m in seq_along(board)) {
    splits <- split_run(board[[m]])
    if (length(splits) == 0) next
    for (sp in splits) {
      for (h in seq_len(nrow(hand))) {
        tile <- hand[h, , drop = FALSE]
        for (part in c("part1", "part2")) {
          other <- if (part == "part1") "part2" else "part1"
          extended <- add_to_meld(sp[[part]], tile)
          if (is.null(extended)) next
          board2 <- replace_meld(board, m, list(extended, sp[[other]]))
          hand2 <- hand[-h, , drop = FALSE]
          if (verify_board(board2)) {
            moves <- append(moves, list(move(
              board = board2, hand = hand2,
              desc = sprintf("Split meld %d after %d and add %s to %s",
                             m, sp$split_at, tile_label(tile), part)
            )))
          }
        }
      }
    }
  }
  moves
}

# All possible moves, deduplicated by description.
# `board` can be a list of melds or a data.frame with `meld_id`.
find_moves <- function(board, hand) {
  board <- board_to_list(board)
  moves <- list(
    move_add_tile(board, hand),
    move_swap_joker(board, hand),
    move_transfer_tile(board, hand),
    move_split_and_add(board, hand)
  )
  moves <- unlist(moves, recursive = FALSE)
  descriptions <- vapply(moves, function(x) x$desc, character(1))
  moves[!duplicated(descriptions)]
}

# Descriptions of the moves (convenient for debugging).
move_descriptions <- function(board, hand) {
  vapply(find_moves(board, hand), function(x) x$desc, character(1))
}
