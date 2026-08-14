# 03_board_verifier.R
#
# Objective 3: verify that the board only contains valid combinations.
#
# A board is a collection of melds. `verify_board()` checks:
#   1. Every meld is a valid run or group (reuses 01_valid_melds.R).
#   2. The tiles are physically possible: each type (number, colour)
#      appears at most as many times as it exists in the pool (2 copies per
#      numbered type, 2 jokers). This also rejects tiles that do not exist
#      (e.g. a 14) or more copies than available spread across melds.
#
# Board representation (shared with 04):
#   - a list of melds, where each meld is a data.frame with columns
#     `number` and `colour` (see 00_tile_pool.R), or
#   - a data.frame with an extra `meld_id` column indicating which meld
#     each tile belongs to.

# Loads 00 and 01 by looking for them in scripts/ upwards from the working
# directory (works even when the wd is not the project root).
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

# --- Board representation -------------------------------------------

# Converts a board (list or data.frame with meld_id) to a list of melds.
board_to_list <- function(board) {
  if (is.data.frame(board)) {
    if (!"meld_id" %in% names(board)) {
      stop("If the board is a data.frame it must have a `meld_id` column")
    }
    melds <- split(board[c("number", "colour")], board$meld_id)
    lapply(unname(melds), function(x) { rownames(x) <- NULL; x })
  } else if (is.list(board)) {
    lapply(board, function(meld) {
      if (!is.data.frame(meld) ||
          !all(c("number", "colour") %in% names(meld))) {
        stop("Each meld must be a data.frame with `number` and `colour` columns")
      }
      meld[c("number", "colour")]
    })
  } else {
    stop("The board must be a list of melds or a data.frame with meld_id")
  }
}

# Converts a list of melds to a data.frame with a `meld_id` column.
board_to_dataframe <- function(melds) {
  if (length(melds) == 0) {
    return(data.frame(meld_id = integer(0), number = integer(0),
                      colour = character(0)))
  }
  melds <- lapply(seq_along(melds), function(i) {
    cbind(meld_id = i, melds[[i]])
  })
  do.call(rbind, melds)
}

# --- Checks ---------------------------------------------------------

# The board tiles must be able to exist in the physical pool.
# Returns list(ok, problems).
check_physical <- function(board_tiles) {
  if (nrow(board_tiles) == 0) {
    return(list(ok = TRUE, problems = character(0)))
  }
  counts <- count_tiles(board_tiles)
  pool_counts <- count_tiles(tile_pool())
  problems <- character(0)
  for (i in seq_len(nrow(counts))) {
    num_i <- counts$number[i]
    col_i <- counts$colour[i]
    matches <- (pool_counts$colour == col_i) &
      ((is.na(pool_counts$number) & is.na(num_i)) |
         (!is.na(pool_counts$number) & !is.na(num_i) & pool_counts$number == num_i))
    max_count <- if (any(matches)) pool_counts$n[which(matches)[1]] else 0
    if (counts$n[i] > max_count) {
      label <- if (is.na(num_i)) "joker" else num_i
      problems <- c(problems, sprintf(
        "Tile %s %s appears %d times (max %d in the pool)",
        label, col_i, counts$n[i], max_count
      ))
    }
  }
  list(ok = length(problems) == 0, problems = problems)
}

# Checks the whole board. Returns TRUE/FALSE, or with `details = TRUE`
# a list with `valid` and `problems` (messages).
verify_board <- function(board, details = FALSE) {
  melds <- board_to_list(board)
  problems <- character(0)

  for (i in seq_along(melds)) {
    if (!is_valid_meld(melds[[i]])) {
      problems <- c(problems, sprintf("Meld %d is not a valid combination", i))
    }
  }

  if (length(melds) > 0) {
    all_tiles <- do.call(rbind, melds)
    physical <- check_physical(all_tiles)
    problems <- c(problems, physical$problems)
  }

  if (details) {
    return(list(valid = length(problems) == 0, problems = problems))
  }
  length(problems) == 0
}
