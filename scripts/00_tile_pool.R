# 00_tile_pool.R
#
# Definition of the standard Rummikub tile pool (106 tiles):
#   - 104 numbered tiles: numbers 1 to 13 in 4 colours
#     (red, blue, yellow, black) x 2 copies of each combination
#   - 2 jokers
#
# This script is loaded from the others with source() so that everyone uses
# the same tile representation.
#
# Tile representation (one row):
#   - `number`: integer 1:13, or NA for a joker
#   - `colour`: character: "red", "blue", "yellow", "black" or "wild" (joker)
#
# A meld is represented as a data.frame with these same columns.

COLOURS <- c("red", "blue", "yellow", "black")

# Builds the complete pool of 106 tiles.
tile_pool <- function() {
  numbered <- expand.grid(number = 1:13, colour = COLOURS)
  numbered <- numbered[rep(seq_len(nrow(numbered)), each = 2), ]
  rownames(numbered) <- NULL
  jokers <- data.frame(number = rep(NA_integer_, 2), colour = "wild")
  rbind(numbered, jokers)
}

# Builds a meld (data.frame of tiles) from number/colour vectors.
tiles <- function(number, colour) {
  data.frame(number = as.integer(number), colour = colour)
}

# Is it a joker?
is_joker <- function(number, colour) {
  is.na(number) | colour == "wild"
}

# --- Tile inventory (shared by all scripts) ------------------------

# Counts how many copies of each type (number, colour) are in `tiles`.
# Returns a data.frame with columns number, colour, n.
count_tiles <- function(tiles) {
  if (nrow(tiles) == 0) {
    return(data.frame(number = integer(0), colour = character(0), n = integer(0)))
  }
  key <- paste(ifelse(is.na(tiles$number), "J", tiles$number),
               tiles$colour, sep = "|")
  freq <- table(key)
  parts <- strsplit(names(freq), "\\|")
  number <- vapply(parts, function(p) {
    if (p[1] == "J") NA_integer_ else as.integer(p[1])
  }, integer(1))
  colour <- vapply(parts, function(p) p[2], character(1))
  data.frame(number = number, colour = colour, n = as.integer(freq))
}

# Subtracts `subtract` (inventory) from `counter` (inventory).
subtract_tiles <- function(counter, subtract) {
  for (i in seq_len(nrow(subtract))) {
    n_r <- subtract$number[i]
    col_r <- subtract$colour[i]
    matches <- (counter$colour == col_r) &
      ((is.na(counter$number) & is.na(n_r)) |
         (!is.na(counter$number) & !is.na(n_r) & counter$number == n_r))
    if (!any(matches)) {
      stop("Tile not found in the pool: ", n_r, " ", col_r)
    }
    j <- which(matches)[1]
    counter$n[j] <- counter$n[j] - subtract$n[i]
    if (counter$n[j] < 0) {
      stop("More tiles than exist in the pool")
    }
  }
  counter[counter$n > 0, , drop = FALSE]
}
