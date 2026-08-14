# 01_valid_melds.R
#
# Objective 1: detection of valid melds.
#
#   - is_run(meld):          is it a run? (3+ consecutive tiles, same
#                            colour; jokers substitute numbers).
#   - is_group(meld):        is it a group? (3-4 tiles with the same number,
#                            different colours; jokers substitute colours).
#   - is_valid_meld(meld):   valid run or group.
#   - assign_run(meld):      resolves the jokers of a run (canonical
#                            number/colour assignment).
#   - assign_group(meld):    resolves the jokers of a group.
#   - meld_score(meld):      points of the meld. Jokers score the value of
#                            the tile they represent (see note).
#   - meets_rule_30(melds):  one or more melds of the opening play add up
#                            to >= 30.
#
# Typical input: a data.frame with columns `number` and `colour`
# (see 00_tile_pool.R).
#
# NOTE on jokers and points: a run with jokers admits several valid
# assignments (e.g. {5, joker, joker} can be 3-4-5, 4-5-6 or 5-6-7) and the
# total changes. `meld_score()` uses the canonical assignment of
# `assign_run()`: first it fills the internal gaps and then it extends to
# the right.

# Loads 00_tile_pool.R by looking for it in scripts/ upwards from the
# working directory (works even when the wd is not the project root,
# e.g. when running the tests).
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

# --- Runs -----------------------------------------------------------

is_run <- function(meld) {
  n <- nrow(meld)
  if (n < 3) return(FALSE)
  nums <- meld$number
  cols <- as.character(meld$colour)
  wild <- is.na(nums)
  if (all(wild)) return(FALSE)                 # only jokers: no real numbers
  if (length(unique(cols[!wild])) > 1) return(FALSE)  # different colours
  real <- sort(nums[!wild])
  if (anyDuplicated(real)) return(FALSE)       # repeated numbers in a run
  j <- sum(wild)
  k <- length(real)
  if (k + j > 13) return(FALSE)                # a run cannot exceed 13 tiles
  gaps <- (max(real) - min(real) + 1) - k
  j >= gaps
}

assign_run <- function(meld) {
  if (!is_run(meld)) stop("The meld is not a valid run")
  nums <- meld$number
  cols <- as.character(meld$colour)
  wild <- is.na(nums)
  col <- unique(cols[!wild])
  real <- sort(nums[!wild])
  j <- sum(wild)
  lo <- min(real)
  hi <- max(real)

  gaps <- (hi - lo + 1) - length(real)
  surplus <- j - gaps
  r_ext <- min(surplus, 13 - hi)              # extends to the right
  l_ext <- surplus - r_ext                    # and the rest to the left

  left <- if (l_ext > 0) (lo - l_ext):(lo - 1) else integer(0)
  right <- if (r_ext > 0) (hi + 1):(hi + r_ext) else integer(0)

  numbers <- c(left, lo:hi, right)
  data.frame(number = numbers, colour = rep(col, length(numbers)))
}

# --- Groups ---------------------------------------------------------

is_group <- function(meld) {
  n <- nrow(meld)
  if (n < 3 || n > 4) return(FALSE)
  nums <- meld$number
  cols <- as.character(meld$colour)
  wild <- is.na(nums)
  if (all(wild)) return(FALSE)                 # only jokers: no real number
  real_num <- nums[!wild]
  real_col <- cols[!wild]
  if (length(unique(real_num)) > 1) return(FALSE)  # different numbers
  if (anyDuplicated(real_col)) return(FALSE)       # repeated colours
  if (any(!(real_col %in% COLOURS))) return(FALSE)
  TRUE
}

assign_group <- function(meld) {
  if (!is_group(meld)) stop("The meld is not a valid group")
  nums <- meld$number
  cols <- as.character(meld$colour)
  wild <- is.na(nums)
  num <- unique(nums[!wild])
  real_colours <- cols[!wild]
  joker_colours <- setdiff(COLOURS, real_colours)[seq_len(sum(wild))]
  data.frame(
    number = rep(num, nrow(meld)),
    colour = c(real_colours, joker_colours)
  )
}

# --- Generic --------------------------------------------------------

is_valid_meld <- function(meld) {
  is_run(meld) || is_group(meld)
}

# Points of the meld (jokers with their canonical value).
meld_score <- function(meld) {
  if (is_run(meld)) {
    sum(assign_run(meld)$number)
  } else if (is_group(meld)) {
    sum(assign_group(meld)$number)
  } else {
    stop("The meld is not valid")
  }
}

# The opening play: sum of the played melds >= 30.
# `melds` can be a single meld or a list of melds.
meets_rule_30 <- function(melds) {
  if (is.data.frame(melds)) melds <- list(melds)
  sum(vapply(melds, meld_score, numeric(1))) >= 30
}
