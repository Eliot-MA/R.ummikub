# tests/test_objective_04.R
#
# Tests for objective 4 (board restructuring).
# Run with:
#   Rscript -e "testthat::test_file('tests/test_objective_04.R')"

source(file.path(dirname(testthat::test_path()), "..", "scripts",
                 "04_board_restructuring.R"))

# source("scripts/04_board_restructuring.R")

library(testthat)

# Finds the move whose description contains `pattern`.
find_move <- function(moves, pattern) {
  for (j in moves) {
    if (grepl(pattern, j$desc)) return(j)
  }
  NULL
}

test_that("add a hand tile to a run at both ends", {
  board <- list(tiles(c(3, 4, 5), c("red", "red", "red")))
  hand <- tiles(c(2, 6, 9), c("red", "red", "red"))
  moves <- find_moves(board, hand)

  j_left <- find_move(moves, "Add 2 red")
  j_right <- find_move(moves, "Add 6 red")
  expect_false(is.null(j_left))
  expect_false(is.null(j_right))
  # 9 red cannot be added (a gap would remain)
  expect_null(find_move(moves, "Add 9 red"))

  # Apply the right move
  expect_true(verify_board(j_right$board))
  expect_equal(nrow(j_right$hand), 2)               # 2 tiles left
  expect_equal(sort(j_right$hand$number), c(2, 9))
  expect_equal(sort(j_right$board[[1]]$number), 3:6)
})

test_that("add a hand tile to a group", {
  board <- list(tiles(c(7, 7, 7), c("red", "blue", "yellow")))
  hand <- tiles(c(7, 7), c("black", "red"))
  moves <- find_moves(board, hand)

  j_black <- find_move(moves, "Add 7 black")
  expect_false(is.null(j_black))
  # 7 red is already in the group: it cannot be added
  expect_null(find_move(moves, "Add 7 red"))

  expect_true(verify_board(j_black$board))
  expect_equal(nrow(j_black$board[[1]]), 4)
  expect_equal(nrow(j_black$hand), 1)
})

test_that("add a joker from the hand", {
  board <- list(tiles(c(3, 4, 5), c("red", "red", "red")))
  hand <- tiles(NA, "wild")
  moves <- find_moves(board, hand)
  j <- find_move(moves, "Add J")
  expect_false(is.null(j))
  expect_equal(nrow(j$hand), 0)
  expect_true(verify_board(j$board))
})

test_that("a joker on the table never returns to the hand", {
  # With a single meld, the joker has nowhere to go: there is no swap move
  # and no move returns the joker to the hand.
  board <- list(tiles(c(3, NA, 5), c("red", "wild", "red")))  # joker = 4
  hand <- tiles(c(4, 8), c("red", "blue"))
  moves <- find_moves(board, hand)
  expect_null(find_move(moves, "Replace the joker"))
  for (j in moves) expect_false(any(is.na(j$hand$number)))
})

test_that("swap joker: replace it and move it to another meld (run)", {
  board <- list(
    tiles(c(3, NA, 5), c("red", "wild", "red")),      # joker = 4
    tiles(c(1, 2, 3), c("blue", "blue", "blue"))      # joker destination
  )
  hand <- tiles(4, "red")
  moves <- find_moves(board, hand)

  j <- find_move(moves,
                 "Replace the joker in meld 1 with 4 red and move the joker to meld 2")
  expect_false(is.null(j))
  expect_true(verify_board(j$board))
  # The joker stays on the table (meld 2), not in the hand
  expect_equal(sort(j$board[[1]]$number), 3:5)
  expect_false(any(is.na(j$board[[1]]$number)))
  expect_true(any(is.na(j$board[[2]]$number)))
  expect_equal(nrow(j$hand), 0)
})

test_that("swap joker in a group (with another destination meld)", {
  board <- list(
    tiles(c(NA, 7, 7), c("wild", "red", "blue")),     # joker = 7
    tiles(c(1, 2, 3), c("blue", "blue", "blue"))      # joker destination
  )
  hand <- tiles(c(7, 7), c("black", "yellow"))
  moves <- find_moves(board, hand)

  j <- find_move(moves,
                 "Replace the joker in meld 1 with 7 black and move the joker to meld 2")
  expect_false(is.null(j))
  expect_true(verify_board(j$board))
  expect_false(any(is.na(j$board[[1]]$number)))       # group complete
  expect_true(any(is.na(j$board[[2]]$number)))         # joker in meld 2
  expect_equal(nrow(j$hand), 1)                        # 7 yellow left
  expect_false(any(is.na(j$hand$number)))              # no joker in the hand
})

test_that("transfer a tile from a run to a group", {
  board <- list(
    tiles(c(3, 4, 5, 6), c("red", "red", "red", "red")),
    tiles(c(6, 6, 6), c("blue", "yellow", "black"))
  )
  hand <- tiles(integer(0), character(0))
  moves <- find_moves(board, hand)

  j <- find_move(moves, "Move 6 red from meld 1 to meld 2")
  expect_false(is.null(j))
  expect_true(verify_board(j$board))
  expect_equal(sort(j$board[[1]]$number), 3:5)
  expect_equal(sort(j$board[[2]]$number), rep(6, 4))
})

test_that("split a run and add a hand tile", {
  board <- list(tiles(2:7, rep("red", 6)))
  hand <- tiles(c(8, 1), c("red", "red"))
  moves <- find_moves(board, hand)

  j <- find_move(moves, "Split meld 1")
  expect_false(is.null(j))
  expect_true(verify_board(j$board))
  # now there are two melds of 4 and 3 tiles
  sizes <- sort(vapply(j$board, nrow, integer(1)))
  expect_equal(sizes, c(3, 4))
  expect_equal(nrow(j$hand), 1)
})

test_that("with no possible moves it returns an empty list", {
  board <- list(tiles(c(3, 4, 5), c("red", "red", "red")))
  hand <- tiles(c(9, 12), c("red", "blue"))
  expect_equal(length(find_moves(board, hand)), 0)
})

test_that("find_moves deduplicates", {
  board <- list(tiles(c(3, 4, 5), c("red", "red", "red")))
  hand <- tiles(6, "red")
  descriptions <- move_descriptions(board, hand)
  expect_equal(length(descriptions), length(unique(descriptions)))
})

test_that("the moves that use the hand remove the tile from it", {
  board <- list(
    tiles(c(3, 4, 5), c("red", "red", "red")),
    tiles(c(7, 7, 7), c("blue", "yellow", "black"))
  )
  hand <- tiles(c(6, 7, 1), c("red", "red", "blue"))
  moves <- find_moves(board, hand)
  expect_true(length(moves) >= 1)
  for (j in moves) {
    expect_true(verify_board(j$board))
  }
})

test_that("find_moves with a data.frame without meld_id gives a clear error", {
  board <- data.frame(number = c(3, 4, 5), colour = c("red", "red", "red"))
  expect_error(find_moves(board, tiles(6, "red")), "meld_id")
})

test_that("find_moves accepts a data.frame with meld_id", {
  board_df <- data.frame(
    meld_id = c(1, 1, 1),
    number = c(3, 4, 5),
    colour = c("red", "red", "red")
  )
  moves <- find_moves(board_df, tiles(6, "red"))
  expect_true(length(moves) >= 1)
})
