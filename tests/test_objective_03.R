# tests/test_objective_03.R
#
# Tests for objective 3 (board verifier).
# Run with:
#   Rscript -e "testthat::test_file('tests/test_objective_03.R')"

source(file.path(dirname(testthat::test_path()), "..", "scripts",
                 "03_board_verifier.R"))

#source("scripts/03_board_verifier.R")

library(testthat)

# --- Valid boards ----------------------------------------------------

test_that("empty board is valid", {
  expect_true(verify_board(list()))
  expect_true(verify_board(board_to_dataframe(list())))
})

test_that("a valid run is valid", {
  board <- list(tiles(c(3, 4, 5), c("red", "red", "red")))
  expect_true(verify_board(board))
})


test_that("several valid melds (run + group)", {
  board <- list(
    tiles(c(3, 4, 5), c("red", "red", "red")),
    tiles(c(7, 7, 7), c("blue", "yellow", "black"))
  )
  expect_true(verify_board(board))
})

test_that("the 2 physical copies of a tile can be on the table", {
  board <- list(
    tiles(c(4, 5, 6), c("red", "red", "red")),          # a red 5
    tiles(c(5, 5, 5), c("red", "blue", "yellow"))       # another red 5
  )
  expect_true(verify_board(board))
})

test_that("the 2 jokers can be on the table", {
  board <- list(
    tiles(c(3, NA, 5), c("red", "wild", "red")),   # joker in a run
    tiles(c(8, 8, NA), c("blue", "yellow", "wild")) # joker in a group
  )
  expect_true(verify_board(board))
})

# --- Invalid boards --------------------------------------------------

test_that("a meld with 2 tiles is invalid", {
  board <- list(tiles(c(3, 4), c("red", "red")))
  expect_false(verify_board(board))
})

test_that("a non-consecutive run is invalid", {
  board <- list(tiles(c(2, 4, 6), c("red", "red", "red")))
  expect_false(verify_board(board))
})

test_that("a non-existent tile (14) is invalid", {
  board <- list(tiles(c(12, 13, 14), c("red", "red", "red")))
  expect_false(verify_board(board))
})

test_that("3 copies of a red 5 (only 2 exist physically) is invalid", {
  board <- list(
    tiles(c(4, 5, 6), c("red", "red", "red")),
    tiles(c(5, 5, 5), c("red", "blue", "yellow")),
    tiles(c(5, 6, 7), c("red", "red", "red"))
  )
  expect_false(verify_board(board))
})

test_that("3 jokers (only 2 exist) is invalid", {
  board <- list(
    tiles(c(1, NA, 3), c("blue", "wild", "blue")),
    tiles(c(2, NA, 4), c("red", "wild", "red")),
    tiles(c(NA, 5, 6), c("wild", "black", "black"))
  )
  expect_false(verify_board(board))
})

test_that("an invalid meld is detected next to a valid one", {
  board <- list(
    tiles(c(3, 4, 5), c("red", "red", "red")),
    tiles(c(9, 9, 9), c("red", "blue", "red"))   # repeated colour
  )
  expect_false(verify_board(board))
})

# --- Details and representation --------------------------------------

test_that("details = TRUE returns valid and problems", {
  board <- list(tiles(c(2, 4, 6), c("red", "red", "red")))
  res <- verify_board(board, details = TRUE)
  expect_false(res$valid)
  expect_type(res$problems, "character")
  expect_true(length(res$problems) >= 1)
})

test_that("data.frame with meld_id is equivalent to a list", {
  board_df <- data.frame(
    meld_id = c(1, 1, 1, 2, 2, 2),
    number = c(3, 4, 5, 7, 7, 7),
    colour = c("red", "red", "red", "blue", "yellow", "black")
  )
  expect_true(verify_board(board_df))

  board_df_bad <- data.frame(
    meld_id = c(1, 1, 2, 2),
    number = c(3, 4, 7, 7),
    colour = c("red", "red", "blue", "red")
  )
  expect_false(verify_board(board_df_bad))
})

test_that("data.frame without meld_id fails", {
  expect_error(verify_board(tiles(c(3, 4, 5), c("red", "red", "red"))),
               "meld_id")
})

test_that("board_to_dataframe and board_to_list are inverses", {
  board_list <- list(
    tiles(c(3, 4, 5), c("red", "red", "red")),
    tiles(c(7, 7, 7), c("blue", "yellow", "black"))
  )
  df <- board_to_dataframe(board_list)
  expect_equal(nrow(df), 6)
  expect_equal(names(df), c("meld_id", "number", "colour"))
  expect_true(verify_board(df))
  expect_equal(board_to_list(df), board_list)
})
