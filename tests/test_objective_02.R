# tests/test_objective_02.R
#
# Tests for objective 2 (probability of drawing a useful tile).
# Run with:
#   Rscript -e "testthat::test_file('tests/test_objective_02.R')"

source(file.path(dirname(testthat::test_path()), "..", "scripts",
                 "02_draw_probability.R"))

library(testthat)

test_that("count_tiles of the complete pool", {
  cnt <- count_tiles(tile_pool())
  expect_equal(nrow(cnt), 53)                 # 52 numbered types + joker
  expect_true(all(cnt$n == 2))                # 2 copies of each type
  expect_equal(sum(cnt$n), 106)
})

test_that("available_pool subtracts hand and board", {
  hand <- tiles(5, "red")
  expect_equal(sum(available_pool(hand)$n), 105)
  expect_equal(sum(available_pool(hand, board = tiles(5, "red"))$n), 104)
  # n_opponent does not change the counts per type (unknown tiles),
  # only the total number of tiles left to draw.
  expect_equal(sum(available_pool(hand, n_opponent = 10)$n), 105)
  expect_equal(attr(available_pool(hand, n_opponent = 10), "total_available"), 95)
})

test_that("available_pool rejects tiles that do not exist", {
  expect_error(available_pool(board = tiles(c(5, 5, 5), rep("red", 3))),
               "More tiles")
})

test_that("probability with an empty hand (only jokers useful)", {
  expect_equal(prob_draw_useful(), 2 / 106)
})


test_that("probability with one tile in the hand", {
  hand <- tiles(5, "red")
  # useful: 4 red (2), 6 red (2), 5 in the other 3 colours (6), jokers (2)
  expect_equal(prob_draw_useful(hand), 12 / 105)
})

test_that("probability with a nearly complete group", {
  hand <- tiles(c(7, 7, 7), c("red", "blue", "yellow"))
  # useful: 6 and 8 of the same colour as the 7 (12), 7 black (2), jokers (2) = 16
  expect_equal(prob_draw_useful(hand), 16 / 103)
})

test_that("probability with a run of 3 in the hand", {
  hand <- tiles(c(1, 2, 3), c("red", "red", "red"))
  # useful: 4 red (2), remaining copies of red 1/2/3 (1 each), 1/2/3 in other
  # colours (18), jokers (2) = 25
  expect_equal(prob_draw_useful(hand), 25 / 103)
})

test_that("the board reduces the useful tiles available", {
  hand <- tiles(5, "red")
  board <- tiles(4, "red")
  # useful: 4 red (1 left), 6 red (2), 5 other colours (6), jokers (2) = 11
  expect_equal(prob_draw_useful(hand, board), 11 / 104)
})

test_that("n_opponent does not change the probability of one draw", {
  hand <- tiles(5, "red")
  p0 <- prob_draw_useful(hand, n_opponent = 0)
  p5 <- prob_draw_useful(hand, n_opponent = 5)
  p20 <- prob_draw_useful(hand, n_opponent = 20)
  expect_equal(p0, p5)
  expect_equal(p0, p20)
})

test_that("n_opponent out of range fails", {
  expect_error(prob_draw_useful(tiles(5, "red"), n_opponent = 200))
})

test_that("custom is_useful criterion", {
  only_red <- function(tile, hand) tile$colour == "red"
  expect_equal(prob_draw_useful(is_useful = only_red), 26 / 106)
})

test_that("useful_tiles returns the useful types with their count", {
  hand <- tiles(5, "red")
  u <- useful_tiles(hand)
  expect_equal(sum(u$n), 12)
  expect_true(all(u$colour != "red" | u$number %in% c(4, 6)))
})
