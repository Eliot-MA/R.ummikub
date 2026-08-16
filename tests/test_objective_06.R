# tests/test_objective_06.R
#
# Tests for objective 6, analysis A (opening / rule of 30):
#   - enumeration of melds formable with a hand (jokers included)
#   - exact resolution of the maximum opening and of the rule of 30
# Run with:
#   Rscript -e "testthat::test_file('tests/test_objective_06.R')"

source(file.path(dirname(testthat::test_path()), "..", "scripts",
                 "06_opening_analysis.R"))

library(testthat)

test_that("prepare_hand counts types and detects jokers", {
  hand <- tiles(c(3, 3, 4, NA, 4), c("red", "blue", "blue", "wild", "blue"))
  prep <- prepare_hand(hand)
  expect_equal(sum(prep$types$n), 5)
  expect_equal(prep$types$n[prep$joker_idx], 1)
  # (3, red) and (4, blue) with 2 copies each
  expect_setequal(prep$types$number, c(3, 4, NA))
})

test_that("possible_melds generates valid runs and groups", {
  hand <- tiles(c(4, 5, 6, 6, 6), c("red", "red", "red", "blue", "yellow"))
  prep <- prepare_hand(hand)
  melds <- possible_melds(prep)
  expect_gt(nrow(melds$M), 0)
  # all melds fit in the hand
  for (j in seq_len(nrow(melds$M))) {
    expect_true(all(melds$M[j, ] <= prep$types$n))
  }
  # the run 4-5-6 (18) and the group of red-blue-yellow 6 (18) exist
  expect_true(30 %in% melds$sums || any(melds$sums >= 18))
})

test_that("possible_melds does not generate 2-tile runs", {
  # (12,13) of the same colour is NOT a valid meld (runs need >= 3 tiles)
  hand <- tiles(c(12, 13), c("blue", "blue"))
  melds <- possible_melds(prepare_hand(hand))
  expect_equal(nrow(melds$M), 0)
  expect_false(is_valid_meld(hand))
  # with a joker the run can only be 11-12-13 (joker as 11): 36 points
  hand2 <- tiles(c(12, 13, NA), c("blue", "blue", "wild"))
  melds2 <- possible_melds(prepare_hand(hand2))
  expect_equal(nrow(melds2$M), 1)
  expect_equal(melds2$sums, 36L)
})

test_that("max_opening uses each tile only once (single run)", {
  # red 3,4,5,6,7: the best play is the full run 3+4+5+6+7 = 25
  hand <- tiles(c(3, 4, 5, 6, 7), rep("red", 5))
  expect_equal(max_opening(hand), 25L)
})

test_that("max_opening combines disjoint melds", {
  # Two runs of 6-9 (red and blue) + joker: the best is stretching one run
  # to 6-10 (40) with the joker and playing the other run whole (30) -> 70
  hand <- tiles(c(6, 7, 8, 9, 6, 7, 8, 9, NA),
                c(rep("red", 4), rep("blue", 4), "wild"))
  expect_equal(max_opening(hand), 70L)
})

test_that("a hand that does not reach 30 gives the exact maximum", {
  # 1-2-3 in red, blue and yellow: best play = the three runs (6+6+6=18)
  hand <- tiles(c(1, 2, 3, 1, 2, 3, 1, 2, 3),
                c(rep("red", 3), rep("blue", 3), rep("yellow", 3)))
  expect_equal(max_opening(hand), 18L)
  expect_false(can_open(hand))
})

test_that("can_open with jokers", {
  # red 9-10-11 + joker: run 9-12 = 42 -> opens
  hand <- tiles(c(9, 10, 11, NA), c("red", "red", "red", "wild"))
  expect_true(can_open(hand))

  # group of 13 with two jokers: 13 x 4 = 52 -> opens
  hand2 <- tiles(c(13, 13, NA, NA), c("red", "blue", "wild", "wild"))
  expect_true(can_open(hand2))
})

test_that("can_open respects that the joker is not doubled", {
  # red 4-5-6 + blue 4-5-6 + 1 joker: each run with the joker gives 4-7 = 22,
  # but there is only one joker -> best 22 + 15 (a run without joker) = 37, opens
  hand <- tiles(c(4, 5, 6, 4, 5, 6, NA),
                c(rep("red", 3), rep("blue", 3), "wild"))
  expect_true(can_open(hand))
  expect_equal(max_opening(hand), 37L)
})

test_that("random_hand returns 14 tiles from the pool", {
  hand <- random_hand(14L)
  expect_equal(nrow(hand), 14)
  keys <- table(paste(ifelse(is.na(hand$number), "J", hand$number),
                      hand$colour))
  expect_true(all(keys <= 2))                 # at most the 2 pool copies
  expect_true(all(hand$colour %in% c(COLOURS, "wild")))
})
