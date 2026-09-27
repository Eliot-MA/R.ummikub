# tests/test_objective_08.R
#
# Tests for objective 7, phase 1, the exact value function V(H):
#   - the exact leftover group coverage, where the obvious greedy fails
#   - the dynamic program of scripts/08_play_or_draw.R on hands worked out
#     by hand, including runs longer than 3 and the runs/groups trade-off
#   - agreement with the branch & bound of scripts/06_opening_analysis.R,
#     which is the independent oracle
#   - the coverage variant against coverage_solver()
#   - the deliberate limits of phase 1: no jokers, no richer-than-pool hands
# Run with:
#   Rscript -e "testthat::test_file('tests/test_objective_08.R')"

source(file.path(dirname(testthat::test_path()), "..", "scripts",
                 "08_play_or_draw.R"))

library(testthat)

# --- the exact helper ----------------------------------------------------

test_that("total_group_size covers more than the take-four-first greedy", {
  # Two groups of three cover all six; the greedy takes one group of four
  # and stops. Regression test for decision D3 in the design document.
  expect_equal(total_group_size(c(2L, 2L, 1L, 1L)), 6L)
  expect_equal(total_group_size(c(2L, 2L, 2L, 2L)), 8L)
  expect_equal(total_group_size(c(2L, 2L, 1L, 0L)), 3L)
  expect_equal(total_group_size(c(1L, 1L, 1L, 0L)), 3L)
  expect_equal(total_group_size(c(1L, 1L, 0L, 0L)), 0L)
})

test_that("total_group_size is exact on all 81 leftover vectors", {
  greedy <- function(leftover) {
    if (sum(leftover) < 3L) return(0L)
    take <- min(4L, sum(leftover > 0L))
    if (take < 3L) return(0L)
    rest <- leftover
    for (ci in seq_along(rest)) if (rest[ci] > 0L) rest[ci] <- rest[ci] - 1L
    greedy(rest) + take
  }
  worse <- 0L
  for (a in 0:2) for (b in 0:2) for (c3 in 0:2) for (d in 0:2) {
    lf <- c(a, b, c3, d)
    expect_gte(total_group_size(lf), greedy(lf))
    if (total_group_size(lf) < greedy(lf)) worse <- worse + 1L
  }
  expect_equal(worse, 0L)
})

# --- the value function on hands worked out by hand ----------------------

test_that("best_meld_value scores a run of three", {
  expect_equal(best_meld_value(tiles(c(3, 4, 5), rep("blue", 3))), 12)
  expect_equal(best_meld_value(tiles(c(1, 2, 3), rep("red", 3))), 6)
})

test_that("best_meld_value scores a run longer than three", {
  # 3-4-5-6-7 is one legal run of five, not 3-4-5 plus 6-7. The program
  # reaches length 3 at value 5 for 12 and then adds 6 and 7.
  expect_equal(best_meld_value(tiles(c(3, 4, 5, 6, 7), rep("red", 5))), 25)
  expect_equal(best_meld_value(tiles(c(3, 4, 5, 6), rep("blue", 4))), 18)
})

test_that("best_meld_value scores groups of three and four", {
  expect_equal(best_meld_value(tiles(rep(7, 4),
                                     c("red", "blue", "yellow", "black"))), 28L)
  expect_equal(best_meld_value(tiles(rep(7, 3),
                                     c("red", "blue", "black"))), 21L)
  # five 7s: one group of four, the fifth tile is unusable
  expect_equal(best_meld_value(tiles(c(7, 7, 7, 7, 7),
                                     c("red", "red", "blue", "blue", "black"))), 21L)
})

test_that("best_meld_value combines disjoint runs and groups", {
  # a run 2-3-4 red (9) and a group of three 1s (3)
  expect_equal(best_meld_value(tiles(c(1, 1, 1, 2, 3, 4),
                                     c("red", "blue", "yellow",
                                       "red", "red", "red"))), 12)
  # a run 3-4-5 blue (12) and a group of three 9s (27)
  expect_equal(best_meld_value(tiles(c(3, 4, 5, 9, 9, 9),
                                     c("blue", "blue", "blue",
                                       "red", "blue", "black"))), 39)
})

test_that("best_meld_value handles two runs of one colour sharing tiles", {
  # 3,4,5 then 4,5,6,7,8,9 of one colour: the two runs cannot share a tile,
  # so the best is 3-4-5 (12) plus 4-5-6-7-8-9 (39) = 51.
  expect_equal(best_meld_value(tiles(c(3, 4, 5, 4, 5, 6, 7, 8, 9),
                                     rep("blue", 9))), 51)
})

test_that("best_meld_value returns 0 when no meld exists", {
  expect_equal(best_meld_value(tiles(integer(0), character(0))), 0)
  expect_equal(best_meld_value(tiles(5, "red")), 0)
  expect_equal(best_meld_value(tiles(c(5, 6), c("red", "red"))), 0)
  expect_equal(best_meld_value(tiles(c(1, 2), c("red", "blue"))), 0)
})

# --- agreement with the independent oracle -------------------------------

test_that("best_meld_value agrees with the branch & bound on small hands", {
  set.seed(2718281)
  for (kk in c(4L, 6L, 8L)) {
    for (i in 1:15) {
      hand <- random_real_hand(kk)
      expect_equal(as.integer(best_meld_value(hand)),
                   as.integer(max_opening(hand)),
                   info = paste("hand size", kk, "repetition", i))
    }
  }
})

test_that("best_meld_coverage agrees with coverage_solver", {
  set.seed(3141592)
  for (i in 1:8) {
    hand <- random_real_hand(10L)
    expect_equal(as.integer(best_meld_coverage(hand)),
                 as.integer(solve_opening_coverage(hand)),
                 info = paste("repetition", i))
  }
  # 6 + 6 across four colours: both groups fit, so all 12 tiles are covered
  hand <- tiles(c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2),
                c("red", "blue", "yellow", "black", "red", "blue",
                  "red", "blue", "yellow", "black", "red", "blue"))
  expect_equal(as.integer(best_meld_coverage(hand)), 12L)
  expect_equal(as.integer(solve_opening_coverage(hand)), 12L)
})

test_that("best_meld_coverage maximizes tiles, not value", {
  # A group of three 1s covers 3 tiles worth 3; a run 5-6-7 covers 3 tiles
  # worth 18. The coverage variant must prefer the first.
  hand <- tiles(c(1, 1, 1, 5, 6, 7),
                c("red", "blue", "yellow", "red", "red", "red"))
  expect_equal(best_meld_value(hand), 21)
  expect_equal(best_meld_coverage(hand), 6L)
})

# --- the deliberate limits of phase 1 ------------------------------------

test_that("best_meld_value rejects jokers instead of guessing", {
  hand <- tiles(c(5, 5, 5, 7), c("red", "blue", "yellow", "black"))
  hand$number[4] <- NA
  expect_error(best_meld_value(hand), "joker")
  expect_error(best_meld_value(hand), "phase 1b")
})

test_that("best_meld_value rejects hands richer than the pool", {
  expect_error(best_meld_value(tiles(rep(1, 5), rep("red", 5))),
               "copies")
  expect_error(best_meld_value(tiles(rep(9, 3), rep("red", 3))),
               "copies")
})

test_that("best_meld_value is deterministic and leaks no state", {
  set.seed(1618033)
  h1 <- tiles(c(3, 4, 5), rep("blue", 3))
  h2 <- tiles(c(9, 10, 11, 12, 1, 2),
              c("red", "red", "red", "red", "blue", "yellow"))
  expect_equal(best_meld_value(h1), 12)
  expect_equal(best_meld_value(h2), 42)
  # Interleaving calls must not change any answer: the memo is shared, so a
  # stale entry would show up here.
  expect_equal(best_meld_value(h1), 12)
  expect_equal(best_meld_value(h2), 42)
  hand <- random_real_hand(8L)
  expect_identical(best_meld_value(hand), best_meld_value(hand))
  expect_identical(best_meld_coverage(hand), best_meld_coverage(hand))
})

# --- the value function scales the way the paper describes ---------------

test_that("the run state is the product over colours, not a single vector", {
  # 10 run states per colour with two copies, and the joint state is their
  # product. The paper's O(n) bound counts one colour's state, so it
  # understates the size. See decision D6 in the design document.
  states <- run_states(2L)
  expect_equal(length(states), 10L)
  tb <- value_tables(2L, identity)
  expect_equal(tb$n1, 10L)
  expect_equal(tb$n_combo, 10000L)
  # four colours, four copies at most per colour, 3^4 leftover patterns
  expect_equal(length(tb$gf), 81L)
})
