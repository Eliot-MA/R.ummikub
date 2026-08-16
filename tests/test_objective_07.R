# tests/test_objective_07.R
#
# Tests for objective 6, analysis B (runs vs groups):
#   - meld-type tagging in possible_melds() (runs / groups)
#   - per-hand tallies and the Monte Carlo aggregation
# Run with:
#   Rscript -e "testthat::test_file('tests/test_objective_07.R')"

source(file.path(dirname(testthat::test_path()), "..", "scripts",
                 "07_runs_vs_groups.R"))

library(testthat)

test_that("possible_melds tags pure runs and pure groups", {
  run_hand <- prepare_hand(tiles(c(4, 5, 6), rep("red", 3)))
  melds_run <- possible_melds(run_hand)
  expect_gt(nrow(melds_run$M), 0)
  expect_true(all(melds_run$type == "run"))

  group_hand <- prepare_hand(tiles(c(6, 6, 6), c("red", "blue", "yellow")))
  melds_group <- possible_melds(group_hand)
  expect_gt(nrow(melds_group$M), 0)
  expect_true(all(melds_group$type == "group"))
})

test_that("a mixed hand yields runs and groups with the right points", {
  prep <- prepare_hand(tiles(c(4, 5, 6, 6, 6),
                             c("red", "red", "red", "yellow", "black")))
  melds <- possible_melds(prep)
  expect_setequal(melds$type, c("run", "group"))
  # run 4-5-6 (15) and group of three 6s (18)
  expect_setequal(melds$sums, c(15L, 18L))
  expect_equal(melds$type[melds$sums == 15L], "run")
  expect_equal(melds$type[melds$sums == 18L], "group")
})

test_that("a joker meld is tagged although it could be either type", {
  # One real tile (5 blue) plus two jokers can form both runs and groups.
  prep <- prepare_hand(tiles(c(5, NA, NA), c("blue", "wild", "wild")))
  melds <- possible_melds(prep)
  expect_true(any(melds$type == "run"))
  expect_true(any(melds$type == "group"))
  expect_equal(sum(melds$type == "run"), 3)   # 3-4-5, 4-5-6, 5-6-7
  expect_equal(sum(melds$type == "group"), 1) # 5 + 5 + 5
  expect_setequal(melds$sums[melds$type == "run"], c(12L, 15L, 18L))
  expect_equal(melds$sums[melds$type == "group"], 15L)
})

test_that("tally_hand_melds counts types and tile-number usage", {
  prep <- prepare_hand(tiles(c(4, 5, 6, 6, 6),
                             c("red", "red", "red", "yellow", "black")))
  t <- tally_hand_melds(prep)
  expect_equal(t$n_runs, 1L)
  expect_equal(t$n_groups, 1L)
  # the run 4-5-6 uses numbers 4, 5, 6; the group of 6s uses number 6
  expect_equal(t$heat_run, c(0, 0, 0, 1, 1, 1, rep(0, 7)))
  expect_equal(t$heat_group, c(0, 0, 0, 0, 0, 1, rep(0, 7)))
})

test_that("tally_runs_groups is deterministic and consistent", {
  res <- tally_runs_groups(10L, seed = 42)
  expect_equal(res$n_hands, 10L)
  expect_equal(nrow(res$per_hand), 10L)
  expect_true(all(res$per_hand$n_runs >= 0L))
  expect_true(all(res$per_hand$n_groups >= 0L))

  # re-enumerating the same hands gives the same per-hand and heat counts
  set.seed(42)
  total_runs <- 0L
  total_groups <- 0L
  heat_run <- integer(13)
  heat_group <- integer(13)
  for (s in 1:10) {
    t <- tally_hand_melds(prepare_hand(random_hand(14L)))
    total_runs <- total_runs + t$n_runs
    total_groups <- total_groups + t$n_groups
    heat_run <- heat_run + t$heat_run
    heat_group <- heat_group + t$heat_group
  }
  expect_equal(sum(res$per_hand$n_runs), total_runs)
  expect_equal(sum(res$per_hand$n_groups), total_groups)
  expect_equal(res$heat$run, heat_run)
  expect_equal(res$heat$group, heat_group)
})
