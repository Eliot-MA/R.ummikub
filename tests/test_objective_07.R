# tests/test_objective_07.R
#
# Tests for objective 6, analysis B (runs vs groups):
#   - coverage branch & bound (disjoint melds maximizing covered tiles)
#   - per-hand tallies of the optimal play and the Monte Carlo aggregation
# Run with:
#   Rscript -e "testthat::test_file('tests/test_objective_07.R')"

source(file.path(dirname(testthat::test_path()), "..", "scripts",
                 "07_runs_vs_groups.R"))

library(testthat)

test_that("coverage_solver plays the full run, not its sub-runs", {
  # 1-2-3-4 red: candidates include 1-2-3, 2-3-4 and 1-2-3-4, but only one
  # disjoint meld fits, so the best play is the full run (4 tiles)
  prep <- prepare_hand(tiles(c(1, 2, 3, 4), rep("red", 4)))
  melds <- possible_melds(prep)
  sol <- coverage_solver(prep, melds)
  expect_equal(sol$covered, 4L)
  expect_equal(length(sol$idx), 1L)
  expect_equal(melds$type[sol$idx], "run")
})

test_that("coverage_solver enforces disjointness", {
  # three copies of 4-5-6 (one per colour): 9 tiles, best play uses all 9
  prep <- prepare_hand(tiles(rep(c(4, 5, 6), 3),
                             rep(c("red", "blue", "yellow"), each = 3)))
  melds <- possible_melds(prep)
  sol <- coverage_solver(prep, melds)
  expect_equal(sol$covered, 9L)
  # the chosen melds share no tile type at all
  M <- melds$M[sol$idx, , drop = FALSE]
  if (nrow(M) > 1L) {
    for (a in 1:(nrow(M) - 1L)) {
      for (b in (a + 1L):nrow(M)) {
        expect_true(all((M[a, ] * M[b, ]) == 0L))
      }
    }
  }
})

test_that("coverage_solver splits a run when a group covers more", {
  # 3-4-5-6 red + 3 blue + 3 black: playing run 4-5-6 AND the group of 3s
  # covers 6 tiles, more than the run 3-4-5-6 alone (4 tiles)
  prep <- prepare_hand(tiles(c(3, 4, 5, 6, 3, 3),
                             c("red", "red", "red", "red", "blue", "black")))
  melds <- possible_melds(prep)
  sol <- coverage_solver(prep, melds)
  expect_equal(sol$covered, 6L)
  expect_setequal(melds$type[sol$idx], c("run", "group"))
})

test_that("solve_opening_coverage wraps the solver", {
  hand <- tiles(c(3, 4, 5, 6, 3, 3),
                c("red", "red", "red", "red", "blue", "black"))
  expect_equal(solve_opening_coverage(hand), 6L)
  expect_equal(solve_opening_coverage(tiles(c(4, 5, 6), rep("red", 3))), 3L)
  # no meld formable -> 0
  expect_equal(solve_opening_coverage(tiles(c(1, 2, 2, 13),
                                            c("red", "blue", "yellow", "black"))),
               0L)
})

test_that("overlapping candidates never overcount tiles", {
  # 4-5-6 red + 6 yellow + 6 black: the run and the group overlap on the
  # red 6, so at most 3 tiles can be covered (never 6)
  prep <- prepare_hand(tiles(c(4, 5, 6, 6, 6),
                             c("red", "red", "red", "yellow", "black")))
  t <- tally_hand_melds(prep)
  expect_equal(t$covered, 3L)
  expect_equal(t$n_runs + t$n_groups, 1L)
})

test_that("tally_hand_melds counts the optimal play", {
  # run 4-5-6 + group of 3s (6 tiles); the group uses number 3, the run
  # uses numbers 4, 5 and 6
  prep <- prepare_hand(tiles(c(3, 4, 5, 6, 3, 3),
                             c("red", "red", "red", "red", "blue", "black")))
  t <- tally_hand_melds(prep)
  expect_equal(t$covered, 6L)
  expect_equal(t$n_runs, 1L)
  expect_equal(t$n_groups, 1L)
  expect_equal(t$heat_run, c(0, 0, 0, 1, 1, 1, rep(0, 7)))
  expect_equal(t$heat_group, c(0, 0, 1, rep(0, 10)))
})

test_that("a 14-tile hand never plays more than floor(14/3) melds", {
  res <- tally_runs_groups(50L, seed = 1)
  expect_true(all(res$per_hand$n_runs + res$per_hand$n_groups <= 4L))
  expect_true(all(res$per_hand$covered <= 14L))
})

test_that("tally_runs_groups is deterministic and consistent", {
  res <- tally_runs_groups(10L, seed = 42)
  expect_equal(res$n_hands, 10L)
  expect_equal(nrow(res$per_hand), 10L)
  expect_true(all(res$per_hand$n_runs >= 0L))
  expect_true(all(res$per_hand$n_groups >= 0L))
  expect_true(all(res$per_hand$covered >= 0L))

  # re-enumerating the same hands gives the same per-hand and heat counts
  set.seed(42)
  total_runs <- 0L
  total_groups <- 0L
  total_covered <- 0L
  heat_run <- integer(13)
  heat_group <- integer(13)
  for (s in 1:10) {
    t <- tally_hand_melds(prepare_hand(random_hand(14L)))
    total_runs <- total_runs + t$n_runs
    total_groups <- total_groups + t$n_groups
    total_covered <- total_covered + t$covered
    heat_run <- heat_run + t$heat_run
    heat_group <- heat_group + t$heat_group
  }
  expect_equal(sum(res$per_hand$n_runs), total_runs)
  expect_equal(sum(res$per_hand$n_groups), total_groups)
  expect_equal(sum(res$per_hand$covered), total_covered)
  expect_equal(res$heat$run, heat_run)
  expect_equal(res$heat$group, heat_group)
})
