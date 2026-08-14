# tests/test_objective_01.R
#
# Tests for objective 1 (valid melds and the rule of 30).
# Run with:
#   Rscript -e "testthat::test_file('tests/test_objective_01.R')"
#   (or `library(testthat); test_file("tests/test_objective_01.R")` in RStudio)

source(file.path(dirname(testthat::test_path()), "..", "scripts",
                 "01_valid_melds.R"))

library(testthat)

test_that("is_run detects valid runs", {
  run <- tiles(c(3, 4, 5), c("blue", "blue", "blue"))
  expect_true(is_run(run))
})

test_that("is_run rejects invalid runs", {
  expect_false(is_run(tiles(c(3, 4), c("blue", "blue"))))             # 2 tiles
  expect_false(is_run(tiles(c(3, 4, 4), c("blue", "blue", "blue"))))  # repeated
  expect_false(is_run(tiles(c(3, 4, 5), c("blue", "blue", "red"))))   # colours
  expect_false(is_run(tiles(c(3, 5, 7), c("blue", "blue", "blue"))))  # gaps
  expect_false(is_run(tiles(c(NA, NA), c("wild", "wild"))))           # only jokers
})

test_that("is_group detects valid groups", {
  grp <- tiles(c(7, 7, 7), c("red", "blue", "black"))
  expect_true(is_group(grp))
  grp4 <- tiles(c(8, 8, 8, 8), COLOURS)
  expect_true(is_group(grp4))
})

test_that("is_group rejects invalid groups", {
  expect_false(is_group(tiles(c(7, 7), c("red", "blue"))))           # 2 tiles
  expect_false(is_group(tiles(c(7, 7, 7, 7, 7), c("red", "blue", "yellow", "black", "red")))) # 5 tiles
  expect_false(is_group(tiles(c(7, 8, 7), c("red", "blue", "yellow")))) # numbers
  expect_false(is_group(tiles(c(7, 7, 7), c("red", "red", "blue"))))    # repeated colour
  expect_false(is_group(tiles(c(NA, NA), c("wild", "wild"))))           # only jokers
})

test_that("jokers in runs", {
  run <- tiles(c(3, NA, 5), c("blue", "wild", "blue"))
  expect_true(is_run(run))
  expect_equal(sum(assign_run(run)$number), 12)   # 3,4,5

  run_end <- tiles(c(1, 2, 3, NA), c("blue", "blue", "blue", "wild"))
  expect_true(is_run(run_end))
  expect_equal(sum(assign_run(run_end)$number), 10)  # 1,2,3,4

  run_only <- tiles(c(5, NA, NA), c("blue", "wild", "wild"))
  expect_true(is_run(run_only))                      # 5,6,7 canonical
  expect_equal(sum(assign_run(run_only)$number), 18)
})

test_that("jokers in groups", {
  grp <- tiles(c(7, 7, NA), c("red", "blue", "wild"))
  expect_true(is_group(grp))
  expect_equal(sum(assign_group(grp)$number), 21)

  grp2 <- tiles(c(13, 13, NA, NA), c("red", "blue", "wild", "wild"))
  expect_true(is_group(grp2))
  expect_equal(sum(assign_group(grp2)$number), 52)
})

test_that("is_valid_meld combines run and group", {
  expect_true(is_valid_meld(tiles(c(9, 10, 11), c("red", "red", "red"))))
  expect_true(is_valid_meld(tiles(c(5, 5, 5), c("red", "blue", "yellow"))))
  expect_false(is_valid_meld(tiles(c(5, 5), c("red", "blue"))))
  expect_false(is_valid_meld(tiles(c(2, 4, 6), c("red", "red", "red"))))
})

test_that("meld_score scores with the canonical value", {
  expect_equal(meld_score(tiles(c(9, 10, 11), c("red", "red", "red"))), 30)
  expect_equal(meld_score(tiles(c(5, 5, 5), c("red", "blue", "yellow"))), 15)
})

test_that("meets_rule_30", {
  expect_true(meets_rule_30(tiles(c(9, 10, 11), c("red", "red", "red"))))   # 30
  expect_false(meets_rule_30(tiles(c(1, 2, 3), c("red", "red", "red"))))   # 6

  meld_a <- tiles(c(8, 9, 10), c("blue", "blue", "blue"))    # 27
  meld_b <- tiles(c(4, 4, 4), c("red", "blue", "yellow"))    # 12
  expect_true(meets_rule_30(list(meld_a, meld_b)))          # 27 + 12 = 39
})
