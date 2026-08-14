# tests/test_objective_05.R
#
# Tests for objective 5 (board visualization).
# Run with:
#   Rscript -e "testthat::test_file('tests/test_objective_05.R')"

source(file.path(dirname(testthat::test_path()), "..", "scripts",
                 "05_board_visualization.R"))



library(testthat)

test_that("board_to_data orders runs by number", {
  board <- list(tiles(c(5, 3, 4), c("red", "red", "red")))
  data <- board_to_data(board)
  expect_equal(data$number, 3:5)
  expect_equal(data$position, 1:3)
  expect_equal(data$meld, rep(1, 3))
  expect_equal(data$label, c("3", "4", "5"))
})

test_that("board_to_data orders groups by colour and labels jokers", {
  board <- list(tiles(c(7, 7, 7), c("yellow", "red", "blue")))
  data <- board_to_data(board)
  expect_equal(data$colour, c("red", "blue", "yellow"))

  board_j <- list(tiles(c(3, NA, 5), c("blue", "wild", "blue")))
  data_j <- board_to_data(board_j)
  expect_true(any(is.na(data_j$number)))
  expect_equal(data_j$label[is.na(data_j$number)], "J")
})

test_that("board_to_data accepts a data.frame with meld_id", {
  board_df <- data.frame(
    meld_id = c(1, 1, 1, 2, 2),
    number = c(8, 9, 10, 4, 4),
    colour = c("red", "red", "red", "blue", "yellow")
  )
  data <- board_to_data(board_df)
  expect_equal(nrow(data), 5)
  expect_equal(sort(unique(data$meld)), 1:2)
})

test_that("empty board", {
  expect_equal(nrow(board_to_data(list())), 0)
})

test_that("show_board prints each meld", {
  board <- list(
    tiles(c(3, 4, 5), c("red", "red", "red")),
    tiles(c(7, 7, 7), c("blue", "yellow", "black"))
  )
  output <- capture.output(show_board(board))
  expect_true(any(grepl("Meld 1 \\(run\\)", output)))
  expect_true(any(grepl("Meld 2 \\(group\\)", output)))
  expect_true(any(grepl("5 red", output)))
})

test_that("show_board with an empty board", {
  expect_output(show_board(list()), "Empty board")
})

test_that("plot_board returns a ggplot", {
  board <- list(
    tiles(c(3, 4, 5), c("red", "red", "red")),
    tiles(c(7, 7, NA), c("blue", "yellow", "wild"))
  )
  p <- plot_board(board)
  expect_s3_class(p, "ggplot")
})

test_that("plot_board with an empty board warns and returns NULL", {
  expect_warning(plot_board(list()), "empty")
  expect_null(suppressWarnings(plot_board(list())))
})
