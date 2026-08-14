# 05_board_visualization.R
#
# Objective 5: representation of the board state.
#
#   - `board_to_data()`: data.frame ready to view/plot, with one row
#     per tile: meld, position (order within the meld), number, colour and
#     label (number, or "J" for joker). Runs are ordered by number; groups
#     by colour.
#   - `plot_board()`: ggplot2 plot, one cell per tile coloured by colour and
#     with the label on top. Returns the ggplot object.
#   - `show_board()`: readable console output.
#
# Board representation: list of melds or data.frame with `meld_id`
# (the same as in 03/04).

# Loading of shared scripts (search in scripts/ upwards from the wd).
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
source_local("01_valid_melds.R")
source_local("03_board_verifier.R")

# --- Ordering and data ----------------------------------------------

# Orders the tiles of a meld: runs by number (jokers last), groups
# by colour order.
order_meld <- function(meld) {
  if (is_run(meld)) {
    meld[order(is.na(meld$number), meld$number), , drop = FALSE]
  } else if (is_group(meld)) {
    meld[order(match(meld$colour, COLOURS)), , drop = FALSE]
  } else {
    meld
  }
}

# Board as a data.frame with one row per tile.
board_to_data <- function(board) {
  melds <- board_to_list(board)
  if (length(melds) == 0) {
    return(data.frame(meld = integer(0), position = integer(0),
                      number = integer(0), colour = character(0),
                      label = character(0)))
  }
  rows <- lapply(seq_along(melds), function(m) {
    meld <- order_meld(melds[[m]])
    data.frame(
      meld = m,
      position = seq_len(nrow(meld)),
      number = meld$number,
      colour = meld$colour,
      label = ifelse(is.na(meld$number), "J", as.character(meld$number)),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# --- Console output and plot -----------------------------------------

show_board <- function(board) {
  melds <- board_to_list(board)
  if (length(melds) == 0) {
    cat("Empty board\n")
    return(invisible(NULL))
  }
  for (m in seq_along(melds)) {
    meld <- order_meld(melds[[m]])
    type <- if (is_run(meld)) "run" else if (is_group(meld)) "group" else "?"
    labels <- ifelse(is.na(meld$number), "J", as.character(meld$number))
    cat(sprintf("Meld %d (%s): %s\n", m, type,
                paste(paste(labels, meld$colour), collapse = " | ")))
  }
  invisible(NULL)
}

plot_board <- function(board, title = "Rummikub board") {
  data <- board_to_data(board)
  if (nrow(data) == 0) {
    warning("The board is empty: nothing to draw")
    return(invisible(NULL))
  }
  palette <- c(red = "#d64545", blue = "#3b6fd4", yellow = "#f2c94c",
               black = "#3a3a3a", wild = "#9e9e9e")
  text_colour <- c(red = "black", blue = "white", yellow = "black",
                   black = "white", wild = "white")
  data$fill <- palette[data$colour]
  data$text_colour <- text_colour[data$colour]

  ggplot2::ggplot(data, ggplot2::aes(x = position, y = factor(meld))) +
    ggplot2::geom_tile(fill = data$fill, colour = "grey25",
                       width = 0.92, height = 0.92) +
    ggplot2::geom_text(ggplot2::aes(label = label, colour = text_colour),
                       size = 5) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_y_discrete(limits = rev(sort(as.character(unique(data$meld))))) +
    ggplot2::coord_fixed(ratio = 1, xlim = c(0.5, max(data$position) + 0.5)) +
    ggplot2::labs(title = title, x = "Position", y = "Meld") +
    ggplot2::theme_minimal()
}
