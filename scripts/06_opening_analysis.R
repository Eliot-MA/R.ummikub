# 06_opening_analysis.R
#
# Objective 6, analysis A: THE OPENING (rule of 30).
#
# Questions:
#   1) What is the probability of being able to open using ONLY the 14
#      initial tiles (one or more valid melds summing to >= 30)?
#   2) If the initial hand does not allow opening, how many drawing turns
#      are needed on average to open?
#
# Method: Monte Carlo with EXACT resolution of the opening.
#   - `prepare_hand()`      : hand -> types (counters per tile type).
#   - `possible_melds()`    : enumerates all melds (runs and groups, with
#                             jokers) that can be formed with that hand.
#   - `solve_opening()`     : maximum achievable score with disjoint melds
#                             (exact branch & bound). With `target = 30` it
#                             cuts off as soon as it is reached.
#   - `can_open()` / `max_opening()`: convenient wrappers.
#   - `random_hand()`       : 14 random tiles from the pool (no replacement).
#   - `simulate_draws_until_open()`: draw after draw until able to open.
#
# Results: saved in `results/` (CSV and PNG) and printed to the console.
# The execution block only runs when the script is launched directly
# (Rscript scripts/06_opening_analysis.R); when sourced from the tests the
# simulation is not run, only the functions are loaded.

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

source_local("01_valid_melds.R")

# --- Hand and counters ------------------------------------------------

# Random hand of k tiles from the pool (without replacement).
random_hand <- function(k = 14L) {
  pool <- tile_pool()
  rows <- sample.int(nrow(pool), k)
  pool[rows, , drop = FALSE]
}

# Hand -> counters per tile type (compact representation).
# Returns a list with:
#   - `types`: data.frame (number, colour, n, value) with one row per present
#     type; `value` = number of the tile (13 as an upper bound for jokers).
#   - `joker_idx`: index of the joker row (integer(0) if there is none).
prepare_hand <- function(hand) {
  stopifnot(is.data.frame(hand), nrow(hand) >= 1L)
  wild <- is.na(hand$number)
  key <- ifelse(wild, "JOKER", paste(hand$number, hand$colour, sep = "|"))
  freq <- table(key)
  keys <- names(freq)
  number <- vapply(keys, function(k) {
    if (k == "JOKER") NA_integer_ else as.integer(sub("\\|.*$", "", k))
  }, integer(1))
  colour <- vapply(keys, function(k) {
    if (k == "JOKER") "wild" else sub("^[^|]*\\|", "", k)
  }, character(1))
  types <- data.frame(
    number = number,
    colour = colour,
    n = as.integer(freq),
    value = ifelse(is.na(number), 13L, number),
    stringsAsFactors = FALSE
  )
  order <- order(ifelse(is.na(types$number), 999L, types$number),
                 match(types$colour, c(COLOURS, "wild")))
  types <- types[order, , drop = FALSE]
  rownames(types) <- NULL
  joker_idx <- which(is.na(types$number))
  list(types = types,
       joker_idx = if (length(joker_idx)) joker_idx else integer(0))
}

# --- Meld enumeration ------------------------------------------------

# All melds (runs and groups) formable with the hand, including the use
# of jokers. Returns a list with:
#   - `M`   : matrix (melds x types), how many tiles of each type it uses.
#   - `sums`: points of each meld (jokers with the value they represent).
possible_melds <- function(prep) {
  types <- prep$types
  m <- nrow(types)
  n_jokers <- if (length(prep$joker_idx)) types$n[prep$joker_idx] else 0L
  rows <- list()
  sums <- list()

  # Runs: window [lo, hi] of the same colour; the numbers missing inside
  # the window are covered by the jokers.
  for (cpos in seq_along(COLOURS)) {
    col <- COLOURS[cpos]
    ids <- which(!is.na(types$number) & types$colour == col)
    if (length(ids) == 0L) next
    nums <- types$number[ids]
    for (lo in 1:12) {
      for (hi in (lo + 2):13) {
        L <- hi - lo + 1L
        present <- unique(nums[nums >= lo & nums <= hi])
        if (length(present) == 0L) next
        missing <- L - length(present)
        if (missing > n_jokers) next
        count <- integer(m)
        for (n in present) count[ids[nums == n][1L]] <- 1L
        if (missing > 0L) count[prep$joker_idx] <- missing
        rows[[length(rows) + 1L]] <- count
        sums[[length(sums) + 1L]] <- as.integer(L * (lo + hi) / 2)
      }
    }
  }

  # Groups: same number, different colours; the colours missing up to
  # 3 or 4 tiles are covered by the jokers.
  for (n in 1:13) {
    ids <- which(!is.na(types$number) & types$number == n)
    if (length(ids) == 0L) next
    tile_colours <- types$colour[ids]
    u_colours <- unique(tile_colours)
    for (s in seq_along(u_colours)) {
      for (total in 3:4) {
        if (s > total) next
        j <- total - s
        if (j > n_jokers) next
        subs <- combn(u_colours, s, simplify = FALSE)
        for (cs in subs) {
          count <- integer(m)
          for (col in cs) count[ids[tile_colours == col][1L]] <- 1L
          if (j > 0L) count[prep$joker_idx] <- j
          rows[[length(rows) + 1L]] <- count
          sums[[length(sums) + 1L]] <- n * total
        }
      }
    }
  }

  if (length(rows) == 0L) {
    return(list(M = matrix(integer(0), nrow = 0, ncol = m), sums = integer(0)))
  }
  list(M = do.call(rbind, rows), sums = as.integer(unlist(sums)))
}

# --- Exact resolution ------------------------------------------------

# Maximum achievable score with disjoint melds of the hand (0 if none).
# `target`: with a finite value, the search cuts off as soon as it is
# reached (ideal for the rule of 30); with `Inf` it returns the exact
# maximum.
solve_opening <- function(hand, target = 30) {
  prep <- prepare_hand(hand)
  types <- prep$types
  m <- nrow(types)
  melds <- possible_melds(prep)
  M <- melds$M
  sums <- melds$sums
  if (nrow(M) == 0L) return(0L)

  # Cheap shortcuts for the rule of 30.
  if (is.finite(target)) {
    if (max(sums) >= target) return(as.integer(target))
    if (sum(types$n * types$value) < target) return(0L)
  }

  values <- types$value
  best <- 0L

  branch <- function(remaining, acc) {
    if (acc >= target) {
      if (acc > best) best <<- acc
      return(TRUE)
    }
    if (acc + sum(remaining * values) <= best) return(FALSE)

    # Usable melds (multiset included in what is left).
    cmp <- M <= rep(remaining, each = nrow(M))
    usable <- rowSums(cmp) == m
    if (!any(usable)) {
      if (acc > best) best <<- acc
      return(FALSE)
    }

    # Most constrained tile: the one covered by the fewest usable melds.
    cov <- colSums(M[usable, , drop = FALSE] > 0)
    cand <- which(remaining > 0 & cov > 0)
    if (length(cand) == 0L) {
      if (acc > best) best <<- acc
      return(FALSE)
    }
    tile_idx <- cand[which.min(cov[cand])]

    # Branch 1: do not use that tile in any meld.
    remaining2 <- remaining
    remaining2[tile_idx] <- 0L
    if (branch(remaining2, acc)) return(TRUE)

    # Branch 2: use it in each meld that contains it (best first).
    js <- which(usable & M[, tile_idx] > 0)
    js <- js[order(sums[js], decreasing = TRUE)]
    for (j in js) {
      if (branch(remaining - M[j, ], acc + sums[j])) return(TRUE)
    }
    FALSE
  }

  branch(types$n, 0L)
  as.integer(best)
}

# Can this hand open (rule of 30)?
can_open <- function(hand) {
  solve_opening(hand, target = 30) >= 30
}

# Exact maximum opening score (for the distribution).
max_opening <- function(hand) {
  solve_opening(hand, target = Inf)
}

# --- Draw simulation --------------------------------------------------

# Draws tiles from the pool until the hand can open; returns the number of
# draws needed (0 if it opens with the 14 initial ones).
simulate_draws_until_open <- function() {
  pool <- tile_pool()
  remaining <- seq_len(nrow(pool))
  idx <- sample(remaining, 14L)
  remaining <- setdiff(remaining, idx)
  hand <- pool[idx, , drop = FALSE]
  draws <- 0L
  while (!can_open(hand)) {
    if (length(remaining) == 0L) break
    ii <- sample(remaining, 1L)
    remaining <- setdiff(remaining, ii)
    hand <- rbind(hand, pool[ii, , drop = FALSE])
    draws <- draws + 1L
  }
  draws
}

# --- Execution block (only when the script is launched directly) --------

if (sys.nframe() == 0L) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The ggplot2 package is required to generate the plots")
  }
  dir.create("results", showWarnings = FALSE)

  set.seed(20260814)

  N_PROB <- 5000L   # hands for probability + maximum-score distribution
  N_DRAWS <- 5000L  # simulated games for the draws until opening

  # --- 1) Probability of opening with the 14 initial tiles ---------------
  cat("Simulating", N_PROB, "hands of 14 tiles...\n")
  pb <- utils::txtProgressBar(min = 0, max = N_PROB, style = 3)
  opens <- logical(N_PROB)
  scores <- integer(N_PROB)
  for (s in seq_len(N_PROB)) {
    hand <- random_hand(14L)
    opens[s] <- can_open(hand)
    scores[s] <- max_opening(hand)
    utils::setTxtProgressBar(pb, s)
  }
  close(pb)

  n_open <- sum(opens)
  p_open <- n_open / N_PROB
  ci <- qbeta(c(0.025, 0.975), n_open + 1, N_PROB - n_open + 1)

  # --- 2) Draws needed until able to open ---------------------------------
  cat("Simulating", N_DRAWS, "openings with drawing...\n")
  pb <- utils::txtProgressBar(min = 0, max = N_DRAWS, style = 3)
  draws <- integer(N_DRAWS)
  for (s in seq_len(N_DRAWS)) {
    draws[s] <- simulate_draws_until_open()
    utils::setTxtProgressBar(pb, s)
  }
  close(pb)

  # --- Console summary -----------------------------------------------------
  cat("\n============================================================\n")
  cat("ANALYSIS A: THE OPENING (rule of 30)\n")
  cat("============================================================\n")
  cat(sprintf("1) Hands that open with the 14 initial tiles: %.1f%% (95%% CI: %.1f%%-%.1f%%)\n",
              p_open * 100, ci[1] * 100, ci[2] * 100))
  qs <- quantile(scores, probs = c(0.25, 0.50, 0.75, 0.90))
  cat(sprintf("   Maximum opening score: mean %.1f, median %d (Q1 %d, Q3 %d, P90 %d)\n",
              mean(scores), qs[2], qs[1], qs[3], qs[4]))
  mean_draws <- mean(draws)
  cat(sprintf("2) Mean draws until opening: %.2f (median %d, P90 %d, max %d)\n",
              mean_draws, median(draws), quantile(draws, 0.90), max(draws)))
  cat(sprintf("   Cross-check with 1): %.1f%% of the games open without drawing\n",
              mean(draws == 0) * 100))
  cat("\n")

  # --- Save resources in results/ ---------------------------------------
  write.csv(data.frame(p_open = p_open, ci_low = ci[1], ci_high = ci[2],
                       n = N_PROB),
            "results/opening_probability.csv", row.names = FALSE)
  write.csv(data.frame(max_score = scores),
            "results/opening_max_score.csv", row.names = FALSE)
  write.csv(data.frame(draws = draws),
            "results/opening_draws.csv", row.names = FALSE)
  draws_table <- as.data.frame(table(draws))
  names(draws_table) <- c("draws", "n")
  draws_table$proportion <- draws_table$n / N_DRAWS
  write.csv(draws_table, "results/opening_draws_distribution.csv",
            row.names = FALSE)
  write.csv(data.frame(mean = mean_draws,
                       median = as.numeric(median(draws)),
                       p90 = as.numeric(quantile(draws, 0.90)),
                       max = max(draws),
                       p_open_without_drawing = mean(draws == 0)),
            "results/opening_draws_summary.csv", row.names = FALSE)

  # --- Plots ----------------------------------------------------------------
  df_scores <- data.frame(score = scores)

  g1 <- ggplot2::ggplot(df_scores, ggplot2::aes(x = score)) +
    ggplot2::geom_histogram(binwidth = 3, fill = "#3b6fd4", colour = "white") +
    ggplot2::geom_vline(xintercept = 30, linetype = "dashed",
                        colour = "#d64545", linewidth = 0.8) +
    ggplot2::annotate("text", x = 33, y = Inf, hjust = 0, vjust = 1.5,
                      label = "rule of 30",
                      colour = "#d64545", size = 3.5) +
    ggplot2::labs(title = "Maximum opening score (14 tiles)",
                  subtitle = sprintf("%.1f%% of hands reach over 30 points",
                                     p_open * 100),
                  x = "Maximum achievable score", y = "Number of hands") +
    ggplot2::theme_minimal()
  ggplot2::ggsave("results/opening_score_histogram.png", g1,
                  width = 7, height = 4.5, dpi = 300)

  # Cumulative probability of having opened after t draws.
  max_draws <- max(draws)
  cum_prob <- sapply(0:max_draws, function(t) mean(draws <= t))
  df_draws <- data.frame(draws = 0:max_draws, cum_prob = cum_prob)

  g2 <- ggplot2::ggplot(df_draws, ggplot2::aes(x = draws, y = cum_prob)) +
    ggplot2::geom_line(colour = "#3b6fd4", linewidth = 1) +
    ggplot2::geom_point(colour = "#3b6fd4", size = 1.5) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(title = "Probability of being able to open after t draws",
                  subtitle = sprintf("mean: %.2f draws; %d%% open without drawing",
                                     mean_draws, round(mean(draws == 0) * 100)),
                  x = "Draws", y = "P(open with <= t draws)") +
    ggplot2::theme_minimal()
  ggplot2::ggsave("results/opening_cumulative_probability.png", g2,
                  width = 7, height = 4.5, dpi = 300)

}
