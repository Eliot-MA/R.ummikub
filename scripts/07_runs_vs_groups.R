# 07_runs_vs_groups.R
#
# Objective 6, analysis B: RUNS vs GROUPS.
#
# Questions:
#   1) What is the relative frequency of runs and groups among the melds
#      formable in random hands of 14 tiles?
#   2) Which tile numbers take part in the most melds, and of which type?
#      (heatmap of melds by tile number)
#
# Method: Monte Carlo with EXACT enumeration of the melds.
#   - `tally_hand_melds()`   : for one hand, counts its formable melds by
#                             type (run / group) and, per tile number, how
#                             many melds of each type use it.
#   - `tally_runs_groups()`  : runs the simulation over `n_hands` hands and
#                             aggregates the tallies.
#
# The meld type is taken from `possible_melds()` (which tags each meld as
# "run" or "group"); it cannot be re-derived from the tile matrix alone,
# because a meld made of a single real tile plus jokers could be either.
#
# Results: saved in `results/` (CSV and PNG) and printed to the console.
# The execution block only runs when the script is launched directly
# (Rscript scripts/07_runs_vs_groups.R); when sourced from the tests the
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

source_local("06_opening_analysis.R")

# --- Per-hand tally ---------------------------------------------------

# Tallies the formable melds of a single prepared hand. Returns a list with:
#   - `n_runs`   : number of run melds.
#   - `n_groups` : number of group melds.
#   - `heat_run`   : integer(13): number of run melds using each tile number.
#   - `heat_group` : integer(13): number of group melds using each number.
tally_hand_melds <- function(prep) {
  types <- prep$types
  melds <- possible_melds(prep)
  if (nrow(melds$M) == 0L) {
    return(list(n_runs = 0L, n_groups = 0L,
                heat_run = integer(13), heat_group = integer(13)))
  }
  M <- melds$M
  is_run <- melds$type == "run"
  is_group <- melds$type == "group"
  heat_run <- integer(13)
  heat_group <- integer(13)
  for (n in 1:13) {
    idx <- which(!is.na(types$number) & types$number == n)
    if (length(idx) == 0L) next
    uses <- rowSums(M[, idx, drop = FALSE] > 0L) > 0L
    heat_run[n] <- sum(uses & is_run)
    heat_group[n] <- sum(uses & is_group)
  }
  list(n_runs = sum(is_run), n_groups = sum(is_group),
       heat_run = heat_run, heat_group = heat_group)
}

# --- Monte Carlo simulation -------------------------------------------

# Runs `n_hands` random hands of `k` tiles and aggregates the tallies.
# Returns a list with:
#   - `per_hand`: data.frame (n_runs, n_groups), one row per hand.
#   - `heat`    : data.frame (number, run, group) with the TOTAL number of
#                 melds of each type that use each tile number (1..13).
#   - `n_hands`, `k`: parameters used.
tally_runs_groups <- function(n_hands = 10000L, k = 14L, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  n_runs <- integer(n_hands)
  n_groups <- integer(n_hands)
  heat_run <- integer(13)
  heat_group <- integer(13)
  for (s in seq_len(n_hands)) {
    prep <- prepare_hand(random_hand(k))
    t <- tally_hand_melds(prep)
    n_runs[s] <- t$n_runs
    n_groups[s] <- t$n_groups
    heat_run <- heat_run + t$heat_run
    heat_group <- heat_group + t$heat_group
  }
  list(
    per_hand = data.frame(n_runs = n_runs, n_groups = n_groups),
    heat = data.frame(number = 1:13, run = heat_run, group = heat_group),
    n_hands = n_hands,
    k = k
  )
}

# --- Execution block ---------------------------------------------------

if (sys.nframe() == 0L) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The ggplot2 package is required to generate the plots")
  }
  dir.create("results", showWarnings = FALSE)

  cat("============================================================\n")
  cat("ANALYSIS B: RUNS vs GROUPS\n")
  cat("============================================================\n")

  N_HANDS <- 10000L
  K <- 14L
  SEED <- 20260816L
  cat(sprintf("Simulating %d random hands of %d tiles (seed %d)...\n\n",
              N_HANDS, K, SEED))
  t0 <- proc.time()[["elapsed"]]
  res <- tally_runs_groups(N_HANDS, K, SEED)
  elapsed <- proc.time()[["elapsed"]] - t0
  cat(sprintf("Done in %.1f s.\n\n", elapsed))

  per_hand <- res$per_hand
  total_runs <- sum(per_hand$n_runs)
  total_groups <- sum(per_hand$n_groups)
  total_melds <- total_runs + total_groups
  p_runs <- total_runs / total_melds
  p_groups <- total_groups / total_melds
  hands_with_run <- mean(per_hand$n_runs > 0)
  hands_with_group <- mean(per_hand$n_groups > 0)
  hands_runs_more <- mean(per_hand$n_runs > per_hand$n_groups)
  hands_groups_more <- mean(per_hand$n_groups > per_hand$n_runs)
  hands_equal <- mean(per_hand$n_runs == per_hand$n_groups)

  cat("1) Relative frequency of the two meld types\n")
  cat("   Total formable melds :", total_melds, "\n")
  cat(sprintf("   Runs   : %6d (%5.1f%%)   mean %.2f per hand\n",
              total_runs, 100 * p_runs, mean(per_hand$n_runs)))
  cat(sprintf("   Groups : %6d (%5.1f%%)   mean %.2f per hand\n",
              total_groups, 100 * p_groups, mean(per_hand$n_groups)))
  cat(sprintf("   Hands with >= 1 run   : %5.1f%%\n", 100 * hands_with_run))
  cat(sprintf("   Hands with >= 1 group : %5.1f%%\n", 100 * hands_with_group))
  cat("\n2) Which type dominates each hand?\n")
  cat(sprintf("   More runs   : %5.1f%%   More groups : %5.1f%%   Equal: %5.1f%%\n",
              100 * hands_runs_more, 100 * hands_groups_more, 100 * hands_equal))
  cat("\n3) Tile numbers and meld type (heatmap, mean melds per hand)\n")
  top_run <- tail(res$heat[order(res$heat$run), ], 3L)
  top_group <- tail(res$heat[order(res$heat$group), ], 3L)
  cat("   Top numbers for runs  :",
      paste(sprintf("%d (%.2f)", top_run$number, top_run$run / N_HANDS),
            collapse = ", "), "\n")
  cat("   Top numbers for groups:",
      paste(sprintf("%d (%.2f)", top_group$number, top_group$group / N_HANDS),
            collapse = ", "), "\n\n")

  # --- Save CSV -----------------------------------------------------
  out_dir <- "results"
  write.csv(per_hand, file.path(out_dir, "runs_vs_groups_per_hand.csv"),
            row.names = FALSE)
  heat_long <- data.frame(
    number = rep(1:13, times = 2),
    type = rep(c("run", "group"), each = 13),
    total = c(res$heat$run, res$heat$group),
    mean_per_hand = c(res$heat$run, res$heat$group) / N_HANDS
  )
  write.csv(heat_long, file.path(out_dir, "runs_vs_groups_heatmap.csv"),
            row.names = FALSE)
  summary_df <- data.frame(
    n_hands = N_HANDS,
    k = K,
    total_melds = total_melds,
    total_runs = total_runs,
    total_groups = total_groups,
    p_runs = p_runs,
    p_groups = p_groups,
    mean_runs_per_hand = mean(per_hand$n_runs),
    mean_groups_per_hand = mean(per_hand$n_groups),
    hands_with_run = hands_with_run,
    hands_with_group = hands_with_group,
    hands_runs_more = hands_runs_more,
    hands_groups_more = hands_groups_more,
    hands_equal = hands_equal
  )
  write.csv(summary_df, file.path(out_dir, "runs_vs_groups_summary.csv"),
            row.names = FALSE)

  # --- Plots --------------------------------------------------------
  df_counts <- data.frame(
    count = c(per_hand$n_runs, per_hand$n_groups),
    type = rep(c("Runs", "Groups"), each = N_HANDS)
  )
  df_heat <- heat_long

  plot_distribution <- function(d, n) {
    ggplot2::ggplot(d, ggplot2::aes(x = count, fill = type)) +
      ggplot2::geom_histogram(binwidth = 1, alpha = 0.6,
                              position = "identity") +
      ggplot2::scale_fill_manual(values = c(Runs = "#3b6fd4",
                                            Groups = "#d45a3b")) +
      ggplot2::labs(
        title = "Number of formable melds per hand",
        subtitle = sprintf("distribution over %d random hands of %d tiles",
                           n, 14L),
        x = "melds per hand", y = "hands",
        fill = NULL
      ) +
      ggplot2::theme_minimal()
  }

  plot_heatmap <- function(d, n) {
    ggplot2::ggplot(d, ggplot2::aes(x = number, y = type, fill = mean_per_hand)) +
      ggplot2::geom_tile(colour = "white") +
      ggplot2::geom_text(
        ggplot2::aes(label = sprintf("%.2f", mean_per_hand)),
        size = 2.8, colour = "white"
      ) +
      ggplot2::scale_fill_viridis_c(option = "magma", end = 0.95) +
      ggplot2::scale_x_continuous(breaks = 1:13) +
      ggplot2::labs(
        title = "Formable melds by tile number",
        subtitle = sprintf("mean number of melds per hand that use each number (%d random hands)",
                           n),
        x = "tile number", y = NULL,
        fill = "mean melds / hand"
      ) +
      ggplot2::theme_minimal()
  }

  p_dist <- plot_distribution(df_counts, N_HANDS)
  p_heat <- plot_heatmap(df_heat, N_HANDS)

  png_path_dist <- file.path(out_dir, "runs_vs_groups_distribution.png")
  png_path_heat <- file.path(out_dir, "runs_vs_groups_heatmap.png")
  ggplot2::ggsave(png_path_dist, p_dist, width = 7, height = 4.5, dpi = 150)
  ggplot2::ggsave(png_path_heat, p_heat, width = 8, height = 3, dpi = 150)

  cat("Results saved in results/:\n")
  cat("   runs_vs_groups_per_hand.csv\n")
  cat("   runs_vs_groups_heatmap.csv\n")
  cat("   runs_vs_groups_summary.csv\n")
  cat("   runs_vs_groups_distribution.png\n")
  cat("   runs_vs_groups_heatmap.png\n")
}
