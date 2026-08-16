# demo_branch_and_bound.R
#
# Didactic demonstration of the branch & bound algorithm in solve_opening().
#
# Goals:
#   1. Watch "live" how the algorithm explores the decision tree: what a
#      branch is, what pruning is and what the bound is.
#   2. Verify exactness against a brute-force "oracle" (brute_max) that
#      enumerates ALL combinations of disjoint melds without pruning.
#   3. See that a greedy method (always picking the most valuable meld) can
#      fall short: that is why the exact search is worth it.
#
# Usage: Rscript scripts/demo_branch_and_bound.R   (from the repo root)

here <- function() {
  args <- commandArgs(FALSE)
  f <- sub("^--file=", "", args[grepl("^--file=", args)])
  if (length(f)) {
    normalizePath(dirname(f[1L]), winslash = "/")
  } else {
    normalizePath(getwd(), winslash = "/")
  }
}
source(file.path(here(), "06_opening_analysis.R"))

# Compact tile labels: "4B" = 4 blue, "J" = joker.
sh_n <- function(x) if (is.na(x)) "J" else as.character(x)
tile_lab <- function(i, types) paste0(sh_n(types$number[i]), types$colour[i])

meld_lab <- function(row, types) {
  keep <- which(row > 0L)
  parts <- lapply(keep, function(i) {
    rep(tile_lab(i, types), row[i])
  })
  paste(unlist(parts), collapse = " ")
}

remain_lab <- function(remaining, types) {
  keep <- which(remaining > 0L)
  if (!length(keep)) return("{}")
  paste(vapply(keep, function(i) {
    paste0(tile_lab(i, types), "x", remaining[i])
  }, character(1)), collapse = " ")
}

# --- Traced copy of solve_opening() --------------------------------------
# Same exact logic as in 06_opening_analysis.R, but printing every node,
# its bound, the chosen tile and the prunings.

solve_opening_traced <- function(hand, target = 30) {
  prep <- prepare_hand(hand)
  types <- prep$types
  m <- nrow(types)
  melds <- possible_melds(prep)
  M <- melds$M
  sums <- melds$sums

  cat("--- Tile types in the hand ---\n")
  for (i in seq_len(m)) {
    cat("   ", tile_lab(i, types), "x", types$n[i], "(value", types$value[i], ")\n")
  }
  cat("--- Possible melds ---\n")
  if (nrow(M) == 0L) {
    cat("   (none) -> 0\n")
    return(0L)
  }
  for (j in seq_len(nrow(M))) {
    cat(sprintf("   M%02d  [%3d points]  = %s\n", j, sums[j], meld_lab(M[j, ], types)))
  }

  if (is.finite(target)) {
    cat(sprintf("Shortcut 1: best single meld = %d  (target %d)\n", max(sums), target))
    if (max(sums) >= target) {
      cat("   -> a single meld already reaches the target: done in zero time\n")
      return(as.integer(target))
    }
    cat(sprintf("Shortcut 2: sum of ALL tile values = %d\n",
                sum(types$n * types$value)))
    if (sum(types$n * types$value) < target) {
      cat("   -> even using every tile you can't reach the target -> 0\n")
      return(0L)
    }
  }

  values <- types$value
  best <- 0L
  node <- 0L

  branch <- function(remaining, acc, depth) {
    node <<- node + 1L
    ind <- strrep("  ", depth)
    cat(sprintf(
      "%s[N%02d] remaining: %-25s acc=%2d  best=%2d  BOUND=acc+sum(remaining)=%d\n",
      ind, node, remain_lab(remaining, types), acc, best,
      acc + sum(remaining * values)
    ))

    if (acc >= target) {
      cat(sprintf("%s  acc>=target -> best=%d, CUTTING OFF THE WHOLE SEARCH\n",
                  ind, acc))
      if (acc > best) best <<- acc
      return(TRUE)
    }
    if (acc + sum(remaining * values) <= best) {
      cat(sprintf(
        "%s  PRUNE: bound %d <= best %d -> this branch cannot improve, discard\n",
        ind, acc + sum(remaining * values), best
      ))
      return(FALSE)
    }

    cmp <- M <= rep(remaining, each = nrow(M))
    usable <- rowSums(cmp) == m
    if (!any(usable)) {
      if (acc > best) best <<- acc
      cat(sprintf("%s  leaf: no meld fits anymore -> best=%d\n", ind, best))
      return(FALSE)
    }

    cov <- colSums(M[usable, , drop = FALSE] > 0)
    cand <- which(remaining > 0L & cov > 0L)
    if (length(cand) == 0L) {
      if (acc > best) best <<- acc
      cat(sprintf("%s  leaf: no playable tiles left -> best=%d\n", ind, best))
      return(FALSE)
    }
    tile_idx <- cand[which.min(cov[cand])]
    cat(sprintf(
      "%s  MOST CONSTRAINED tile: %s (covered by only %d usable meld(s))\n",
      ind, tile_lab(tile_idx, types), cov[tile_idx]
    ))
    usable_idx <- which(usable)
    cat(sprintf("%s  usable melds now: %s\n", ind,
                paste(sprintf("[%d=%s]", sums[usable_idx],
                              vapply(usable_idx, function(j) meld_lab(M[j, ], types), "")),
                      collapse = "  ")))

    remaining2 <- remaining
    remaining2[tile_idx] <- 0L
    cat(sprintf("%s  BRANCH A: discard %s\n", ind, tile_lab(tile_idx, types)))
    if (branch(remaining2, acc, depth + 1L)) return(TRUE)

    js <- which(usable & M[, tile_idx] > 0L)
    js <- js[order(sums[js], decreasing = TRUE)]
    for (j in js) {
      cat(sprintf("%s  BRANCH B: use %s in meld [%d=%s]\n",
                  ind, tile_lab(tile_idx, types), sums[j], meld_lab(M[j, ], types)))
      if (branch(remaining - M[j, ], acc + sums[j], depth + 1L)) return(TRUE)
    }
    FALSE
  }

  branch(types$n, 0L, 0L)
  cat(sprintf("===== EXACT RESULT: %d points =====\n", as.integer(best)))
  as.integer(best)
}

# --- Brute-force oracle (no pruning, no heuristics) -----------------------
# Complete enumeration of ALL combinations of disjoint melds, in increasing
# index order (each combination is visited exactly once).

brute_max <- function(hand) {
  prep <- prepare_hand(hand)
  types <- prep$types
  m <- nrow(types)
  melds <- possible_melds(prep)
  M <- melds$M
  sums <- melds$sums
  if (nrow(M) == 0L) return(0L)
  best <- 0L
  dfs <- function(remaining, acc, from) {
    if (acc > best) best <<- acc
    cmp <- M <= rep(remaining, each = nrow(M))
    usable <- which(rowSums(cmp) == m)
    for (j in usable) {
      if (j > from) dfs(remaining - M[j, ], acc + sums[j], j)
    }
  }
  dfs(types$n, 0L, 0L)
  best
}

# --- Greedy (negative reference) ------------------------------------------
# Always takes the highest-scoring available meld, and so on.

greedy_max <- function(hand) {
  prep <- prepare_hand(hand)
  types <- prep$types
  m <- nrow(types)
  melds <- possible_melds(prep)
  M <- melds$M
  sums <- melds$sums
  if (nrow(M) == 0L) return(0L)
  remaining <- types$n
  acc <- 0L
  repeat {
    cmp <- M <= rep(remaining, each = nrow(M))
    usable <- which(rowSums(cmp) == m)
    if (!length(usable)) break
    j <- usable[which.max(sums[usable])]
    remaining <- remaining - M[j, ]
    acc <- acc + sums[j]
  }
  acc
}

show_hand <- function(number, colour) {
  tiles(number, colour)
}

# --- Search tree (phylogenetic style) ------------------------------------
# `record_search` runs the same branch & bound as solve_opening() but records
# every node (id, parent, depth, acc, bound, best, decision, outcome).
# `tree_layout` computes the phylogram coordinates:
#   - x = depth in the tree
#   - leaves are spread vertically and internal nodes sit at the midpoint of
#     their children (cladogram).
# `plot_search_tree` draws the tree in the Plots pane of RStudio.

record_search <- function(hand, target = 30) {
  prep <- prepare_hand(hand)
  types <- prep$types
  m <- nrow(types)
  melds <- possible_melds(prep)
  M <- melds$M
  sums <- melds$sums
  values <- types$value
  col_abbr <- c(red = "R", blue = "B", yellow = "Y", black = "K", wild = "W")
  tile_lab <- function(i) {
    if (is.na(types$number[i])) "J" else paste0(types$number[i], col_abbr[types$colour[i]])
  }

  nodes <- data.frame(
    id = integer(), parent = integer(), depth = integer(),
    acc = integer(), cota = integer(), best = integer(),
    tile = character(), action = character(),
    outcome = character(), newbest = logical(),
    stringsAsFactors = FALSE
  )
  next_id <- 0L
  best <- 0L

  add_node <- function(parent, acc, cota, action) {
    next_id <<- next_id + 1L
    depth <- if (is.na(parent)) 0L else nodes$depth[nodes$id == parent] + 1L
    nodes <<- rbind(nodes, data.frame(
      id = next_id, parent = parent, depth = depth, acc = acc,
      cota = cota, best = best, tile = "",
      action = action, outcome = "node", newbest = FALSE,
      stringsAsFactors = FALSE
    ))
    next_id
  }
  set_outcome <- function(id, out) nodes$outcome[nodes$id == id] <<- out
  set_newbest <- function(id) nodes$newbest[nodes$id == id] <<- TRUE
  update_best <- function(id, acc) {
    if (acc > best) {
      best <<- acc
      set_newbest(id)
    }
  }

  if (nrow(M) == 0L) {
    return(list(nodes = nodes, best = 0L))
  }
  total_value <- sum(types$n * values)

  # Finite-target shortcuts (same criterion as solve_opening()).
  if (is.finite(target)) {
    if (max(sums) >= target) {
      rid <- add_node(NA_integer_, as.integer(target), as.integer(target),
                      "shortcut 1: the best single meld already reaches the target")
      set_outcome(rid, "target")
      set_newbest(rid)
      return(list(nodes = nodes, best = as.integer(target)))
    }
    if (total_value < target) {
      rid <- add_node(NA_integer_, 0L, total_value,
                      "shortcut 2: even using every tile the target is out of reach")
      set_outcome(rid, "leaf")
      return(list(nodes = nodes, best = 0L))
    }
  }

  root_id <- add_node(NA_integer_, 0L, total_value, "root: initial hand")
  set_outcome(root_id, "root")

  # `branch` processes an already created node (id): it decides on its most
  # constrained tile and creates the children of branches A and B.
  branch <- function(id, remaining, acc) {
    cota <- acc + sum(remaining * values)
    nodes$cota[nodes$id == id] <<- cota
    if (acc >= target) {
      if (acc > best) {
        best <<- acc
        set_newbest(id)
      }
      set_outcome(id, "target")
      return(TRUE)
    }
    if (cota <= best) {
      set_outcome(id, "prune")
      return(FALSE)
    }
    cmp <- M <= rep(remaining, each = nrow(M))
    usable <- rowSums(cmp) == m
    if (!any(usable)) {
      update_best(id, acc)
      set_outcome(id, "leaf")
      return(FALSE)
    }
    cov <- colSums(M[usable, , drop = FALSE] > 0)
    cand <- which(remaining > 0L & cov > 0L)
    if (length(cand) == 0L) {
      update_best(id, acc)
      set_outcome(id, "leaf")
      return(FALSE)
    }
    tile_idx <- cand[which.min(cov[cand])]
    nodes$tile[nodes$id == id] <<- tile_lab(tile_idx)

    # Branch A: discard the most constrained tile.
    remaining2 <- remaining
    remaining2[tile_idx] <- 0L
    aid <- add_node(id, acc, cota, paste("discard", tile_lab(tile_idx)))
    if (branch(aid, remaining2, acc)) return(TRUE)

    # Branch B: use it in every meld that contains it (best first).
    js <- which(usable & M[, tile_idx] > 0L)
    js <- js[order(sums[js], decreasing = TRUE)]
    for (j in js) {
      bid <- add_node(id, acc + sums[j], acc + sums[j],
                      sprintf("use %s in M%02d = %d pts",
                              tile_lab(tile_idx), j, sums[j]))
      if (branch(bid, remaining - M[j, ], acc + sums[j])) return(TRUE)
    }
    FALSE
  }

  branch(root_id, types$n, 0L)
  list(nodes = nodes, best = as.integer(best))
}

# Cladogram coordinates: leaves spread vertically in order of appearance,
# internal nodes at the midpoint of their children, x = depth.
tree_layout <- function(nodes) {
  n <- nrow(nodes)
  id <- nodes$id
  has_child <- id %in% nodes$parent[!is.na(nodes$parent)]
  y <- numeric(n)
  k <- 0L
  for (i in seq_len(n)) {
    if (!has_child[i]) {
      k <- k + 1L
      y[i] <- k
    }
  }
  children <- split(id, as.character(nodes$parent))
  for (i in rev(seq_len(n))) {
    if (has_child[i]) {
      y[i] <- mean(y[children[[as.character(id[i])]]])
    }
  }
  list(x = nodes$depth, y = y, n_leaves = k)
}

# Draws the search tree in the Plots pane (base graphics).
plot_search_tree <- function(hand, target = 30, main = NULL, ...) {
  rec <- record_search(hand, target)
  nodes <- rec$nodes
  if (nrow(nodes) == 0L) {
    plot.new()
    title("No formable melds")
    return(invisible(rec))
  }

  lay <- tree_layout(nodes)
  x <- lay$x
  y <- lay$y
  outcome <- nodes$outcome

  col <- c(target = "#7b3294", prune = "#d7191c", leaf = "#1a9850",
           node = "#2c7bb6", root = "black")
  pch <- c(target = 8, prune = 4, leaf = 21, node = 21, root = 22)

  maxl <- max(nchar(nodes$action),
              nchar(sprintf("acc=%d  best=%d  bound=%d",
                            max(nodes$acc), max(nodes$best), max(nodes$cota))))
  xlim <- c(0, max(x) + 0.6 + maxl * 0.26)
  ylim <- c(0.5, lay$n_leaves + 0.5)

  if (is.null(main)) {
    main <- sprintf("Branch & bound (target = %s) - exact maximum: %d pts",
                    if (is.finite(target)) as.character(target) else "Inf",
                    rec$best)
  }

  plot.new()
  plot.window(xlim = xlim, ylim = ylim)
  parents <- nodes$parent

  # Ladder edges (phylogram style).
  for (i in seq_len(nrow(nodes))) {
    if (is.na(parents[i])) next
    p <- parents[i]
    segments(x[p], y[p], x[i], y[p], col = "grey70")
    segments(x[i], y[p], x[i], y[i], col = "grey70")
  }

  # Highlight the path of every node that set a new best record.
  for (i in which(nodes$newbest)) {
    j <- i
    while (!is.na(parents[j])) {
      p <- parents[j]
      segments(x[p], y[p], x[j], y[p], col = "#fdae61", lwd = 2.5)
      segments(x[j], y[p], x[j], y[j], col = "#fdae61", lwd = 2.5)
      j <- p
    }
  }

  # Nodes.
  for (i in seq_len(nrow(nodes))) {
    points(x[i], y[i], pch = pch[outcome[i]],
           cex = if (outcome[i] == "root") 1.4 else 0.9,
           col = col[outcome[i]], bg = col[outcome[i]], lwd = 1.5)
  }

  # Labels (action + context line).
  for (i in seq_len(nrow(nodes))) {
    text(x[i] + 0.2, y[i], nodes$action[i], adj = c(0, 1), cex = 0.72)
    text(x[i] + 0.2, y[i],
         sprintf("acc=%d  best=%d  bound=%d  [%s]",
                 nodes$acc[i], nodes$best[i], nodes$cota[i], outcome[i]),
         adj = c(0, -0.55), cex = 0.6, col = "grey30")
  }

  title(main, font.main = 1)
  legend("topright", inset = 0.02, pch = pch, col = col, legend = names(col),
         pt.bg = col, bg = "white", cex = 0.8, title = "Node type")
  invisible(rec)
}

# ============================================================================
# 1) FULL TRACE: hand where competing melds must be COMBINED
#    (the two blue 4s are split between the run and the group).
# ============================================================================

h1 <- show_hand(c(2, 3, 4, 4, 5, 4, 4),
                c("blue", "blue", "blue", "blue", "blue", "black", "yellow"))

cat("\n############ EXAMPLE 1: trace with target = Inf (exact maximum) ############\n\n")
r1 <- solve_opening_traced(h1, target = Inf)
cat("\nVerification against brute force:",
    if (r1 == brute_max(h1)) "OK (matches)\n" else "ERROR\n")

cat("\nDoes this hand open with 14 tiles? (target = 30)\n")
r1b <- solve_opening_traced(h1, target = 30)
cat("\ncan_open(h1) =", can_open(h1), "\n")

# ============================================================================
# 2) Hand that DOES open: shortcut 1 cuts immediately.
# ============================================================================

h2 <- show_hand(6:13, rep("blue", 8))
cat("\n############ EXAMPLE 2: blue run 6..13 (opens easily) ############\n\n")
r2 <- solve_opening_traced(h2, target = 30)
cat("\ncan_open(h2) =", can_open(h2), "  (8 tiles: 76 points, rule of 30 OK)\n")

# ============================================================================
# 3) GREEDY FAILS: picking the most expensive meld first misses the best play.
# ============================================================================

h3 <- show_hand(c(2, 3, 4, 5, 6, 3, 3, 4, 4),
                c("blue", "blue", "blue", "blue", "blue",
                  "yellow", "black", "yellow", "black"))
cat("\n############ EXAMPLE 3: the greedy can be wrong ############\n\n")
print(h3)
cat("  Greedy (best meld first):", greedy_max(h3), "points\n")
cat("  Exact optimum (branch&bound):", solve_opening(h3, Inf), "points\n")
cat("  Brute force:", brute_max(h3), "points\n")

# ============================================================================
# 4) MASS VERIFICATION: small random hands, branch&bound vs oracle.
# ============================================================================

cat("\n############ EXAMPLE 4: exactness check on random hands ############\n\n")
set.seed(123)
sizes <- 6:10
reps <- 40
bad <- 0L
greedy_fail <- 0L
total <- 0L
t0 <- proc.time()
for (k in sizes) {
  for (i in seq_len(reps)) {
    h <- random_hand(k)
    exact <- solve_opening(h, Inf)
    oracle <- brute_max(h)
    total <- total + 1L
    if (exact != oracle) {
      bad <- bad + 1L
      cat("MISMATCH with", k, "tiles:", exact, "vs", oracle, "\n")
    }
    if (greedy_max(h) < exact) greedy_fail <- greedy_fail + 1L
  }
}
t1 <- proc.time()
cat(sprintf(
  "Verified %d random hands in %.1fs\n  branch&bound == brute force in %d/%d cases\n",
  total, (t1 - t0)[["elapsed"]], total - bad, total
))
cat(sprintf("  The greedy fell short in %d of %d hands (%.1f%%)\n",
            greedy_fail, total, 100 * greedy_fail / total))

# ============================================================================
# 5) SEARCH TREE IN THE PLOTS PANE (phylogenetic style)
#    In RStudio it is drawn in the Plots pane. Run with Rscript without an
#    interactive console it saves a PNG to results/.
# ============================================================================

cat("\n############ EXAMPLE 5: search tree (Plots pane) ############\n\n")
cat("  Calling plot_search_tree(h1, target = Inf) ...\n")

plot_one <- function(hand, target, file, main) {
  if (interactive()) {
    plot_search_tree(hand, target, main = main)
  } else {
    dir.create("results", showWarnings = FALSE)
    grDevices::png(file.path("results", file), width = 1400, height = 800)
    plot_search_tree(hand, target, main = main)
    grDevices::dev.off()
    cat("  Plot saved to results/", file, "\n", sep = "")
  }
}

plot_one(h1, Inf,
         "demo_bnb_example1_max.png",
         "Example 1 - exact maximum with branch & bound (26 pts)")
plot_one(h1, 30,
         "demo_bnb_example1_canopen.png",
         "Example 1 - rule of 30 (can_open = FALSE, target shortcut)")
plot_one(h3, 30,
         "demo_bnb_example3.png",
         "Example 3 - the greedy is wrong (exact maximum 24 pts)")
