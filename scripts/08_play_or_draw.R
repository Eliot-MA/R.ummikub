# 08_play_or_draw.R
#
# Objective 6, analysis C: PLAY OR DRAW.
#
# Question: at my turn, with a hand H, a board B and a pool P, should I
# play a meld or draw a tile from the pool?
#
# Method. Both sides of that decision need the same inner quantity,
#
#     V(H) = maximum total value meldeable from the hand H
#
# so this script starts by building V. It is computed with the dynamic
# program of van Rijn, Takes and Vis (2016), "The Complexity of Rummikub
# Problems", arXiv:1604.07553, which solves the Rummikub puzzle in
# polynomial time. The design rationale, the alternatives that were
# rejected and the open decisions are recorded in
# reports/play_or_draw.qmd, which is the reference document for this
# analysis.
#
# What the measurements actually showed, and what phase 1 concludes:
#
#   * The program is exact. It agrees with the branch & bound of
#     scripts/06_opening_analysis.R on every hand tested, and with
#     coverage_solver() in its coverage form.
#   * It is SLOWER than that branch & bound, by two to three orders of
#     magnitude on full 14-tile hands. The reason is the state space. The
#     paper's O(n * k * f(m)) bound counts the run state of ONE colour, but
#     the state that is actually needed is the product over colours: with
#     two copies each that is 10^4 = 10000 states per value, not 10. See
#     decision D6 in the design document.
#   * So the production scorer for phases 2 to 5 is max_opening(), and
#     this program stays as the reference definition of V and as the
#     oracle for the tests. Decision D7.
#
# The two agree, which is the useful outcome: the fast scorer inherits the
# correctness of the slow one.
#
# PHASES (see reports/play_or_draw.qmd for the full plan)
#   Phase 1  V as a dynamic program                      <- THIS FILE
#   Phase 1b jokers inside V
#   Phase 2  draw_value(): exact expected gain of one drawn tile
#   Phase 3  exposure(): what playing gives the opponent
#   Phase 4  decide_play_or_draw() and the threshold sweep over lambda
#   Phase 5  simulate_game(): policy comparison
#   Phase 6  reports/play_or_draw.qmd write-up
#
# The execution block (phase 1) verifies the program against the branch &
# bound and measures the cost of one solve by hand size. It only runs when
# the script is launched directly (Rscript scripts/08_play_or_draw.R).

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
# 06 provides the branch & bound used as the independent oracle to verify
# this program against (max_opening, coverage_solver, prepare_hand).
source_local("06_opening_analysis.R")

# === The value function V ============================================
#
# ---------------------------------------------------------------------------
# 1) Inventory of the hand as a (number x colour) matrix
# ---------------------------------------------------------------------------

# Copies of each (number, colour) in the hand: a 13 x 4 integer matrix
# indexed by [number, colour]. Rows of an empty hand are all zero.
hand_matrix <- function(hand) {
  m <- matrix(0L, nrow = 13L, ncol = length(COLOURS),
              dimnames = list(NULL, COLOURS))
  if (is.null(hand) || nrow(hand) == 0L) return(m)
  stopifnot(is.data.frame(hand), all(c("number", "colour") %in% names(hand)))
  if (any(is.na(hand$number))) {
    stop("hand_matrix() does not support jokers: number must be a real tile value")
  }
  for (i in seq_len(nrow(hand))) {
    ci <- match(hand$colour[i], COLOURS)
    if (is.na(ci)) stop("Unknown colour: ", hand$colour[i])
    m[hand$number[i], ci] <- m[hand$number[i], ci] + 1L
  }
  m
}

# ---------------------------------------------------------------------------
# 2) Groups: exact maximum coverage of the leftovers of ONE tile value
# ---------------------------------------------------------------------------

# `leftover` is an integer vector, one entry per colour, with the number
# of copies of a single tile value that are not used in a run. Returns the
# maximum number of those tiles that can be covered by disjoint groups
# (3 or 4 tiles of the same value, distinct colours).
#
# EXACT, by enumerating the first group of an optimal solution and
# recursing on what is left.
#
# Do not replace this by the obvious greedy ("take as many distinct
# colours as possible, four first"). That greedy is wrong. With leftovers
# (2,2,1,1) it takes a group of four and stops, covering 4 tiles, while
# two groups of three cover all 6. The enumeration returns 6. Since the
# objective is the total value, and every tile of a group of value v is
# worth v, coverage is exactly the score -- so a wrong helper here is a
# wrong answer, not a slow one.
#
# The recursion is tiny: at most 8 tiles and 4 colours, so at most a
# handful of branches at each of at most two levels.
total_group_size <- function(leftover) {
  if (sum(leftover) < 3L) return(0L)
  n_col <- sum(leftover > 0L)
  best <- 0L
  for (size in c(4L, 3L)) {
    if (n_col < size) next
    for (cs in utils::combn(which(leftover > 0L), size, simplify = FALSE)) {
      rest <- leftover
      rest[cs] <- rest[cs] - 1L
      cand <- size + total_group_size(rest)
      if (cand > best) best <- cand
    }
  }
  best
}

# ---------------------------------------------------------------------------
# 3) Runs: all the ways one colour can use its copies of value v
# ---------------------------------------------------------------------------

# `rl`      : current run lengths of one colour, a vector of length m with
#             values in 0..3, where 3 means "3 or more". Runs of equal
#             length are interchangeable, so `rl` is kept sorted.
# `avail`   : copies of (this colour, v) that the hand actually has.
# `value`   : the tile value v.
# `reward`  : function of a tile value giving its worth. `identity` scores
#             melded value; `function(v) 1L` scores melded TILE COUNT.
#
# Returns a list of options, each `list(runs = <new sorted rl>,
# used = <copies spent on runs>, score = <points earned at this value>)`.
#
# Scoring follows the paper. A run that grows from length 2 to length 3
# becomes legal and scores the whole run, (v-2) + (v-1) + v. A run that is
# already legal (length 3, meaning 3 or more) and is continued simply
# scores v more, because the extra tile is worth its own value. Runs of
# length 0 or 1 score nothing yet, and a run that is not continued is
# reset to 0 and scores nothing at all -- its tiles cannot be melded.
#
# Capping the length at 3 is what makes the state space finite. Lengths
# 4, 5, 6... need no separate decision: the run is already legal, and
# every further tile adds only its own value.
colour_run_options <- function(rl, avail, value, reward = identity) {
  m <- length(rl)
  out <- list()
  for (j in 0:min(avail, m)) {
    sels <- if (j == 0L) list(integer(0)) else
      utils::combn(seq_len(m), j, simplify = FALSE)
    for (sel in sels) {
      new <- rep(0L, m)              # runs we do not continue are dropped
      sc <- 0
      for (i in sel) {
        len <- rl[i]
        if (len == 2L) {
          sc <- sc + reward(value - 2L) + reward(value - 1L) + reward(value)
        } else if (len == 3L) {
          sc <- sc + reward(value)
        }
        new[i] <- min(len + 1L, 3L)
      }
      out[[length(out) + 1L]] <- list(runs = sort(new), used = j, score = sc)
    }
  }
  # Continuing different runs of equal length leads to the same state, so
  # the same option can be generated more than once. Keep one.
  key <- vapply(out, function(o) {
    paste0(paste(o$runs, collapse = ""), "|", o$used, "|", o$score)
  }, character(1))
  out[!duplicated(key)]
}

# ---------------------------------------------------------------------------
# 4) Lookup tables, built once per (copies, reward) pair
# ---------------------------------------------------------------------------

# The tables below depend only on the shape of the game and on `reward`,
# never on the hand, so they are built once and cached.
.value_table_cache <- new.env(parent = emptyenv())

# All non-decreasing run-length vectors of length `copies` over 0..3, i.e.
# every multiset of `copies` run lengths. For copies = 2 that is the ten
# states the paper counts as f(m) = C(m+3, 3) = 10.
run_states <- function(copies) {
  out <- list()
  gen <- function(from, k, pre) {
    if (k == 0L) {
      out[[length(out) + 1L]] <<- pre
      return(invisible(NULL))
    }
    for (v in from:3) gen(v, k - 1L, c(pre, v))
  }
  gen(0L, copies, integer(0))
  out
}

value_tables <- function(copies = 2L, reward = identity) {
  ck <- paste0(copies, "|", reward(7))
  hit <- .value_table_cache[[ck]]
  if (!is.null(hit)) return(hit)

  states <- run_states(copies)
  n1 <- length(states)
  n_col <- length(COLOURS)
  mult <- as.integer(c(1L, cumprod(rep(n1, n_col))[-n_col]))
  n_combo <- as.integer(n1^n_col)

  # Index of a state vector inside `states`.
  state_index <- function(rl) {
    for (i in seq_along(states)) {
      if (identical(states[[i]], as.integer(rl))) return(i - 1L)
    }
    stop("run state not found: ", paste(rl, collapse = ","))
  }

  # total_group_size for every leftover vector with 0..2 copies per colour.
  # At most 3^4 = 81 cases, so the whole table is cheap.
  gf <- integer(3L^n_col)
  for (code in seq_len(3L^n_col) - 1L) {
    r <- code
    lf <- integer(n_col)
    for (ci in seq_len(n_col)) {
      lf[ci] <- r %% 3L
      r <- r %/% 3L
    }
    gf[code + 1L] <- total_group_size(lf)
  }

  # opt[[colour]][[(value - 1) * 3 + avail + 1]][[state]] = the transitions of
  # that colour, at that value, with that many copies in hand, starting from
  # run state number `state`. Transitions depend on the current run lengths,
  # so the table has to cover every state, not just the empty one.
  opt <- vector("list", n_col)
  for (ci in seq_len(n_col)) {
    o <- vector("list", 13L * 3L)
    for (v in 1:13) {
      for (av in 0:2) {
        per_state <- vector("list", n1)
        for (si in seq_len(n1)) {
          lst <- colour_run_options(states[[si]], av, v, reward)
          per_state[[si]] <- list(
            idx = vapply(lst, function(x) state_index(x$runs), integer(1)),
            used = vapply(lst, function(x) x$used, integer(1)),
            score = vapply(lst, function(x) x$score, numeric(1))
          )
        }
        o[[(v - 1L) * 3L + av + 1L]] <- per_state
      }
    }
    opt[[ci]] <- o
  }

  tb <- list(states = states, n1 = n1, mult = mult, n_combo = n_combo,
             gf = gf, opt = opt, pow3 = as.numeric(3^(seq_len(n_col) - 1L)),
             memo = rep(NA_real_, 14L * n_combo),
             dirty = integer(14L * n_combo))
  .value_table_cache[[ck]] <- tb
  tb
}

# ---------------------------------------------------------------------------
# 5) The dynamic program
# ---------------------------------------------------------------------------

# Maximum worth meldeable from `hand` with disjoint runs and groups, or 0
# if no meld exists. Exact.
#
# The state is (value, run length of every colour and copy, capped at 3).
# The program sweeps the values 1..13 once, left to right, and never
# backtracks: what happened at lower values is fully summarised by the run
# lengths, because runs are the only thing that spans values. Groups are
# single-value, so once the run continuations of a value are fixed, its
# leftover tiles can be grouped optimally and independently of every other
# value.
#
# `copies` is the number of copies of each tile in the pool, the paper's m.
# It sets how many runs per colour are tracked; 2 for the standard
# 106-tile pool.
#
# `reward` maps a tile value to its worth: `identity` maximises melded
# value, `function(v) 1L` maximises the number of melded tiles.
#
# Implementation note. The paper indexes its table by a single run vector;
# the state is really the PRODUCT of the per-colour states, so for four
# colours with two copies each there are 10^4 = 10000 of them, not the
# handful the paper's bound suggests. Everything below exists to make
# that cheap: run states are enumerated once, transitions are tabulated
# once, the memo is a flat numeric vector addressed by an integer, and
# colours with no tile at the current value are skipped (their only option
# is the identity, which also resets their runs).
best_meld_value <- function(hand, copies = 2L, reward = identity) {
  if (!is.null(hand) && !is.data.frame(hand)) {
    stop("hand must be a data.frame with columns number and colour")
  }
  if (!is.null(hand) && nrow(hand) > 0L && any(is.na(hand$number))) {
    stop("best_meld_value() does not support jokers yet (phase 1b): a hand ",
         "containing a joker cannot be scored exactly here. See ",
         "reports/play_or_draw.qmd, decision D2.")
  }
  if (copies < 1L) stop("copies must be >= 1")
  H <- hand_matrix(hand)
  # The program tracks `copies` run slots per colour and looks up transitions
  # by 0..2 copies in hand, so a hand richer than the pool has no answer.
  if (any(H > copies)) {
    stop("hand holds more than ", copies, " copies of some tile, which the ",
         "pool cannot supply")
  }
  tb <- value_tables(copies, reward)
  mult <- tb$mult
  gf <- tb$gf
  n1 <- tb$n1
  n_combo <- tb$n_combo

  # Values with at least one tile. Outside [lo, hi] nothing can happen:
  # below lo there is nothing to start from, above hi nothing to extend.
  nz <- which(rowSums(H) > 0L)
  if (length(nz) == 0L) return(0)
  lo <- nz[1L]
  hi <- nz[length(nz)]

  # What one tile of each value is worth, indexed by that value. The suffix
  # is the total worth still in hand from value v onwards, used to stop the
  # sweep once the hand runs out; nothing below the lowest value present can
  # be reached, so the recursion starts there.
  reward_v <- vapply(seq_len(13L), reward, numeric(1))
  suffix_r <- rev(cumsum(rev(rowSums(H) * reward_v)))

  # Base-3 encoding of the hand at every value, before the runs take
  # anything. Subtracting what the runs use, colour by colour, turns this
  # into the encoding of the leftovers.
  full_code <- as.numeric(H %*% tb$pow3)

  # The memo is kept in the cached table and cleared afterwards by resetting
  # only the entries this call touched. Reallocating it every call would
  # dominate the runtime: it is 14 * 10000 doubles, while a typical hand
  # visits only a few dozen states.
  memo <- tb$memo
  dirty <- tb$dirty
  nd <- 0L

  rec <- function(v, s) {
    if (v > hi) return(0)
    mi <- v * n_combo + s + 1L
    hit <- memo[mi]
    if (!is.na(hit)) return(hit)
    nd <<- nd + 1L
    if (nd <= length(dirty)) dirty[nd] <<- mi
    if (suffix_r[v] == 0) {
      memo[mi] <- 0
      return(0)
    }
    av <- as.integer(H[v, ])

    # Enumerate the joint run continuations. Only colours that actually
    # hold a tile of value v can do anything; for the others the sole
    # option is to drop the run, which leaves both the score and the tiles
    # taken unchanged, and resets that colour's run to zero.
    ci_act <- which(av > 0L)
    o_list <- vector("list", length(ci_act))
    m_act <- integer(length(ci_act))
    for (a in seq_along(ci_act)) {
      ci <- ci_act[a]
      # Split the joint state into this colour's run state.
      si <- (s %/% mult[ci]) %% n1
      o <- tb$opt[[ci]][[(v - 1L) * 3L + av[ci] + 1L]][[si + 1L]]
      o_list[[a]] <- o
      m_act[a] <- length(o$idx)
    }

    st <- 0L
    sc <- 0
    code <- full_code[v]
    L <- 1L
    for (a in seq_along(ci_act)) {
      o <- o_list[[a]]
      m <- m_act[a]
      st <- rep(st, each = m) + rep(o$idx * mult[ci_act[a]], times = L)
      sc <- rep(sc, each = m) + rep(o$score, times = L)
      code <- rep(code, each = m) - rep(o$used * tb$pow3[ci_act[a]], times = L)
      L <- L * m
    }

    # `code` is the leftover vector, one entry per colour encoded in base 3,
    # so the whole 3^length(COLOURS) table of total_group_size() can be
    # indexed directly. It identifies the leftovers completely, so it is
    # also what makes two enumerations of the same state comparable.
    keep <- !duplicated(cbind(st, code))
    st <- st[keep]; sc <- sc[keep]
    gv <- gf[as.integer(code[keep]) + 1L] * reward_v[v]

    # Look at the most promising continuations first, so that `best` is
    # already high by the time the rest are tried. Ordering is safe; PRUNING
    # would not be. When a run reaches length 3 at value v the program
    # credits the whole run, (v-2) + (v-1) + v, and two of those tiles have
    # values below v. So the value still obtainable from a state is NOT
    # bounded by the worth of the tiles with higher numbers, and a suffix
    # bound here silently drops valid solutions: 3,4,5,6 of one colour then
    # returns 15 instead of 18.
    ord <- order(sc + gv, decreasing = TRUE)
    st <- st[ord]; sc <- sc[ord]; gv <- gv[ord]
    best <- 0
    for (i in seq_along(st)) {
      cand <- sc[i] + gv[i] + rec(v + 1L, st[i])
      if (cand > best) best <- cand
    }
    memo[mi] <- best
    best
  }

  out <- rec(lo, 0L)
  if (nd > 0L) memo[dirty[seq_len(nd)]] <- NA_real_
  out
}

# Maximum number of TILES meldeable from the hand, instead of value. Same
# program with a flat reward, so it can be checked against
# coverage_solver() in scripts/07_runs_vs_groups.R.
best_meld_coverage <- function(hand, copies = 2L) {
  best_meld_value(hand, copies = copies, reward = function(v) 1)
}

# === Verification against the branch & bound =========================
#
# Independent oracle: solve_opening(hand, target = Inf) from
# scripts/06_opening_analysis.R enumerates candidate melds and searches
# them with a different representation, a different bound and a different
# search order. Agreement on random hands is strong evidence that the
# dynamic program is exact. The pattern follows demo_branch_and_bound.R,
# which uses brute_max() the same way.

# A random hand of k real tiles (no jokers), so that both solvers see the
# same problem. Jokers appear in about a quarter of 14-tile hands and the
# dynamic program does not accept them yet.
random_real_hand <- function(k = 14L) {
  pool <- tile_pool()
  pool <- pool[!is.na(pool$number), , drop = FALSE]
  pool[sample.int(nrow(pool), k), , drop = FALSE]
}

# Compare the two solvers on `n` random hands of `k` tiles.
# Returns the number of agreements and the timings.
verify_against_bnb <- function(n = 200L, k = 14L, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  agree <- 0L
  t_dp <- 0
  t_bnb <- 0
  for (i in seq_len(n)) {
    hand <- random_real_hand(k)
    t0 <- proc.time()[["elapsed"]]
    v_dp <- best_meld_value(hand)
    t1 <- proc.time()[["elapsed"]]
    v_bnb <- max_opening(hand)
    t2 <- proc.time()[["elapsed"]]
    t_dp <- t_dp + (t1 - t0)
    t_bnb <- t_bnb + (t2 - t1)
    if (identical(as.integer(v_dp), as.integer(v_bnb))) agree <- agree + 1L
  }
  list(n = n, k = k, agree = agree, seconds_dp = t_dp, seconds_bnb = t_bnb)
}

# === Execution block (only when the script is launched directly) =======
#
# NOTE ON COST. The dynamic program is exact, but at this state space it is
# SLOWER than the branch & bound it is checked against: see the table below
# and decision D7 in reports/play_or_draw.qmd. The sample sizes here are
# small for that reason. To use V in a search, call max_opening() from
# scripts/06_opening_analysis.R, which is the fast exact scorer validated
# against this program. Keep best_meld_value() as the reference definition
# and as the oracle for the tests.

if (sys.nframe() == 0L) {
  dir.create("results", showWarnings = FALSE)

  # --- 1) Exactness: the two solvers must never disagree --------------
  cat("PHASE 1 - verifying the dynamic program against the branch & bound\n")
  v <- verify_against_bnb(n = 60L, k = 14L, seed = 20260820)
  cat(sprintf("  hands of 14 tiles, no jokers: %d\n", v$n))
  cat(sprintf("  agreements: %d / %d\n", v$agree, v$n))
  cat(sprintf("  dynamic program: %.2fs   branch & bound: %.2fs\n",
              v$seconds_dp, v$seconds_bnb))
  if (v$agree != v$n) stop("DYNAMIC PROGRAM DISAGREES WITH THE B&B")

  # Smaller hands too: the B&B is the only other exact reference we have,
  # so exercise shapes where melds are scarce and the search is least guided.
  for (kk in c(4L, 6L, 8L, 10L, 12L)) {
    vv <- verify_against_bnb(n = 60L, k = kk, seed = 20260820 + kk)
    cat(sprintf("  k=%2d  agreements %d / %d\n", kk, vv$agree, vv$n))
    if (vv$agree != vv$n) {
      stop("DYNAMIC PROGRAM DISAGREES WITH THE B&B at k=", kk)
    }
  }

  # --- 2) Coverage: the same program with a flat reward ---------------
  cat("\nPHASE 1 - coverage variant vs coverage_solver()\n")
  set.seed(20260820)
  bad <- 0L
  for (i in 1:40) {
    hand <- random_real_hand(12L)
    if (!identical(as.integer(best_meld_coverage(hand)),
                   as.integer(solve_opening_coverage(hand)))) {
      bad <- bad + 1L
    }
  }
  cat(sprintf("  hands of 12 tiles: %d mismatches out of 40\n", bad))
  if (bad > 0L) stop("COVERAGE VARIANT DISAGREES WITH coverage_solver()")

  # --- 3) Cost of one exact solve, by hand size -----------------------
  cat("\nPHASE 1 - cost of one exact solve, by hand size\n")
  sizes <- 4:14
  rows <- data.frame(k = sizes, ms_dp = NA_real_, ms_bnb = NA_real_)
  for (i in seq_along(sizes)) {
    kk <- sizes[i]
    reps <- 25L
    hands <- lapply(seq_len(reps), function(r) random_real_hand(kk))
    t0 <- proc.time()[["elapsed"]]
    for (h in hands) best_meld_value(h)
    t1 <- proc.time()[["elapsed"]]
    for (h in hands) max_opening(h)
    t2 <- proc.time()[["elapsed"]]
    rows$ms_dp[i] <- 1000 * (t1 - t0) / reps
    rows$ms_bnb[i] <- 1000 * (t2 - t1) / reps
  }
  print(rows, row.names = FALSE)
  write.csv(rows, "results/phase1_solve_cost.csv", row.names = FALSE)

  # --- 4) A sanity check on the pieces --------------------------------
  # total_group_size is exact where the greedy is not. This is the
  # regression test for decision D3 in the design document.
  cat("\nPHASE 1 - total_group_size (exact) vs a take-four-first greedy\n")
  g <- function(leftover) {
    if (sum(leftover) < 3L) return(0L)
    take <- min(4L, sum(leftover > 0L))
    if (take < 3L) return(0L)
    rest <- leftover
    for (ci in seq_along(rest)) if (rest[ci] > 0L) rest[ci] <- rest[ci] - 1L
    g(rest) + take
  }
  bad <- 0L
  for (a in 0:2) for (b in 0:2) for (cc in 0:2) for (d in 0:2) {
    lf <- c(a, b, cc, d)
    if (total_group_size(lf) != g(lf)) bad <- bad + 1L
  }
  cat(sprintf("  all 81 leftover vectors: %d where the greedy is wrong\n", bad))
  cat("  e.g. (2,2,1,1): exact =", total_group_size(c(2L, 2L, 1L, 1L)),
      " greedy =", g(c(2L, 2L, 1L, 1L)), "\n")

  cat("\nWrote results/phase1_solve_cost.csv\n")
}

