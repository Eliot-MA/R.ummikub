# 01_melds_validos.R
#
# Objetivo 1: detección de melds válidos.
#
#   - es_run(meld):            ¿es una escalera? (3+ fichas consecutivas,
#                              mismo color; los jokers sustituyen números).
#   - es_group(meld):          ¿es un grupo? (3-4 fichas del mismo número,
#                              colores distintos; los jokers sustituyen
#                              colores).
#   - es_meld_valido(meld):    run o group válido.
#   - asignar_run(meld):       resuelve los jokers de un run (asignación
#                              canónica de número/color).
#   - asignar_group(meld):     resuelve los jokers de un group.
#   - suma_meld(meld):         puntos del meld. Los jokers puntúan el valor
#                              de la ficha que representan (ver nota).
#   - cumple_regla_30(melds):  uno o más melds de la jugada inicial suman >= 30.
#
# Entrada típica: un data.frame con columnas `number` y `colour`
# (ver 00_pool_fichas.R).
#
# NOTA sobre jokers y puntos: un run con jokers admite varias asignaciones
# válidas (ej. {5, joker, joker} puede ser 3-4-5, 4-5-6 o 5-6-7) y la suma
# cambia. `suma_meld()` usa la asignación canónica de `asignar_run()`:
# primero rellena los huecos internos y después extiende por la derecha.

# Carga 00_pool_fichas.R buscándolo en scripts/ desde el directorio de
# trabajo hacia arriba (funciona aunque el wd no sea la raíz del proyecto,
# p.ej. al ejecutar los tests).
source_local <- function(rel) {
  dir_actual <- normalizePath(getwd(), winslash = "/")
  repeat {
    candidato <- file.path(dir_actual, "scripts", rel)
    if (file.exists(candidato)) {
      source(candidato)
      return(invisible(TRUE))
    }
    padre <- dirname(dir_actual)
    if (padre == dir_actual) break
    dir_actual <- padre
  }
  stop("No se encuentra scripts/", rel, " desde ", getwd())
}

source_local("00_pool_fichas.R")

# --- Runs (escaleras) ----------------------------------------------

es_run <- function(meld) {
  n <- nrow(meld)
  if (n < 3) return(FALSE)
  nums <- meld$number
  cols <- as.character(meld$colour)
  wilds <- is.na(nums)
  if (all(wilds)) return(FALSE)             # solo jokers: sin números reales
  if (length(unique(cols[!wilds])) > 1) return(FALSE)  # colores distintos
  reales <- sort(nums[!wilds])
  if (anyDuplicated(reales)) return(FALSE)  # números repetidos en un run
  j <- sum(wilds)
  k <- length(reales)
  if (k + j > 13) return(FALSE)             # un run no supera 13 fichas
  huecos <- (max(reales) - min(reales) + 1) - k
  j >= huecos
}

asignar_run <- function(meld) {
  if (!es_run(meld)) stop("El meld no es un run válido")
  nums <- meld$number
  cols <- as.character(meld$colour)
  wilds <- is.na(nums)
  col <- unique(cols[!wilds])
  reales <- sort(nums[!wilds])
  j <- sum(wilds)
  lo <- min(reales)
  hi <- max(reales)

  huecos <- (hi - lo + 1) - length(reales)
  sobrantes <- j - huecos
  r_ext <- min(sobrantes, 13 - hi)          # extiende a la derecha
  l_ext <- sobrantes - r_ext                # y lo que falte a la izquierda

  izq <- if (l_ext > 0) (lo - l_ext):(lo - 1) else integer(0)
  der <- if (r_ext > 0) (hi + 1):(hi + r_ext) else integer(0)

  numeros <- c(izq, lo:hi, der)
  data.frame(number = numeros, colour = rep(col, length(numeros)))
}

# --- Groups (grupos) ------------------------------------------------

es_group <- function(meld) {
  n <- nrow(meld)
  if (n < 3 || n > 4) return(FALSE)
  nums <- meld$number
  cols <- as.character(meld$colour)
  wilds <- is.na(nums)
  if (all(wilds)) return(FALSE)             # solo jokers: sin número real
  reales_num <- nums[!wilds]
  reales_col <- cols[!wilds]
  if (length(unique(reales_num)) > 1) return(FALSE)  # números distintos
  if (anyDuplicated(reales_col)) return(FALSE)       # colores repetidos
  if (any(!(reales_col %in% COLORES))) return(FALSE)
  TRUE
}

asignar_group <- function(meld) {
  if (!es_group(meld)) stop("El meld no es un group válido")
  nums <- meld$number
  cols <- as.character(meld$colour)
  wilds <- is.na(nums)
  num <- unique(nums[!wilds])
  colores_reales <- cols[!wilds]
  colores_joker <- setdiff(COLORES, colores_reales)[seq_len(sum(wilds))]
  data.frame(
    number = rep(num, nrow(meld)),
    colour = c(colores_reales, colores_joker)
  )
}

# --- Genérico -------------------------------------------------------

es_meld_valido <- function(meld) {
  es_run(meld) || es_group(meld)
}

# Puntos del meld (jokers con su valor canónico).
suma_meld <- function(meld) {
  if (es_run(meld)) {
    sum(asignar_run(meld)$number)
  } else if (es_group(meld)) {
    sum(asignar_group(meld)$number)
  } else {
    stop("El meld no es válido")
  }
}

# La jugada inicial: suma de los melds jugados >= 30.
# `melds` puede ser un único meld o una lista de melds.
cumple_regla_30 <- function(melds) {
  if (is.data.frame(melds)) melds <- list(melds)
  sum(vapply(melds, suma_meld, numeric(1))) >= 30
}
