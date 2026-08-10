# 02_probabilidad_pesca.R
#
# Objetivo 2: probabilidad de pescar una ficha útil.
#
# Contexto: conocemos nuestra mano y las fichas visibles en la mesa; el
# contrincante (2 jugadores) ha cogido `n_contrincante` fichas desconocidas.
#
# Modelo: muestreo sin reemplazo. El pool "disponible" es el pool completo
# menos las fichas que vemos (mano + mesa). Las fichas del contrincante son
# desconocidas: solo reducen el número de fichas restantes.
#
# RESULTADO CLAVE (intercambiabilidad): para UNA pesca, la probabilidad de
# que la ficha extraída sea útil NO depende de `n_contrincante`. La ficha
# que pescamos es uniforme sobre el pool disponible (mano+mesa fuera), con
# o sin las fichas del contrincante. `n_contrincante` solo reduce el pool
# restante (importa para varias pescas, no para una).
#
# "Útil" es un criterio configurable (`es_util`). Por defecto:
#   - joker: siempre útil
#   - group: mismo número que una ficha de la mano, con color distinto
#   - run:   mismo color que una ficha de la mano y número consecutivo (±1)
# (heurística de "mejora la mano", no de "completa un meld").

# Carga 00_pool_fichas.R buscándolo en scripts/ desde el directorio de
# trabajo hacia arriba (funciona aunque el wd no sea la raíz del proyecto).
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

# --- Pool disponible ------------------------------------------------
# Devuelve inventario (number, colour, n); atributo `total_disponible`
# = fichas que quedan tras quitar también las del contrincante.
pool_disponible <- function(mano = NULL, mesa = NULL, n_contrincante = 0) {
  if (is.null(mano)) mano <- fichas(integer(0), character(0))
  if (is.null(mesa)) mesa <- fichas(integer(0), character(0))
  contador <- contar_fichas(pool_fichas())
  contador <- restar_fichas(contador, contar_fichas(mano))
  contador <- restar_fichas(contador, contar_fichas(mesa))
  total <- sum(contador$n)
  if (n_contrincante < 0 || n_contrincante > total) {
    stop("n_contrincante debe estar entre 0 y ", total)
  }
  attr(contador, "total_disponible") <- total - n_contrincante
  contador
}

# --- Criterio de ficha útil (por defecto) ---------------------------

# `ficha`: data.frame de 1 fila con columnas number, colour.
# `mano`:  data.frame con columnas number, colour.
es_util_ficha <- function(ficha, mano) {
  if (es_joker(ficha$number, ficha$colour)) return(TRUE)
  if (nrow(mano) == 0) return(FALSE)
  num <- ficha$number
  col <- ficha$colour
  h_num <- mano$number
  h_col <- mano$colour

  # group: mismo número, color distinto al de la mano
  mismo_num <- !is.na(h_num) & h_num == num
  if (any(mismo_num) && !(col %in% unique(h_col[mismo_num]))) return(TRUE)

  # run: mismo color y número consecutivo
  mismo_col <- h_col == col & !is.na(h_num)
  if (any(abs(h_num[mismo_col] - num) == 1)) return(TRUE)

  FALSE
}

# --- Probabilidad ----------------------------------------------------

# Tipos de fichas disponibles en el pool que `es_util` considera útiles.
fichas_utiles <- function(mano = NULL, mesa = NULL, n_contrincante = 0,
                          es_util = es_util_ficha) {
  if (is.null(mano)) mano <- fichas(integer(0), character(0))
  disp <- pool_disponible(mano, mesa, n_contrincante)
  util <- vapply(seq_len(nrow(disp)), function(i) {
    es_util(disp[i, c("number", "colour"), drop = FALSE], mano)
  }, logical(1))
  disp[util, , drop = FALSE]
}

# P(pescar una ficha útil) = nº de copias útiles / nº de copias disponibles.
prob_pesca_util <- function(mano = NULL, mesa = NULL, n_contrincante = 0,
                            es_util = es_util_ficha) {
  if (is.null(mano)) mano <- fichas(integer(0), character(0))
  disp <- pool_disponible(mano, mesa, n_contrincante)
  total <- sum(disp$n)
  if (total == 0) return(NA_real_)
  util <- vapply(seq_len(nrow(disp)), function(i) {
    es_util(disp[i, c("number", "colour"), drop = FALSE], mano)
  }, logical(1))
  sum(disp$n[util]) / total
}
