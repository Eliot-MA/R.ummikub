# 04_reestructuracion_tablero.R
#
# Objetivo 4: reorganizar los melds de la mesa para colocar fichas de la mano.
#
# En Rummikub, en turnos posteriores se pueden romper y reordenar los combos
# de la mesa siempre que, al acabar el turno, todo siga siendo válido. Aquí
# modelamos los movimientos atómicos sobre un tablero:
#
#   1. `añadir_a_meld()`: colocar UNA ficha (de la mano o de otro meld) en un
#      meld existente (extender un run por sus extremos, rellenar un colour
#      libre de un group, o usar un joker).
#   2. `mover_añadir_ficha()`: jugadas que colocan una ficha de la mano.
#   3. `mover_intercambiar_joker()`: sustituir un joker de la mesa por la
#      ficha real que representa y mover el joker liberado a OTRO meld.
#      Regla: un wild nunca vuelve a la mano (solo mano -> tablero o
#      tablero -> tablero).
#   4. `mover_trasladar_ficha()`: mover una ficha de un meld a otro (deja el
#      meld de origen válido y el de destino válido). No coloca ficha de la
#      mano: sirve de preparación para otras jugadas.
#   5. `mover_dividir_y_añadir()`: dividir un run en dos runs válidos y añadir
#      una ficha de la mano a una de las partes.
#   6. `buscar_jugadas()`: reúne todas las jugadas anteriores y las devuelve
#      deduplicadas. Cada jugada es una lista con:
#        $tablero  -> tablero resultante (lista de melds)
#        $mano     -> mano resultante (data.frame)
#        $desc     -> descripción legible
#
# Todas las jugadas se validan con `verificar_tablero()` (objetivo 3).

# Carga de scripts compartidos (búsqueda en scripts/ hacia arriba desde el wd).
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
source_local("01_melds_validos.R")
source_local("03_verificador_tablero.R")

# --- Utilidades -----------------------------------------------------

etiqueta_ficha <- function(ficha) {
  if (es_joker(ficha$number, ficha$colour)) {
    "J"
  } else {
    paste(ficha$number, ficha$colour)
  }
}

tipo_meld <- function(meld) {
  if (es_run(meld)) return("run")
  if (es_group(meld)) return("group")
  "invalido"
}

# Melds resueltos (jokers con su valor canónico), o NULL si no es válido.
meld_resuelto <- function(meld) {
  if (es_run(meld)) return(asignar_run(meld))
  if (es_group(meld)) return(asignar_group(meld))
  NULL
}

movimiento <- function(tablero, mano, desc) {
  list(tablero = tablero, mano = mano, desc = desc)
}

# Sustituye el meld `m` de `tablero` por los melds de `nuevos` (1 o más).
reemplazar_meld <- function(tablero, m, nuevos) {
  if (length(nuevos) == 1) {
    tablero[[m]] <- nuevos[[1]]
    return(tablero)
  }
  c(tablero[seq_len(m - 1)],
    nuevos,
    if (m < length(tablero)) tablero[(m + 1):length(tablero)])
}

# --- Añadir una ficha a un meld -------------------------------------

# Intenta colocar `ficha` (1 fila) en `meld`. Devuelve el meld resultante
# si es válido, o NULL si no se puede.
añadir_a_meld <- function(meld, ficha) {
  tipo <- tipo_meld(meld)
  if (tipo == "invalido") return(NULL)
  res <- meld_resuelto(meld)
  nuevo <- rbind(meld, ficha)
  rownames(nuevo) <- NULL

  if (es_joker(ficha$number, ficha$colour)) {
    # Un joker puede extenderse en un run o rellenar un color en un group.
    limite <- if (tipo == "run") 13 else 4
    if (nrow(nuevo) <= limite && es_meld_valido(nuevo)) return(nuevo)
    return(NULL)
  }

  if (tipo == "run") {
    if (ficha$colour != unique(res$colour)) return(NULL)
    num <- ficha$number
    if (num < 1 || num > 13) return(NULL)
    if (num == min(res$number) - 1 || num == max(res$number) + 1) {
      if (es_meld_valido(nuevo)) return(nuevo)
    }
    return(NULL)
  }

  # group
  if (ficha$number != unique(res$number)) return(NULL)
  if (ficha$colour %in% res$colour) return(NULL)
  if (nrow(nuevo) <= 4 && es_meld_valido(nuevo)) return(nuevo)
  NULL
}

# --- Jugadas --------------------------------------------------------

# Colocar una ficha de la mano en un meld existente.
mover_añadir_ficha <- function(tablero, mano) {
  if (nrow(mano) == 0) return(list())
  movs <- list()
  for (h in seq_len(nrow(mano))) {
    ficha <- mano[h, , drop = FALSE]
    for (m in seq_along(tablero)) {
      nuevo_meld <- añadir_a_meld(tablero[[m]], ficha)
      if (is.null(nuevo_meld)) next
      tablero2 <- tablero
      tablero2[[m]] <- nuevo_meld
      mano2 <- mano[-h, , drop = FALSE]
      if (verificar_tablero(tablero2)) {
        movs <- append(movs, list(movimiento(
          tablero = tablero2, mano = mano2,
          desc = sprintf("Añadir %s al meld %d", etiqueta_ficha(ficha), m)
        )))
      }
    }
  }
  movs
}

# Sustituir un joker de la mesa por una ficha real de la mano y colocar el
# joker liberado en OTRO meld de la mesa. El joker nunca pasa a la mano.
mover_intercambiar_joker <- function(tablero, mano) {
  if (nrow(mano) == 0) return(list())
  movs <- list()
  for (m in seq_along(tablero)) {
    meld <- tablero[[m]]
    pos_jokers <- which(is.na(meld$number))
    if (length(pos_jokers) == 0) next
    for (p in pos_jokers) {
      joker <- meld[p, , drop = FALSE]
      for (h in seq_len(nrow(mano))) {
        ficha <- mano[h, , drop = FALSE]
        if (es_joker(ficha$number, ficha$colour)) next
        meld_sin_joker <- rbind(meld[-p, , drop = FALSE], ficha)
        rownames(meld_sin_joker) <- NULL
        if (!es_meld_valido(meld_sin_joker)) next
        for (m2 in seq_along(tablero)) {
          if (m2 == m) next
          meld_destino <- añadir_a_meld(tablero[[m2]], joker)
          if (is.null(meld_destino)) next
          tablero2 <- tablero
          tablero2[[m]] <- meld_sin_joker
          tablero2[[m2]] <- meld_destino
          mano2 <- mano[-h, , drop = FALSE]
          if (verificar_tablero(tablero2)) {
            movs <- append(movs, list(movimiento(
              tablero = tablero2, mano = mano2,
              desc = sprintf(
                "Sustituir el joker del meld %d por %s y mover el joker al meld %d",
                m, etiqueta_ficha(ficha), m2)
            )))
          }
        }
      }
    }
  }
  movs
}

# Índices de las fichas de `meld` que pueden retirarse dejando un meld válido.
fichas_trasladables <- function(meld) {
  if (nrow(meld) < 4) return(integer(0))
  res <- integer(0)
  for (i in seq_len(nrow(meld))) {
    resto <- meld[-i, , drop = FALSE]
    if (nrow(resto) >= 3 && es_meld_valido(resto)) res <- c(res, i)
  }
  res
}

# Mover una ficha de un meld a otro. No coloca fichas de la mano.
mover_trasladar_ficha <- function(tablero, mano) {
  movs <- list()
  for (m in seq_along(tablero)) {
    for (idx in fichas_trasladables(tablero[[m]])) {
      ficha <- tablero[[m]][idx, , drop = FALSE]
      resto <- tablero[[m]][-idx, , drop = FALSE]
      for (m2 in seq_along(tablero)) {
        if (m2 == m) next
        nuevo <- añadir_a_meld(tablero[[m2]], ficha)
        if (is.null(nuevo)) next
        tablero2 <- tablero
        tablero2[[m]] <- resto
        tablero2[[m2]] <- nuevo
        if (verificar_tablero(tablero2)) {
          movs <- append(movs, list(movimiento(
            tablero = tablero2, mano = mano,
            desc = sprintf("Mover %s del meld %d al meld %d",
                           etiqueta_ficha(ficha), m, m2)
          )))
        }
      }
    }
  }
  movs
}

# Divisiones de un run (sin jokers) en dos runs válidos de >= 3 fichas.
dividir_run <- function(meld) {
  if (tipo_meld(meld) != "run" || any(is.na(meld$number))) return(list())
  nums <- sort(meld$number)
  col <- unique(meld$colour)
  n <- length(nums)
  if (n < 6) return(list())
  splits <- list()
  for (k in 3:(n - 3)) {
    splits <- append(splits, list(list(
      parte1 = fichas(nums[seq_len(k)], rep(col, k)),
      parte2 = fichas(nums[(k + 1):n], rep(col, n - k)),
      corte = nums[k]
    )))
  }
  splits
}

# Dividir un run en dos y añadir una ficha de la mano a una de las partes.
mover_dividir_y_añadir <- function(tablero, mano) {
  if (nrow(mano) == 0) return(list())
  movs <- list()
  for (m in seq_along(tablero)) {
    splits <- dividir_run(tablero[[m]])
    if (length(splits) == 0) next
    for (sp in splits) {
      for (h in seq_len(nrow(mano))) {
        ficha <- mano[h, , drop = FALSE]
        for (parte in c("parte1", "parte2")) {
          otra <- if (parte == "parte1") "parte2" else "parte1"
          ext <- añadir_a_meld(sp[[parte]], ficha)
          if (is.null(ext)) next
          tablero2 <- reemplazar_meld(tablero, m, list(ext, sp[[otra]]))
          mano2 <- mano[-h, , drop = FALSE]
          if (verificar_tablero(tablero2)) {
            movs <- append(movs, list(movimiento(
              tablero = tablero2, mano = mano2,
              desc = sprintf("Dividir el meld %d tras %d y añadir %s a %s",
                             m, sp$corte, etiqueta_ficha(ficha), parte)
            )))
          }
        }
      }
    }
  }
  movs
}

# Todas las jugadas posibles, deduplicadas por descripción.
# `tablero` puede ser una lista de melds o un data.frame con `meld_id`.
buscar_jugadas <- function(tablero, mano) {
  tablero <- tablero_a_lista(tablero)
  movs <- list(
    mover_añadir_ficha(tablero, mano),
    mover_intercambiar_joker(tablero, mano),
    mover_trasladar_ficha(tablero, mano),
    mover_dividir_y_añadir(tablero, mano)
  )
  movs <- unlist(movs, recursive = FALSE)
  descs <- vapply(movs, function(x) x$desc, character(1))
  movs[!duplicated(descs)]
}

# Descripciones de las jugadas (conveniente para depurar).
descripcion_jugadas <- function(tablero, mano) {
  vapply(buscar_jugadas(tablero, mano), function(x) x$desc, character(1))
}
