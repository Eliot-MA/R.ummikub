# 03_verificador_tablero.R
#
# Objetivo 3: verificar que el tablero solo contiene combinaciones válidas.
#
# Un tablero es una colección de melds. `verificar_tablero()` comprueba:
#   1. Cada meld es un run o un group válido (reutiliza 01_melds_validos.R).
#   2. Las fichas son físicamente posibles: cada tipo (number, colour)
#      aparece como máximo las veces que existen en el pool (2 copias por
#      tipo numerado, 2 jokers). Esto también rechaza fichas que no existen
#      (ej. un 14) o más copias de las disponibles repartidas entre melds.
#
# Representación del tablero (compartida con 04):
#   - lista de melds, donde cada meld es un data.frame con columnas
#     `number` y `colour` (ver 00_pool_fichas.R), o
#   - un data.frame con una columna extra `meld_id` que indica a qué meld
#     pertenece cada ficha.

# Carga 00 y 01 buscándolos en scripts/ desde el directorio de trabajo
# hacia arriba (funciona aunque el wd no sea la raíz del proyecto).
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

# --- Representación del tablero -------------------------------------

# Convierte un tablero (lista o data.frame con meld_id) a lista de melds.
tablero_a_lista <- function(tablero) {
  if (is.data.frame(tablero)) {
    if (!"meld_id" %in% names(tablero)) {
      stop("Si el tablero es un data.frame debe tener columna `meld_id`")
    }
    lista <- split(tablero[c("number", "colour")], tablero$meld_id)
    lapply(unname(lista), function(x) { rownames(x) <- NULL; x })
  } else if (is.list(tablero)) {
    lapply(tablero, function(meld) {
      if (!is.data.frame(meld) ||
          !all(c("number", "colour") %in% names(meld))) {
        stop("Cada meld debe ser un data.frame con columnas number y colour")
      }
      meld[c("number", "colour")]
    })
  } else {
    stop("El tablero debe ser una lista de melds o un data.frame con meld_id")
  }
}

# Convierte una lista de melds a data.frame con columna `meld_id`.
tablero_a_dataframe <- function(lista) {
  if (length(lista) == 0) {
    return(data.frame(meld_id = integer(0), number = integer(0),
                      colour = character(0)))
  }
  lista <- lapply(seq_along(lista), function(i) {
    cbind(meld_id = i, lista[[i]])
  })
  do.call(rbind, lista)
}

# --- Comprobaciones -------------------------------------------------

# Las fichas del tablero deben poder existir en el pool físico.
# Devuelve list(ok, problemas).
verificar_fisico <- function(fichas_tablero) {
  if (nrow(fichas_tablero) == 0) {
    return(list(ok = TRUE, problemas = character(0)))
  }
  conteo <- contar_fichas(fichas_tablero)
  pool_c <- contar_fichas(pool_fichas())
  problemas <- character(0)
  for (i in seq_len(nrow(conteo))) {
    num_i <- conteo$number[i]
    col_i <- conteo$colour[i]
    coincide <- (pool_c$colour == col_i) &
      ((is.na(pool_c$number) & is.na(num_i)) |
         (!is.na(pool_c$number) & !is.na(num_i) & pool_c$number == num_i))
    maximo <- if (any(coincide)) pool_c$n[which(coincide)[1]] else 0
    if (conteo$n[i] > maximo) {
      etiqueta <- if (is.na(num_i)) "joker" else num_i
      problemas <- c(problemas, sprintf(
        "La ficha %s %s aparece %d veces (máximo %d en el pool)",
        etiqueta, col_i, conteo$n[i], maximo
      ))
    }
  }
  list(ok = length(problemas) == 0, problemas = problemas)
}

# Comprueba el tablero entero. Devuelve TRUE/FALSE, o con `detalle = TRUE`
# una lista con `valido` y `problemas` (mensajes).
verificar_tablero <- function(tablero, detalle = FALSE) {
  melds <- tablero_a_lista(tablero)
  problemas <- character(0)

  for (i in seq_along(melds)) {
    if (!es_meld_valido(melds[[i]])) {
      problemas <- c(problemas, sprintf("El meld %d no es una combinación válida", i))
    }
  }

  if (length(melds) > 0) {
    todas <- do.call(rbind, melds)
    fisico <- verificar_fisico(todas)
    problemas <- c(problemas, fisico$problemas)
  }

  if (detalle) {
    return(list(valido = length(problemas) == 0, problemas = problemas))
  }
  length(problemas) == 0
}
