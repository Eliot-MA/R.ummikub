# 05_visualizacion_tablero.R
#
# Objetivo 5: representación del estado del tablero.
#
#   - `tablero_a_datos()`: data.frame listo para ver/plotear, con una fila
#     por ficha: meld, posicion (orden dentro del meld), number, colour y
#     etiqueta (número, o "J" para joker). En los runs se ordena por número;
#     en los groups, por color.
#   - `graficar_tablero()`: plot con ggplot2, una celda por ficha coloreada
#     por colour y con la etiqueta encima. Devuelve el objeto ggplot.
#   - `mostrar_tablero()`: impresión legible por consola.
#
# Representación del tablero: lista de melds o data.frame con `meld_id`
# (la misma de 03/04).

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

# --- Orden y datos --------------------------------------------------

# Ordena las fichas de un meld: runs por número (jokers al final), groups
# por orden de color.
ordenar_meld <- function(meld) {
  if (es_run(meld)) {
    meld[order(is.na(meld$number), meld$number), , drop = FALSE]
  } else if (es_group(meld)) {
    meld[order(match(meld$colour, COLORES)), , drop = FALSE]
  } else {
    meld
  }
}

# Tablero como data.frame de una fila por ficha.
tablero_a_datos <- function(tablero) {
  melds <- tablero_a_lista(tablero)
  if (length(melds) == 0) {
    return(data.frame(meld = integer(0), posicion = integer(0),
                      number = integer(0), colour = character(0),
                      etiqueta = character(0)))
  }
  filas <- lapply(seq_along(melds), function(m) {
    meld <- ordenar_meld(melds[[m]])
    data.frame(
      meld = m,
      posicion = seq_len(nrow(meld)),
      number = meld$number,
      colour = meld$colour,
      etiqueta = ifelse(is.na(meld$number), "J", as.character(meld$number)),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, filas)
}

# --- Impresión y gráfico --------------------------------------------

mostrar_tablero <- function(tablero) {
  melds <- tablero_a_lista(tablero)
  if (length(melds) == 0) {
    cat("Tablero vacío\n")
    return(invisible(NULL))
  }
  for (m in seq_along(melds)) {
    meld <- ordenar_meld(melds[[m]])
    tipo <- if (es_run(meld)) "run" else if (es_group(meld)) "group" else "?"
    etiquetas <- ifelse(is.na(meld$number), "J", as.character(meld$number))
    cat(sprintf("Meld %d (%s): %s\n", m, tipo,
                paste(paste(etiquetas, meld$colour), collapse = " | ")))
  }
  invisible(NULL)
}

graficar_tablero <- function(tablero, titulo = "Tablero de Rummikub") {
  datos <- tablero_a_datos(tablero)
  if (nrow(datos) == 0) {
    warning("El tablero está vacío: no hay nada que dibujar")
    return(invisible(NULL))
  }
  paleta <- c(red = "#d64545", blue = "#3b6fd4", yellow = "#f2c94c",
              black = "#3a3a3a", wild = "#9e9e9e")
  color_texto <- c(red = "black", blue = "white", yellow = "black",
                   black = "white", wild = "white")
  datos$relleno <- paleta[datos$colour]
  datos$color_texto <- color_texto[datos$colour]

  ggplot2::ggplot(datos, ggplot2::aes(x = posicion, y = factor(meld))) +
    ggplot2::geom_tile(fill = datos$relleno, colour = "grey25",
                       width = 0.92, height = 0.92) +
    ggplot2::geom_text(ggplot2::aes(label = etiqueta, colour = color_texto),
                       size = 5) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_y_discrete(limits = rev(sort(as.character(unique(datos$meld))))) +
    ggplot2::coord_fixed(ratio = 1, xlim = c(0.5, max(datos$posicion) + 0.5)) +
    ggplot2::labs(title = titulo, x = "Posición", y = "Meld") +
    ggplot2::theme_minimal()
}
