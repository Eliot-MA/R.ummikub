# tests/test_objetivo_04.R
#
# Pruebas del objetivo 4 (reestructuración del tablero).
# Ejecutar con:
#   Rscript -e "testthat::test_file('tests/test_objetivo_04.R')"

source(file.path(dirname(testthat::test_path()), "..", "scripts",
                 "04_reestructuracion_tablero.R"))

# source("scripts/04_reestructuracion_tablero.R")

library(testthat)

# Busca la jugada cuya descripción contiene `patron`.
encontrar_jugada <- function(jugadas, patron) {
  for (j in jugadas) {
    if (grepl(patron, j$desc)) return(j)
  }
  NULL
}

test_that("añadir ficha de la mano a un run por ambos extremos", {
  tablero <- list(fichas(c(3, 4, 5), c("red", "red", "red")))
  mano <- fichas(c(2, 6, 9), c("red", "red", "red"))
  jugadas <- buscar_jugadas(tablero, mano)

  j_izq <- encontrar_jugada(jugadas, "Añadir 2 red")
  j_der <- encontrar_jugada(jugadas, "Añadir 6 red")
  expect_false(is.null(j_izq))
  expect_false(is.null(j_der))
  # 9 red no puede añadirse (queda un hueco)
  expect_null(encontrar_jugada(jugadas, "Añadir 9 red"))

  # Aplicar la jugada de la derecha
  expect_true(verificar_tablero(j_der$tablero))
  expect_equal(nrow(j_der$mano), 2)               # quedan 2 fichas
  expect_equal(sort(j_der$mano$number), c(2, 9))
  expect_equal(sort(j_der$tablero[[1]]$number), 3:6)
})

test_that("añadir ficha de la mano a un group", {
  tablero <- list(fichas(c(7, 7, 7), c("red", "blue", "yellow")))
  mano <- fichas(c(7, 7), c("black", "red"))
  jugadas <- buscar_jugadas(tablero, mano)

  j_negro <- encontrar_jugada(jugadas, "Añadir 7 black")
  expect_false(is.null(j_negro))
  # 7 red ya está en el group: no puede añadirse
  expect_null(encontrar_jugada(jugadas, "Añadir 7 red"))

  expect_true(verificar_tablero(j_negro$tablero))
  expect_equal(nrow(j_negro$tablero[[1]]), 4)
  expect_equal(nrow(j_negro$mano), 1)
})

test_that("añadir un joker de la mano", {
  tablero <- list(fichas(c(3, 4, 5), c("red", "red", "red")))
  mano <- fichas(NA, "wild")
  jugadas <- buscar_jugadas(tablero, mano)
  j <- encontrar_jugada(jugadas, "Añadir J")
  expect_false(is.null(j))
  expect_equal(nrow(j$mano), 0)
  expect_true(verificar_tablero(j$tablero))
})

test_that("un joker de la mesa nunca vuelve a la mano", {
  # Con un solo meld, el joker no tiene dónde ir: no hay jugada de
  # intercambio y ninguna jugada devuelve el joker a la mano.
  tablero <- list(fichas(c(3, NA, 5), c("red", "wild", "red")))  # joker = 4
  mano <- fichas(c(4, 8), c("red", "blue"))
  jugadas <- buscar_jugadas(tablero, mano)
  expect_null(encontrar_jugada(jugadas, "Sustituir el joker"))
  for (j in jugadas) expect_false(any(is.na(j$mano$number)))
})

tablero <- list(
  fichas(c(3, NA, 5), c("red", "wild", "red")),      # joker = 4
  fichas(c(1, 2, 3), c("blue", "blue", "blue"))      # destino del joker
)
mano <- fichas(4, "red")
jugadas <- buscar_jugadas(tablero, mano)

test_that("intercambiar joker: sustituir y mover el joker a otro meld (run)", {
  tablero <- list(
    fichas(c(3, NA, 5), c("red", "wild", "red")),      # joker = 4
    fichas(c(1, 2, 3), c("blue", "blue", "blue"))      # destino del joker
  )
  mano <- fichas(4, "red")
  jugadas <- buscar_jugadas(tablero, mano)

  j <- encontrar_jugada(jugadas,
                        "Sustituir el joker del meld 1 por 4 red y mover el joker al meld 2")
  expect_false(is.null(j))
  expect_true(verificar_tablero(j$tablero))
  # El joker queda en la mesa (meld 2), no en la mano
  expect_equal(sort(j$tablero[[1]]$number), 3:5)
  expect_false(any(is.na(j$tablero[[1]]$number)))
  expect_true(any(is.na(j$tablero[[2]]$number)))
  expect_equal(nrow(j$mano), 0)
})

test_that("intercambiar joker en un group (con otro meld de destino)", {
  tablero <- list(
    fichas(c(NA, 7, 7), c("wild", "red", "blue")),     # joker = 7
    fichas(c(1, 2, 3), c("blue", "blue", "blue"))      # destino del joker
  )
  mano <- fichas(c(7, 7), c("black", "yellow"))
  jugadas <- buscar_jugadas(tablero, mano)

  j <- encontrar_jugada(jugadas,
                        "Sustituir el joker del meld 1 por 7 black y mover el joker al meld 2")
  expect_false(is.null(j))
  expect_true(verificar_tablero(j$tablero))
  expect_false(any(is.na(j$tablero[[1]]$number)))       # group completo
  expect_true(any(is.na(j$tablero[[2]]$number)))         # joker en el meld 2
  expect_equal(nrow(j$mano), 1)                          # queda 7 yellow
  expect_false(any(is.na(j$mano$number)))                # sin joker en la mano
})

test_that("trasladar una ficha de un run a un group", {
  tablero <- list(
    fichas(c(3, 4, 5, 6), c("red", "red", "red", "red")),
    fichas(c(6, 6, 6), c("blue", "yellow", "black"))
  )
  mano <- fichas(integer(0), character(0))
  jugadas <- buscar_jugadas(tablero, mano)

  j <- encontrar_jugada(jugadas, "Mover 6 red del meld 1 al meld 2")
  expect_false(is.null(j))
  expect_true(verificar_tablero(j$tablero))
  expect_equal(sort(j$tablero[[1]]$number), 3:5)
  expect_equal(sort(j$tablero[[2]]$number), rep(6, 4))
})

test_that("dividir un run y añadir ficha de la mano", {
  tablero <- list(fichas(2:7, rep("red", 6)))
  mano <- fichas(c(8, 1), c("red", "red"))
  jugadas <- buscar_jugadas(tablero, mano)

  j <- encontrar_jugada(jugadas, "Dividir el meld 1")
  expect_false(is.null(j))
  expect_true(verificar_tablero(j$tablero))
  # ahora hay dos melds de 4 y 3 fichas
  tamanos <- sort(vapply(j$tablero, nrow, integer(1)))
  expect_equal(tamanos, c(3, 4))
  expect_equal(nrow(j$mano), 1)
})

test_that("sin jugadas posibles devuelve lista vacía", {
  tablero <- list(fichas(c(3, 4, 5), c("red", "red", "red")))
  mano <- fichas(c(9, 12), c("red", "blue"))
  expect_equal(length(buscar_jugadas(tablero, mano)), 0)
})

test_that("buscar_jugadas deduplica", {
  tablero <- list(fichas(c(3, 4, 5), c("red", "red", "red")))
  mano <- fichas(6, "red")
  descs <- descripcion_jugadas(tablero, mano)
  expect_equal(length(descs), length(unique(descs)))
})

test_that("las jugadas que usan la mano quitan la ficha de la mano", {
  tablero <- list(
    fichas(c(3, 4, 5), c("red", "red", "red")),
    fichas(c(7, 7, 7), c("blue", "yellow", "black"))
  )
  mano <- fichas(c(6, 7, 1), c("red", "red", "blue"))
  jugadas <- buscar_jugadas(tablero, mano)
  expect_true(length(jugadas) >= 1)
  for (j in jugadas) {
    expect_true(verificar_tablero(j$tablero))
  }
})

test_that("buscar_jugadas con data.frame sin meld_id da error claro", {
  tablero <- data.frame(number = c(3, 4, 5), colour = c("red", "red", "red"))
  expect_error(buscar_jugadas(tablero, fichas(6, "red")), "meld_id")
})

test_that("buscar_jugadas acepta data.frame con meld_id", {
  tablero_df <- data.frame(
    meld_id = c(1, 1, 1),
    number = c(3, 4, 5),
    colour = c("red", "red", "red")
  )
  jugadas <- buscar_jugadas(tablero_df, fichas(6, "red"))
  expect_true(length(jugadas) >= 1)
})
