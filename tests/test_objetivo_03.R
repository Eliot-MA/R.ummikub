# tests/test_objetivo_03.R
#
# Pruebas del objetivo 3 (verificador de tablero).
# Ejecutar con:
#   Rscript -e "testthat::test_file('tests/test_objetivo_03.R')"

source(file.path(dirname(testthat::test_path()), "..", "scripts",
                 "03_verificador_tablero.R"))

#source("scripts/03_verificador_tablero.R")

library(testthat)

# --- Tableros válidos -----------------------------------------------

test_that("tablero vacío es válido", {
  expect_true(verificar_tablero(list()))
  expect_true(verificar_tablero(tablero_a_dataframe(list())))
})

test_that("un run válido es válido", {
  tablero <- list(fichas(c(3, 4, 5), c("red", "red", "red")))
  expect_true(verificar_tablero(tablero))
})


test_that("varios melds válidos (run + group)", {
  tablero <- list(
    fichas(c(3, 4, 5), c("red", "red", "red")),
    fichas(c(7, 7, 7), c("blue", "yellow", "black"))
  )
  expect_true(verificar_tablero(tablero))
})

test_that("las 2 copias físicas de una ficha pueden estar en la mesa", {
  tablero <- list(
    fichas(c(4, 5, 6), c("red", "red", "red")),          # un 5 rojo
    fichas(c(5, 5, 5), c("red", "blue", "yellow"))       # otro 5 rojo
  )
  expect_true(verificar_tablero(tablero))
})

test_that("los 2 jokers pueden estar en la mesa", {
  tablero <- list(
    fichas(c(3, NA, 5), c("red", "wild", "red")),   # joker en un run
    fichas(c(8, 8, NA), c("blue", "yellow", "wild")) # joker en un group
  )
  expect_true(verificar_tablero(tablero))
})

# --- Tableros inválidos ---------------------------------------------

test_that("un meld con 2 fichas es inválido", {
  tablero <- list(fichas(c(3, 4), c("red", "red")))
  expect_false(verificar_tablero(tablero))
})

test_that("un run no consecutivo es inválido", {
  tablero <- list(fichas(c(2, 4, 6), c("red", "red", "red")))
  expect_false(verificar_tablero(tablero))
})

test_that("una ficha inexistente (14) es inválida", {
  tablero <- list(fichas(c(12, 13, 14), c("red", "red", "red")))
  expect_false(verificar_tablero(tablero))
})

test_that("3 copias de un 5 rojo (solo hay 2 físicas) es inválido", {
  tablero <- list(
    fichas(c(4, 5, 6), c("red", "red", "red")),
    fichas(c(5, 5, 5), c("red", "blue", "yellow")),
    fichas(c(5, 6, 7), c("red", "red", "red"))
  )
  expect_false(verificar_tablero(tablero))
})

test_that("3 jokers (solo hay 2) es inválido", {
  tablero <- list(
    fichas(c(1, NA, 3), c("blue", "wild", "blue")),
    fichas(c(2, NA, 4), c("red", "wild", "red")),
    fichas(c(NA, 5, 6), c("wild", "black", "black"))
  )
  expect_false(verificar_tablero(tablero))
})

test_that("un meld inválido se detecta junto a otro válido", {
  tablero <- list(
    fichas(c(3, 4, 5), c("red", "red", "red")),
    fichas(c(9, 9, 9), c("red", "blue", "red"))   # color repetido
  )
  expect_false(verificar_tablero(tablero))
})

# --- Detalle y representación ---------------------------------------

test_that("detalle = TRUE devuelve valido y problemas", {
  tablero <- list(fichas(c(2, 4, 6), c("red", "red", "red")))
  res <- verificar_tablero(tablero, detalle = TRUE)
  expect_false(res$valido)
  expect_type(res$problemas, "character")
  expect_true(length(res$problemas) >= 1)
})

test_that("data.frame con meld_id equivale a lista", {
  tablero_df <- data.frame(
    meld_id = c(1, 1, 1, 2, 2, 2),
    number = c(3, 4, 5, 7, 7, 7),
    colour = c("red", "red", "red", "blue", "yellow", "black")
  )
  expect_true(verificar_tablero(tablero_df))

  tablero_df_malo <- data.frame(
    meld_id = c(1, 1, 2, 2),
    number = c(3, 4, 7, 7),
    colour = c("red", "red", "blue", "red")
  )
  expect_false(verificar_tablero(tablero_df_malo))
})

test_that("data.frame sin meld_id falla", {
  expect_error(verificar_tablero(fichas(c(3, 4, 5), c("red", "red", "red"))),
               "meld_id")
})

test_that("tablero_a_dataframe y tablero_a_lista son inversos", {
  lista <- list(
    fichas(c(3, 4, 5), c("red", "red", "red")),
    fichas(c(7, 7, 7), c("blue", "yellow", "black"))
  )
  df <- tablero_a_dataframe(lista)
  expect_equal(nrow(df), 6)
  expect_equal(names(df), c("meld_id", "number", "colour"))
  expect_true(verificar_tablero(df))
  expect_equal(tablero_a_lista(df), lista)
})
