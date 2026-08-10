# tests/test_objetivo_05.R
#
# Pruebas del objetivo 5 (visualización del tablero).
# Ejecutar con:
#   Rscript -e "testthat::test_file('tests/test_objetivo_05.R')"

source(file.path(dirname(testthat::test_path()), "..", "scripts",
                 "05_visualizacion_tablero.R"))



library(testthat)

test_that("tablero_a_datos ordena los runs por número", {
  tablero <- list(fichas(c(5, 3, 4), c("red", "red", "red")))
  datos <- tablero_a_datos(tablero)
  expect_equal(datos$number, 3:5)
  expect_equal(datos$posicion, 1:3)
  expect_equal(datos$meld, rep(1, 3))
  expect_equal(datos$etiqueta, c("3", "4", "5"))
})

test_that("tablero_a_datos ordena los groups por color y etiqueta jokers", {
  tablero <- list(fichas(c(7, 7, 7), c("yellow", "red", "blue")))
  datos <- tablero_a_datos(tablero)
  expect_equal(datos$colour, c("red", "blue", "yellow"))

  tablero_j <- list(fichas(c(3, NA, 5), c("blue", "wild", "blue")))
  datos_j <- tablero_a_datos(tablero_j)
  expect_true(any(is.na(datos_j$number)))
  expect_equal(datos_j$etiqueta[is.na(datos_j$number)], "J")
})

test_that("tablero_a_datos acepta data.frame con meld_id", {
  tablero_df <- data.frame(
    meld_id = c(1, 1, 1, 2, 2),
    number = c(8, 9, 10, 4, 4),
    colour = c("red", "red", "red", "blue", "yellow")
  )
  datos <- tablero_a_datos(tablero_df)
  expect_equal(nrow(datos), 5)
  expect_equal(sort(unique(datos$meld)), 1:2)
})

test_that("tablero vacío", {
  expect_equal(nrow(tablero_a_datos(list())), 0)
})

test_that("mostrar_tablero imprime cada meld", {
  tablero <- list(
    fichas(c(3, 4, 5), c("red", "red", "red")),
    fichas(c(7, 7, 7), c("blue", "yellow", "black"))
  )
  salida <- capture.output(mostrar_tablero(tablero))
  expect_true(any(grepl("Meld 1 \\(run\\)", salida)))
  expect_true(any(grepl("Meld 2 \\(group\\)", salida)))
  expect_true(any(grepl("5 red", salida)))
})

test_that("mostrar_tablero con tablero vacío", {
  expect_output(mostrar_tablero(list()), "Tablero vacío")
})

test_that("graficar_tablero devuelve un ggplot", {
  tablero <- list(
    fichas(c(3, 4, 5), c("red", "red", "red")),
    fichas(c(7, 7, NA), c("blue", "yellow", "wild"))
  )
  p <- graficar_tablero(tablero)
  expect_s3_class(p, "ggplot")
})

test_that("graficar_tablero con tablero vacío avisa y devuelve NULL", {
  expect_warning(graficar_tablero(list()), "vacío")
  expect_null(suppressWarnings(graficar_tablero(list())))
})
