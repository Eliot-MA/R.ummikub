# tests/test_objetivo_01.R
#
# Pruebas del objetivo 1 (melds válidos y regla del 30).
# Ejecutar con:
#   Rscript -e "testthat::test_file('tests/test_objetivo_01.R')"
#   (o `library(testthat); test_file("tests/test_objetivo_01.R")` en RStudio)

source(file.path(dirname(testthat::test_path()), "..", "scripts",
                 "01_melds_validos.R"))

library(testthat)

test_that("es_run detecta escaleras válidas", {
  run <- fichas(c(3, 4, 5), c("blue", "blue", "blue"))
  expect_true(es_run(run))
})

test_that("es_run rechaza escaleras inválidas", {
  expect_false(es_run(fichas(c(3, 4), c("blue", "blue"))))             # 2 fichas
  expect_false(es_run(fichas(c(3, 4, 4), c("blue", "blue", "blue"))))  # repetido
  expect_false(es_run(fichas(c(3, 4, 5), c("blue", "blue", "red"))))   # colores
  expect_false(es_run(fichas(c(3, 5, 7), c("blue", "blue", "blue"))))  # huecos
  expect_false(es_run(fichas(c(NA, NA), c("wild", "wild"))))           # solo jokers
})

test_that("es_group detecta grupos válidos", {
  grp <- fichas(c(7, 7, 7), c("red", "blue", "black"))
  expect_true(es_group(grp))
  grp4 <- fichas(c(8, 8, 8, 8), COLORES)
  expect_true(es_group(grp4))
})

test_that("es_group rechaza grupos inválidos", {
  expect_false(es_group(fichas(c(7, 7), c("red", "blue"))))           # 2 fichas
  expect_false(es_group(fichas(c(7, 7, 7, 7, 7), c("red", "blue", "yellow", "black", "red")))) # 5 fichas
  expect_false(es_group(fichas(c(7, 8, 7), c("red", "blue", "yellow")))) # números
  expect_false(es_group(fichas(c(7, 7, 7), c("red", "red", "blue"))))    # color repetido
  expect_false(es_group(fichas(c(NA, NA), c("wild", "wild"))))           # solo jokers
})

test_that("jokers en runs", {
  run <- fichas(c(3, NA, 5), c("blue", "wild", "blue"))
  expect_true(es_run(run))
  expect_equal(sum(asignar_run(run)$number), 12)   # 3,4,5

  run_fin <- fichas(c(1, 2, 3, NA), c("blue", "blue", "blue", "wild"))
  expect_true(es_run(run_fin))
  expect_equal(sum(asignar_run(run_fin)$number), 10)  # 1,2,3,4

  run_solo <- fichas(c(5, NA, NA), c("blue", "wild", "wild"))
  expect_true(es_run(run_solo))                      # 5,6,7 canónico
  expect_equal(sum(asignar_run(run_solo)$number), 18)
})

test_that("jokers en groups", {
  grp <- fichas(c(7, 7, NA), c("red", "blue", "wild"))
  expect_true(es_group(grp))
  expect_equal(sum(asignar_group(grp)$number), 21)

  grp2 <- fichas(c(13, 13, NA, NA), c("red", "blue", "wild", "wild"))
  expect_true(es_group(grp2))
  expect_equal(sum(asignar_group(grp2)$number), 52)
})

test_that("es_meld_valido combina run y group", {
  expect_true(es_meld_valido(fichas(c(9, 10, 11), c("red", "red", "red"))))
  expect_true(es_meld_valido(fichas(c(5, 5, 5), c("red", "blue", "yellow"))))
  expect_false(es_meld_valido(fichas(c(5, 5), c("red", "blue"))))
  expect_false(es_meld_valido(fichas(c(2, 4, 6), c("red", "red", "red"))))
})

test_that("suma_meld puntúa con el valor canónico", {
  expect_equal(suma_meld(fichas(c(9, 10, 11), c("red", "red", "red"))), 30)
  expect_equal(suma_meld(fichas(c(5, 5, 5), c("red", "blue", "yellow"))), 15)
})

test_that("cumple_regla_30", {
  expect_true(cumple_regla_30(fichas(c(9, 10, 11), c("red", "red", "red"))))   # 30
  expect_false(cumple_regla_30(fichas(c(1, 2, 3), c("red", "red", "red"))))   # 6

  meld_a <- fichas(c(8, 9, 10), c("blue", "blue", "blue"))    # 27
  meld_b <- fichas(c(4, 4, 4), c("red", "blue", "yellow"))    # 12
  expect_true(cumple_regla_30(list(meld_a, meld_b)))          # 27 + 12 = 39
})
