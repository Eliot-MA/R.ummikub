# tests/test_objetivo_02.R
#
# Pruebas del objetivo 2 (probabilidad de pescar ficha útil).
# Ejecutar con:
#   Rscript -e "testthat::test_file('tests/test_objetivo_02.R')"

source(file.path(dirname(testthat::test_path()), "..", "scripts",
                 "02_probabilidad_pesca.R"))

library(testthat)

test_that("contar_fichas del pool completo", {
  c <- contar_fichas(pool_fichas())
  expect_equal(nrow(c), 53)                 # 52 tipos numerados + joker
  expect_true(all(c$n == 2))                # 2 copias de cada tipo
  expect_equal(sum(c$n), 106)
})

test_that("pool_disponible resta mano y mesa", {
  mano <- fichas(5, "red")
  expect_equal(sum(pool_disponible(mano)$n), 105)
  expect_equal(sum(pool_disponible(mano, mesa = fichas(5, "red"))$n), 104)
  # n_contrincante no cambia las cuentas por tipo (fichas desconocidas),
  # solo el total de fichas que quedan por pescar.
  expect_equal(sum(pool_disponible(mano, n_contrincante = 10)$n), 105)
  expect_equal(attr(pool_disponible(mano, n_contrincante = 10), "total_disponible"), 95)
})

test_that("pool_disponible rechaza fichas que no existen", {
  expect_error(pool_disponible(mesa = fichas(c(5, 5, 5), rep("red", 3))),
               "más fichas")
})

test_that("probabilidad con mano vacía (solo jokers útiles)", {
  expect_equal(prob_pesca_util(), 2 / 106)
})


test_that("probabilidad con una ficha en la mano", {
  mano <- fichas(5, "red")
  # útiles: 4 rojo (2), 6 rojo (2), 5 en otros 3 colores (6), jokers (2)
  expect_equal(prob_pesca_util(mano), 12 / 105)
})

test_that("probabilidad con un grupo casi completo", {
  mano <- fichas(c(7, 7, 7), c("red", "blue", "yellow"))
  # útiles: 6 y 8 del mismo color que el 7 (12), 7 negro (2), jokers (2) = 16
  expect_equal(prob_pesca_util(mano), 16 / 103)
})

test_that("probabilidad con un run de 3 en la mano", {
  mano <- fichas(c(1, 2, 3), c("red", "red", "red"))
  # útiles: 4 rojo (2), copias restantes 1/2/3 rojo (1 c/u), 1/2/3 en otros
  # colores (18), jokers (2) = 25
  expect_equal(prob_pesca_util(mano), 25 / 103)
})

test_that("la mesa reduce los útiles disponibles", {
  mano <- fichas(5, "red")
  mesa <- fichas(4, "red")
  # útiles: 4 rojo (1 queda), 6 rojo (2), 5 otros colores (6), jokers (2) = 11
  expect_equal(prob_pesca_util(mano, mesa), 11 / 104)
})

test_that("n_contrincante no cambia la probabilidad de una pesca", {
  mano <- fichas(5, "red")
  p0 <- prob_pesca_util(mano, n_contrincante = 0)
  p5 <- prob_pesca_util(mano, n_contrincante = 5)
  p20 <- prob_pesca_util(mano, n_contrincante = 20)
  expect_equal(p0, p5)
  expect_equal(p0, p20)
})

test_that("n_contrincante fuera de rango falla", {
  expect_error(prob_pesca_util(fichas(5, "red"), n_contrincante = 200))
})

test_that("criterio es_util personalizado", {
  solo_rojas <- function(ficha, mano) ficha$colour == "red"
  expect_equal(prob_pesca_util(es_util = solo_rojas), 26 / 106)
})

test_that("fichas_utiles devuelve los tipos útiles con su conteo", {
  mano <- fichas(5, "red")
  u <- fichas_utiles(mano)
  expect_equal(sum(u$n), 12)
  expect_true(all(u$colour != "red" | u$number %in% c(4, 6)))
})
