# 00_pool_fichas.R
#
# Definición del pool de fichas de Rummikub (estándar, 106 fichas):
#   - 104 fichas numeradas: números 1 a 13 en 4 colores
#     (red, blue, yellow, black) x 2 copias de cada combinación
#   - 2 jokers
#
# Este script se carga desde los demás con source() para que todos usen
# la misma representación de fichas.
#
# Representación de ficha (una fila):
#   - `number`: entero 1:13, o NA para un joker
#   - `colour`: carácter: "red", "blue", "yellow", "black" o "wild" (joker)
#
# Un meld se representa como un data.frame con estas mismas columnas.

COLORES <- c("red", "blue", "yellow", "black")

# Crea el pool completo de 106 fichas.
pool_fichas <- function() {
  numeradas <- expand.grid(number = 1:13, colour = COLORES)
  numeradas <- numeradas[rep(seq_len(nrow(numeradas)), each = 2), ]
  rownames(numeradas) <- NULL
  jokers <- data.frame(number = rep(NA_integer_, 2), colour = "wild")
  rbind(numeradas, jokers)
}

# Crea un meld (data.frame de fichas) a partir de vectores number/colour.
fichas <- function(number, colour) {
  data.frame(number = as.integer(number), colour = colour)
}

# ¿Es un joker?
es_joker <- function(number, colour) {
  is.na(number) | colour == "wild"
}

# --- Inventario de fichas (compartido por todos los scripts) --------

# Cuenta cuántas copias de cada tipo (number, colour) hay en `tiles`.
# Devuelve data.frame con columnas number, colour, n.
contar_fichas <- function(tiles) {
  if (nrow(tiles) == 0) {
    return(data.frame(number = integer(0), colour = character(0), n = integer(0)))
  }
  clave <- paste(ifelse(is.na(tiles$number), "J", tiles$number),
                 tiles$colour, sep = "|")
  t <- table(clave)
  partes <- strsplit(names(t), "\\|")
  number <- vapply(partes, function(p) {
    if (p[1] == "J") NA_integer_ else as.integer(p[1])
  }, integer(1))
  colour <- vapply(partes, function(p) p[2], character(1))
  data.frame(number = number, colour = colour, n = as.integer(t))
}

# Resta `restar` (inventario) de `contador` (inventario).
restar_fichas <- function(contador, restar) {
  for (i in seq_len(nrow(restar))) {
    num_r <- restar$number[i]
    col_r <- restar$colour[i]
    coincide <- (contador$colour == col_r) &
      ((is.na(contador$number) & is.na(num_r)) |
         (!is.na(contador$number) & !is.na(num_r) & contador$number == num_r))
    if (!any(coincide)) {
      stop("Ficha no encontrada en el pool: ", num_r, " ", col_r)
    }
    j <- which(coincide)[1]
    contador$n[j] <- contador$n[j] - restar$n[i]
    if (contador$n[j] < 0) {
      stop("Hay más fichas de las que existen en el pool")
    }
  }
  contador[contador$n > 0, , drop = FALSE]
}
