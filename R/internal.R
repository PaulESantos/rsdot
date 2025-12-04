#' Corrige texto con UTF-8 doblemente decodificado (mojibake)
#'
#' Esta función intenta reparar cadenas de texto que fueron codificadas
#' originalmente en UTF-8 y luego interpretadas/guardadas erróneamente como
#' Latin-1 / Windows-1252, produciendo secuencias del tipo `"QUIÃ\u0091OTA"`
#' en lugar de `"QUIÑOTA"`.
#'
#' El caso típico que corrige es cuando un caracter de dos bytes en UTF-8
#' (por ejemplo, la *Ñ* codificada como `C3 91`) termina almacenado como
#' cuatro bytes `C3 83 C2 91`, que luego se muestran como `"ÃÃÂ\u0091"`.
#' La función detecta este patrón a nivel de bytes y lo reescribe a su forma
#' correcta de dos bytes UTF-8.
#'
#' @param x
#'   Vector de caracteres a corregir. Puede contener `NA`.
#'
#' @return
#'   Un vector de caracteres con la misma longitud que `x`, donde las
#'   secuencias de UTF-8 doblemente decodificadas han sido reemplazadas
#'   por su forma correcta (por ejemplo, `"QUIÃ\u0091OTA"` → `"QUIÑOTA"`),
#'   en la medida en que coincidan con el patrón detectado.
#'
#' @details
#' La función trabaja a nivel de bytes usando \code{charToRaw()} y recorre
#' cada cadena buscando el patrón:
#' \itemize{
#'   \item \code{C3 83 C2 XX}, que se reemplaza por \code{C3 XX}.
#' }
#' Esto corresponde a caracteres de dos bytes en UTF-8 que han sido
#' interpretados erróneamente como Latin-1/Windows-1252 y luego vueltos
#' a codificar en UTF-8.
#'
#' Actualmente la lógica está centrada en este patrón específico, que es
#' suficiente para muchos casos de texto en español (por ejemplo letras
#' con tilde y la letra Ñ) afectados por doble decodificación. Si se
#' identifican otros patrones de mojibake, la función se puede extender
#' añadiendo reglas adicionales dentro del bucle.
#'
#'
#' @keywords internal
fix_double_utf8 <- function(x) {
  vapply(
    x,
    FUN.VALUE = character(1),
    FUN = function(s) {
      if (is.na(s)) return(NA_character_)

      r <- charToRaw(s)
      out <- raw(0)
      n <- length(r)
      i <- 1L

      while (i <= n) {
        # Patrón típico: C3 83 C2 XX  -> debería ser C3 XX
        if (i <= n - 3L &&
            r[i]   == as.raw(0xc3) &&
            r[i+1] == as.raw(0x83) &&
            r[i+2] == as.raw(0xc2)) {

          out <- c(out, as.raw(0xc3), r[i+3])
          i <- i + 4L

        } else {
          out <- c(out, r[i])
          i <- i + 1L
        }
      }

      rawToChar(out)
    }
  ) |>
    as.character()
}
