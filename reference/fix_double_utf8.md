# Corrige texto con UTF-8 doblemente decodificado (mojibake)

Esta función intenta reparar cadenas de texto que fueron codificadas
originalmente en UTF-8 y luego interpretadas/guardadas erróneamente como
Latin-1 / Windows-1252, produciendo secuencias del tipo
`"QUIÃ\u0091OTA"` en lugar de `"QUIÑOTA"`.

## Usage

``` r
fix_double_utf8(x)
```

## Arguments

- x:

  Vector de caracteres a corregir. Puede contener `NA`.

## Value

Un vector de caracteres con la misma longitud que `x`, donde las
secuencias de UTF-8 doblemente decodificadas han sido reemplazadas por
su forma correcta (por ejemplo, `"QUIÃ\u0091OTA"` → `"QUIÑOTA"`), en la
medida en que coincidan con el patrón detectado.

## Details

El caso típico que corrige es cuando un caracter de dos bytes en UTF-8
(por ejemplo, la *Ñ* codificada como `C3 91`) termina almacenado como
cuatro bytes `C3 83 C2 91`, que luego se muestran como `"ÃÃÂ\u0091"`. La
función detecta este patrón a nivel de bytes y lo reescribe a su forma
correcta de dos bytes UTF-8.

La función trabaja a nivel de bytes usando
[`charToRaw()`](https://rdrr.io/r/base/rawConversion.html) y recorre
cada cadena buscando el patrón:

- `C3 83 C2 XX`, que se reemplaza por `C3 XX`.

Esto corresponde a caracteres de dos bytes en UTF-8 que han sido
interpretados erróneamente como Latin-1/Windows-1252 y luego vueltos a
codificar en UTF-8.

Actualmente la lógica está centrada en este patrón específico, que es
suficiente para muchos casos de texto en español (por ejemplo letras con
tilde y la letra Ñ) afectados por doble decodificación. Si se
identifican otros patrones de mojibake, la función se puede extender
añadiendo reglas adicionales dentro del bucle.
