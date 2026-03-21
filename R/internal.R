# Corrects text with double-decoded UTF-8 (mojibake)
#
# This function has been replaced by more robust implementations in `utils-internal.R`.
# It is kept for backward compatibility and is now a wrapper for `fix_encoding_text`.
#
# @param x Character vector to correct.
# @return Corrected character vector.
fix_double_utf8 <- function(x) {
  fix_encoding_text(x)
}
