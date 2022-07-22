# base-r equivalents of stringr::str_*  ---------------------------------------------

# `nomatch` is represented by `NULL` and `NA`, in `base-r`'s and `stringr`'s, resp.
if_no_match_return_na <- function(x) {
  if (length(x) == 0L) NA else x
}

if_no_match_return_na = Vectorize(if_no_match_return_na)

## extract bracketed match -----
str_match <- function(string, pattern, invert = F, ...) {
  if_no_match_return_na(
    regmatches(string, regexec(pattern, string, ...), invert = invert)
  )
}


str_match_all <- function(string, pattern, invert = F, ...) {
  if_no_match_return_na(
    regmatches(string, gregexec(pattern, string, ...), invert = invert)
  )
}


## extract match ----
str_extract <- function(string, pattern, invert = F, ...) {
  if_no_match_return_na(
    regmatches(string, regexpr(pattern, string,...), invert = invert)
  )
}

str_extract_all <- function(string, pattern, invert=F, ...) {
  if_no_match_return_na(
    regmatches(string, gregexpr(pattern, string, ...), invert = invert)
  )
}


## replace matches in the string ----
str_replace <- function(string, pattern, replacement, ...) {
  sub(pattern, replacement, string, ...)
}

str_replace_all <- function(string, pattern, replacement, ...) {
  gsub(pattern, replacement, string, ...)
}

## remove matched from the string ----

str_remove <- function(string, pattern, ...) {
  str_replace(string, pattern, "", ...)
}

str_remove_all <- function(string, pattern, ...) {
  str_replace_all(string, pattern, "", ...)
}


starts_with = function(string, pattern, ignore.case = FALSE, ...) {
  if (anyNA(string)) stop('`string` should not contain any NA', call. = FALSE)
  grepl(paste0("^", pattern), string, ignore.case = ignore.case, ...)
}
ends_with = function(string, pattern, ignore.case = FALSE, ...) {
  if (anyNA(string)) stop('`string` should not contain any NA', call. = FALSE)
  grepl(paste0(pattern, "$"), string, ignore.case = ignore.case, ...)
}



## pad a string with n width ----
str_pad <- function(string, n) {
  warning(any(n < nchar(string)), '`n` should be greater than `nchar(x)`', call. = F)
  sprintf(paste0("%", n, "s"), string)
}

## remove white space anywhere in the string ----
str_squish <- function(string) {
  gsub("[ ]{2,}", " ", trimws(string, which = 'both'))
}

## subset a string ----
str_sub <- function(string, start = 1L, end = nchar(string)) {
  str_sub_one <- function(x, s, e) {
    if (s < 0) s <- nchar(x) + s + 1
    if (e < 0) e <- nchar(x) + e + 1
    if (s > e) {
      warning("`start` is greater than `end` for some string.", call. = F)
    }
    substr(x, s, e)
  }
  lt <- length(string)
  ls <- length(start)
  le <- length(end)

  if (all(c(lt, ls, le) == 1L)) {
    return(str_sub_one(string, start, end))
  }

  n <- max(lt, ls, le)
  if (lt && lt < n) {
    string <- rep_len(string, length.out = n) # as in `substring()`
  }
  mapply(str_sub_one, x = string, s = start, e = end, USE.NAMES = F)
}
