# miscellaneous helpers

gold = (1+5^0.5)/2 # the golden ratio

is.outlier <- function(x, cutoff=4, ...) {
  m = mean(x, ...)
  s = sd(x, ...)
  (x < m - cutoff * s) | (x > m + cutoff * s)
}

# growth rate
grate <- function(x, n=1) {
  (x/data.table::shift(x, n=n)) - 1
}

round1 <- function(x) {
  round(x, digits = 1L)
}
round2 <- function(x) {
  round(x, digits = 2L)
}
round3 <- function(x) {
  round(x, digits = 3L)
}

# round + format
formatNum <- function(x, digits = 0L, nsmall = digits, ...) {
  format(round(x, digits = digits), big.mark = ",", trim = TRUE, nsmall = nsmall, ...)
}

fread_utf8 <- function(...) data.table::fread(..., encoding = "UTF-8")
fread_keepzeros <- function(file, ..., keepLeadingZeros=TRUE) {
  data.table::fread(file, ..., keepLeadingZeros = keepLeadingZeros)
}
appendLeadingZeros <- function(x) {
  ifelse(grepl('^[1-9][0-9]{3}$', x), paste0('0', x), x)
}


# dln(x) = lnx_1 - lnx_0 = ln(x_1/x_0)
# g = x_1/x_0 - 1
ldiff2grate <- function(x, percent=FALSE) {
  grate = exp(x)-1
  if (percent)
    return(grate * 100)
  grate
}


# edit equation
edit_eq <- function(eq) {
  eq = trimws(eq, "both")
  eq = gsub("(?<==) ?[+]", " ", eq, perl = T)
  eq = gsub("([+-])", " \\1 ", eq, perl = T)
  eq = sub("^[+]", "", eq)
  eq = gsub("-", " - ", eq)
  eq = gsub(" {2,}", " ", eq)
  trimws(eq, "both")
}



lm_labels <- function(dat, x, y, xlab = 'x', ylab = 'y') {
  form = as.formula(paste(y, '~', x))
  mod = lm(form, data = dat)
  se = summary(mod)$coefficients[2, 2]
  formula = sprintf(
    "italic(widehat(%s)) == %.2f %+.2f(%.2f) * italic(%s)",
    ylab, coef(mod)[1], coef(mod)[2], se, xlab)
  formula = sub("(?<!== )([-+])", '~\\1~', formula, perl = T)
  r2 = cor(dat[[x]], dat[[y]])**2
  r2 = sprintf("italic(R^2) == %.2f", r2)
  data.frame(formula = formula, r2 = r2)
}

## -----------------------------------------

#' replace missing value label (text) by the corresponding numerical code
#' e.g. "Sonstiges Missing" is encoded as '-9'.
#'
replace_missing_label_by_value <- function(x) {
  data.table::fcase(
    x %like% "[Aa]nonymisiert|Anonymized", "-11",
    x %like% "Variable ist erst in zukünftigen Eingabemasken vorhanden|Future variable", "-10",
    x %like% "Sonstiges Missing|Other missing", "-9",
    x %like% "Variable trifft auf diesen Datensatzyp nicht zu|Variable for other types only", "-8",
    x %like% "keine Angabe|Not specified", "-7",
    x %like% "Variable nicht mehr in Einagbemaske vorhanden|Old variable \\(no longer part of the platform\\)", "-6",
    x %like% "[Uu]nplausibler Wert geloescht|Implausible value", "-5",
    rep_len(TRUE, length(x)), x
  )
}



#' translate German var names into English (predefined in `table`)
translate_names <- function(var_de, table = NULL) {

  if (is.data.frame(var_de))
    stop('Expecting a character vector, not a data.frame!
         Perhaps, you want to translate the names of `var_de`?', call. = FALSE)

  if (is.null(table)) {
    source('script/cleaning/lists-of-variables.R', local = TRUE)
    table = selected_vars[,c('var_de', 'var_en')]
  }
  idx = match(var_de, table$var_de)
  out = table$var_en[idx]
  not_translated = which(is.na(out))
  out[not_translated] = var_de[not_translated]
  out
}

translate_mon <- function(monat) {
  mons = c("Januar", "Februar", "März", "April", "Mai", "Juni",
           "Juli", "August", "September", "Oktober", "November","Dezember")
  abb = c("Jan", "Feb", "März", "Apr", "Mai", "Juni",
          "Juli", "Aug", "Sept", "Okt", "Nov", "Dez")
  if (all(nchar(setdiff(monat, 'Mai')) >= 4L) )
    out = month.name[match(tolower(monat), tolower(mons))]
  else
    out = month.name[match(tolower(monat), tolower(abb))]
  out[which(is.na(out))] = monat[which(is.na(out))]
  out
}


#' count cases per categories of cols
freq <- function(df, cols) {
  stopifnot('`df` needs to be a `data.table`.' = is.data.table(df))
  types = vapply(df[, ..cols], class, character(1L))
  facs = cols[types %in% c('factor', 'integer')]
  chars = cols[types %in% 'character']
  ignored = setdiff(cols, union(facs, chars))
  if (length(ignored) != 0)
    warning('Ignoring non factor and/or character vars: ',
            paste0('`', ignored, '`', collapse = ", "), call. = FALSE)
  cols = union(facs, chars)
  lapply(cols, function(x) df[, .N, by = x])
}


## -------------------
# append values to par list
app_par <- function(par, ...) {
  dots = list(...)
  if (length(dots) == 0) return(par)
  stopifnot(!is.null(names(dots)) && !"" %in% names(dots))
  for (i in seq_along(dots)) {
    par[[names(dots)[[i]]]] = dots[[i]]
  }
  par
}


# global plotting parameters
op = par()
#opts_chunk$set(global.par=T) # carry par() over to all chunks
gpar = list(
  mar = c(4, 4, 2, 1) + .1,
  mgp = c(2, .5, 0), # margin b/n fig and axis label, axis text, and axis (line)
  tcl = -0.4,
  las = 1,
  bty = "L",
  family = "sans", cex=0.8
)
