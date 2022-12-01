# miscellaneous helpers

gold = (1+5^0.5)/2 # the golden ratio

is_outlier <- function(x, cutoff = 4, na.rm = TRUE, ...) {
  m = mean(x, na.rm = na.rm, ...)
  s = sd(x, na.rm = na.rm, ...)
  (x <= m - cutoff * s) | (x >= m + cutoff * s)
}

# a practical cut
cut2 <- function(x, ..., include.lowest = TRUE, right = FALSE) {
  cut(x, ..., include.lowest = include.lowest, right = right)
}

# append 01 to mon-year -> date
mydate <- function(mon, year) {
  as.Date(paste0("01-", paste0(mon, '-', year)), "%d-%m-%Y")
}

mdate <- function(x) {
  as.Date(paste0("01-", x), "%d-%m-%Y")
}

fmy = function(x, format = "%b-%y") format.Date(x, format = format)

meter = function(x) units::set_units(x, "m")

km = function(x) units::set_units(x, "km")

getLabel <- function(x, math = FALSE) {
  switch(x,
    lnp = if (math) expression(ln ~ P) else "ln P",
    lnr = if (math) expression(ln ~ R) else "ln R",
    lnhpi = if (math) expression(ln ~ P) else "ln P",
    lnhri = if (math) expression(ln ~ R) else "ln R",
    x
  )
}


# tidy fixed effects from fixest::fixef()
tidy_fixeff <- function(fe_obj) {
  stopifnot(inherits(fe_obj, "fixest.fixef") && names(fe_obj) != "")
  fe = data.frame(id = names(fe_obj[[1]]), eff = fe_obj[[1]])
  fixeffs = strsplit(names(fe_obj), "^", fixed = TRUE)[[1]]
  fe[, fixeffs] = do.call("rbind", strsplit(fe$id, "_"))
  fe = fe[, c(fixeffs, "eff")]
  rownames(fe) = NULL
  fe
}


# count missing values, which are flagged by any negative number
count_missing <- function(df) {
  lapply(df, \(x) sum(x < 0)) |>
    {\(x) data.frame(var = names(x), count = unname(unlist(x)))}()
}


# STL decomposition
decomp = function(x,
  do = c("deseason", "detrend", "remainder", 'trend', 'seasonal'),
  start, end, freq = 12) {
    # start = c(2018L, 1L), end = c(2021L, 12L)
  if (nrow(x) < 2L) {
    message('Atleast 2 obs needed! No operation')
    return(x)
    }
  stopifnot(all(c("year", "mon", "val") %in% names(x)))
  if (!missing(start) || !missing(end))
    stopifnot(length(start) == length(end) && length(start) == 2L)

  if (missing(start) || missing(end)) {
    start = c(min(x$year), min(x$mon))
    end = c(max(x$year), max(x$mon))
    message(sprintf('Auto generated start[y, m]: [%i, %i]', start[1], start[2]))
    message(sprintf('Auto generated end[y, m]: [%i, %i]', end[1], end[2]))
}
  do = match.arg(do)
  message('STL Decompostion: <', do, '-ing>')
  # x = x[order(x$year, x$mon),] # tidyr::completes does ordering automatically
  xc = tidyr::complete(x, tidyr::expand(x, year = (start[1]:end[1]), mon = (start[2]:end[2])))
  # tidyr::complete may put some year-mons that are not specified at the end
  xc = xc[order(xc$year, xc$mon), ]
  xts = ts(xc$val, start = start, end = end, frequency = freq)
  # deal with NA actual and NA introduced by tidy::complete
  seen = -(setdiff(which(is.na(xc$val)), which(is.na(x$val))))
  if (length(seen) == 0) {
    seen = seq_len(nrow(xc))
  }
  na.act = function(x) {
    zoo::na.approx(x, na.rm = FALSE, rule = 2)
  }
  tryCatch(
    {
      components = stl(xts, s.window = "per", na.action = na.act)$time.series
      xts = rowSums(components) # NA imputed
      my = format(zoo::as.yearmon(time(components)), "%m-%Y")
      stlval = switch(do,
        deseason = xts - components[, "seasonal"],
        detrend = xts - components[, "trend"],
        remainder = components[, "remainder"],
        trend = components[, 'trend'],
        seasonal = components[, "reasonal"],
        stop("Did you pass the right val for `return`?", call. = FALSE)
      )
      data.frame(
        year = as.numeric(substr(my, 4, 7)),
        mon = as.integer(substr(my, 1, 2)),
        stlval = as.vector(stlval)
      )
    },
    error = function(e) {
      withCallingHandlers({
        warning(e)
        xc$val[seen]
      })
    }
  )
}



# round function factory, useless?
rnd <- function(digits) {
  force(digits)
  function(x) round(x, digits=digits)
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
