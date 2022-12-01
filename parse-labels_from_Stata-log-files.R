source("script/helpers/base-helpers.R")
makeNames <- function(x, sep = "_") {
    nms = make.names(tolower(x), allow_ = FALSE)
    nms = gsub("\\.+", sep, nms)
    nms = sub("^X", "", nms)
    nms = gsub(sprintf("(^%s+)|(%s+$)", sep, sep), "", nms)
    nms
}

# this script parses variable and value labels from the Stata labels log file
# input: path to the (Stata) log file that contains variable/value labels
# for example, the log file for homes for sale (HK) is `./Dokumentation/Labels/Labels_Immoscout_HK_en.txt`

get_labels <- function(path) {
  if (!grepl("Labels_Immoscout_(W|H)(K|M)_en[.]txt$", path)) {
    stop('Expecting a Stata log file with `_en.txt` ending', call. = FALSE)
    }
  labels = readLines(path)
  header_end = grep("opened on:", labels)
  footer_start = max(grep("name:", labels))
  if (is.null(header_end) || is.null(footer_start)){
    stop("Could not parse the location of the header and footer!", call. = FALSE)
  }
  labels = labels[-(1:header_end)]       # rm header
  labels = labels[1:(footer_start - 1L)] # rm footer

  # variables' info is separated by 'empty line' in the file
  index = which(labels == "")
  ne = length(index)
  out = vector("list", ne)
  s = 1
  for (i in 1:ne) {
    out[[i]] = c(s, index[[i]] - 1)
    s = index[[i]] + 1
  }

  lbls = lapply(out, function(x) labels[x[[1]]:x[[2]]])
  var_names = vapply(lbls, `[[`, "", 1L) |>
    {\(x) regmatches(x, regexpr("\\(\\w+?\\)$", x))}() |> # (not greedy is important e.g. look at Unique_ID)
    unname() |>
    gsub(pattern="[()]",replacement="", x=_)
  names(lbls) = var_names

  # some vars do not have value labels
  has_value_label = sapply(
    lbls,
    \(x) !grepl("\\bVariable .+ does not have a value label[.]\\b", x)[[2]]
  )

  value_labels = lapply(var_names, function(var_de) {
    stopifnot(hasName(lbls, var_de))
    x = lbls[[var_de]]
    attribute = x[c(1, 2)]
    x = trimws(x[-c(1, 2)])
    n = length(x)
    x = strcapture(
      "(^-?\\d{1,}) (.+)", x,data.frame(value = integer(n), label = character(n))
      )
    attr(x, "info") <- attribute
    x
  })
  names(value_labels) = var_names

  # variable labels
  variable_labels = Map(
    \(p, x) sub(sprintf(" (%s)", p), "", x, fixed = TRUE),
    var_names, vapply(lbls, `[[`, "", 1L)
  ) |>
    sapply(\(x) sub("^Variable: ", "", x))

  # some vars do not have variable label e.g. `duplicateid`
  has_variable_label = (variable_labels != "")
  variable_labels = data.frame(name = names(variable_labels), label = variable_labels)
  # replace empty labels with the corresponding names
  variable_labels = within(variable_labels, {
    label <- ifelse(!has_variable_label, name, label)
  })

  return(list(
  variable = variable_labels,
  value = value_labels,
  has_variable_label = has_variable_label,
  has_value_label = has_value_label
))
}

# write to disk ----
labels = get_labels('../Common-Data/.RED_v6.0/Dokumentation/Labels/Labels_Immoscout_HK_en.txt')

dict = with(labels$variable, data.frame(var_de = name, label, var_en = makeNames(label)))
write.csv(dict, 'data/variable-and-value_labels/variable-labels.csv', row.names = FALSE)

# has labels other than labels for missing values
has_real_labels = with(labels, sapply(value, \(x) !all(x[['value']] < 0)))
with(labels, lapply(
  seq_along(value[has_value_label & has_real_labels]),
  \(i) with(list(x = value[has_value_label][[i]]), data.frame(
    var = c(names(value[has_value_label])[[i]], rep("----", nrow(x) - 1)),
    x
  ))
)) |> do.call(what = rbind) |>
  write.table('data/variable-and-value_labels/value-labels.txt', quote = FALSE, row.names = FALSE)

# value labels for missing values
special_value_labels = with(labels, value[!has_real_labels]) |>
  do.call(what = rbind) |>
  unique()
rownames(special_value_labels) = NULL
write.csv(
  special_value_labels,'data/variable-and-value_labels/missing-value_labels.csv',
  quote = FALSE, row.names = FALSE
  )

# get value labels of a factor variable
## include_missing is to exclude -5 to -11 which are special missing values
get_value_labels <- function(var_name, include_missing = TRUE) {
  stopifnot(
    `The \`var_name\` is not correct. Try passing the var_name in german?` = var_name %in% names(labels$value)
  )
  x <- labels$value[[var_name]]
  if (is.null(x)) {
    warning("label for `", var_name, "` not found. Make sure that you pass the correct var name.", call. = FALSE)
  }
  if (include_missing) {
    x
  } else if (!include_missing) {
    x[which(x$value > -1), ]
  }
}

# test
get_value_labels('kategorie_Haus')
