library(data.table)
library(haven, include.only = "read_dta")
source("script/helpers/base-helpers.R")
source("script/helpers/utils.R")
since_when = 2016 # keep data since this year
extract_raw_files = FALSE

# extract if not available
## caution: unzip may truncate extraction if the size is > 4GB
## better to do it with the command line or manually with 7zip etc
zipfile = "../Common-Data/RED_v6.1.zip"
extractdir = paste0("../Common-Data/.", file.stem(zipfile))
# unzip(zipfile, overwrite = TRUE, exdir = extractdir) # fails because of size
if (extract_raw_files) {
  system(sprintf("unzip -o -d %s %s", extractdir, zipfile))
} # o for overwrite d for directory


## stata files
dlist = c(
  `homes-sales` = "HK_SUF",
  `homes-rents` = "HM_SUF",
  `apartments-sales` = "WK_SUF",
  `apartments-rents` = "WM_SUF"
)
# dlist_csv = sub("_", "", dlist)

zip_fpath_chosen_ftype = paste0(extractdir, "/csv.zip")
if (extract_raw_files)
  system(sprintf('unzip -o -d %s %s',
                 paste0(dirname(zip_fpath_chosen_ftype), '/', file.stem(zip_fpath_chosen_ftype)),
                 zip_fpath_chosen_ftype))

homes_zipfile = paste0(extractdir, "/csv/", dlist, "_csv.zip")
for (zfile in homes_zipfile) {
  if (extract_raw_files) {
    if (file.exists(zfile)) {
      system(sprintf("unzip -o -d %s %s", dirname(zfile), zfile))
    } else {
      warning(sprintf("The zipfile `%s` does not exist so no extraction has been made.", zfile))
    }
  }
}

# iterate over homes/apartments for sale (HK_SUF,WK_SUF) and -----
# homes/apartments for rent (HM_SUF,WM_SUF)

# have to choose either the .csv or .dta files
## the former comes with text labels for missing values, the latter can come in with or without tex


for (h in seq_along(dlist_csv)[4]) {
  file_list = dir(
    path = paste0(extractdir, "/csv/", dlist[[h]], "_csv/"),
    pattern = paste0("^", dlist_csv[[h]], "(_ohneText)?([0-9]+)?[.]csv$"),
    full.names = TRUE
  )
  #names(file_list) = sub("(..)(.+)", "\\1_\\2", names(file_list))
  if (length(file_list) == 0) {
    stop("No list of files found.", call. = FALSE)
  } else if (length(file_list) > 1) {
    # sorting was necessary because HKSUF10 was coming second instead of 10th
    file_list = file_list[order(as.integer(regmatches(basename(file_list), regexpr("[0-9]+", basename(file_list)))))]
  }
  names(file_list) = tools::file_path_sans_ext(basename(file_list))
  var_list = lapply(file_list, fread, nrows = 10) |> lapply(names)
  all_vars = Reduce(union, var_list) # complete var set
  common_vars = Reduce(intersect, var_list) # vars available across the files
  problematic = setdiff(all_vars, common_vars) # vars that exist only in some of the files/datasets

  # a matrix showing whether a variable is available or not in each data set
  if (FALSE) {
    data_record = matrix(nrow = length(all_vars), ncol = length(var_list))
    dimnames(data_record) = list(all_vars, names(var_list))
    for (v in seq_along(all_vars)) {
      for (i in seq_along(var_list)) {
        data_record[v, i] = as.integer(all_vars[[v]] %in% var_list[[i]])
      }
    }
    data_record = cbind(data_record, sum = rowSums(data_record))
  }

  ## combine datasets across by year
  yearvar = lapply(file_list, \(f) names(fread(f, nrows = 1L))) |>
    unlist() |>
    grep("^[e]?jahr$", x = _, value = TRUE)
  yearvar = if ("jahr" %in% yearvar) {
    "jahr"
  } else if ("ejahr" %in% yearvar) {
    "ejahr"
  } else {
    stop("No `year` variable in the data set.", call. = FALSE)
  }
  years_in_dataset = lapply(file_list, \(f) unique(fread(f, select = (yearvar)))) |>
   rbindlist(idcol = "file")
  if (yearvar == 'ejahr')
    names(years_in_dataset)[match('ejahr', names(years_in_dataset))] = "jahr"
  # part of `since_when` could be in a data set that contains since_when-1
  selected_files = years_in_dataset[years_in_dataset$jahr >= since_when,]$file
  #selected_files = sub("SUF", "_SUF", selected_files)
  selected = names(file_list)[names(file_list) %in% selected_files]
  # make explicitly NA
  all_nas_patt = c(
    "Variable trifft auf diesen Datensatzyp nicht zu|Variable for other types only",
    "Variable nicht mehr in Einagbemaske vorhanden|Old variable \\(no longer part of the platform\\)"
  )
  all_nas_num = c(-8, -10) # for ohneText data

  df_suf = lapply(file_list[selected], fread);gc()
  df_suf = lapply(df_suf, \(d) d[, lapply(.SD, replace_missing_label_by_value),.SDcols=is.character])
  df_suf = lapply(df_suf, \(d) {
    # variables that should be dropped
    drop = names(d)[vapply(
      d[sample.int(nrow(d), 100), ],
      \(x) all(grepl(paste(all_nas_num, collapse = "|"), x)), NA
    )]
    d[, (setdiff(names(d), drop)), with=FALSE]
  })
  common_vars_selected = Reduce(intersect, lapply(df_suf, names))
  # drop vars that are not available across data sets
  df_suf = lapply(df_suf, \(x) x[, (intersect(names(x), common_vars_selected)), with=FALSE])
  gc()
  df_suf = rbindlist(df_suf, use.names = TRUE)
  gc()
  if (yearvar == 'ejahr') {
  df_suf[, jahr := ejahr]
  common_vars_selected = c(common_vars_selected, 'jahr')
  }
  df_suf = df_suf[jahr >= since_when, ]
  if (!('r1_id' %in% names(df_suf)) && 'ergg_1km' %in% names(df_suf)) {
    df_suf[, r1_id := ergg_1km]
    common_vars_selected = c('r1_id', common_vars_selected)
    }
  # make English names
  var_dict = read.csv("data/variable-and-value_labels/variables-metadata_manually.csv")
  var_names_en = with(var_dict, var_en[match(common_vars_selected, var_de)])
  var_names_en = replace(var_names_en, is.na(var_names_en), common_vars_selected[is.na(var_names_en)])

  # rename all vars in the data set
  setnames(df_suf, common_vars_selected, var_names_en)

  ## write to disk
  prefix = names(dlist)[[h]]
  suffix = df_suf[, paste(range(year), collapse = "-")]
  fwrite(df_suf, sprintf("data/%s_%s.csv", prefix, suffix), nThread = getDTthreads() - 1L)
  message("`", prefix, "_", suffix, "` has been written.")
  rm(df_suf)
  gc()
}
