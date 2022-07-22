library(data.table)
library(haven, include.only = 'read_dta')
source("script/helpers/base-helpers.R")
source("script/helpers/utils.R")
# params
since_when = 2016 # keep observations since
extract_raw_files = FALSE

# extract if not available
## caution: utils::unzip may truncate extraction if the size is > 4GB so don't rely on it
## better to do it with the command line or manually with 7zip etc
zipfile = "../Common-Data/RED_v6.1.zip"
extractdir = paste0("../Common-Data/.", file.stem(zipfile))

if (extract_raw_files) {
  # o for overwrite d for directory
  system(sprintf("unzip -o -d %s %s", extractdir, zipfile))
  }


## Stata files ----
dlist = c(
  `homes-sales` = "HK_SUF",
  `homes-rents` = "HM_SUF",
  `apartments-sales` = "WK_SUF",
  `apartments-rents` = "WM_SUF"
)
chosen_zipfile = paste0(extractdir, "/Stata.zip")
zips_in_chosen = paste0(extractdir, file.stem(chosen_zipfile), '/', dlist, ".zip")
if (extract_raw_files & !dir.exists(tools::file_path_sans_ext(chosen_zipfile)))
  system(sprintf('unzip -o -d %s %s', dirname(chosen_zipfile), chosen_zipfile))
for (zfile in homes_zipfile) {
  if (extract_raw_files)
    if (file.exists(zfile)) {
      system(sprintf("unzip -o -d %s %s", dirname(zfile), zfile))
    } else {
        warning(sprintf('The zip file `%s` does not exist, no extraction has been made.', zfile))
      }
}

# iterate over homes/apartments for sale (HK_SUF,WK_SUF) and -----
# homes/apartments for rent (HM_SUF,WM_SUF)

# have to choose either the .csv or .dta files
## the former comes with text labels for missing values, the latter can come with or without text
# here chose to work with Stata ohneText data sets
for (h in seq_along(dlist)) {
  file_list = dir(
    path = paste0(extractdir, "/Stata/ohneText/", dlist[[h]], "_ohneText/"),
    pattern = paste0(dlist[[h]], "_ohneText([0-9]+)?[.]dta$"),
    full.names = TRUE
  )
  if (length(file_list) == 0) stop('No list of files found.', call. = FALSE)
  # sorting was necessary because HKSUF10 was coming second instead of 10th
  file_list = file_list[order(as.integer(regmatches(basename(file_list), regexpr("[0-9]+", basename(file_list)))))]
  names(file_list) = tools::file_path_sans_ext(basename(file_list))
  var_list = lapply(file_list, read_dta, n_max = 10) |> lapply(names)
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
  yearvar = lapply(file_list, \(f) names(read_dta(f, n_max = 1L))) |>
    unlist() |>
    grep('^[e]?jahr$', x = _, value = TRUE)
  yearvar = if ("jahr" %in% yearvar) {
  "jahr"
} else if ("ejahr" %in% yearvar) {
  "ejahr"
} else {
  stop("No `year` variable in the data set.", call. = FALSE)
}
  years_in_dataset = lapply(file_list, \(f) unique(read_dta(f, col_select = (yearvar)))) |>
    rbindlist(idcol = "file")

  # part of since_when could be in a data set that contains since_when-1
  selected = names(file_list)[names(file_list) %in% years_in_dataset[get(yearvar) >= since_when, file]]
  # make explicitly NA
  all_nas_patt = c(
    "Variable trifft auf diesen Datensatzyp nicht zu|Variable for other types only",
    "Variable nicht mehr in Einagbemaske vorhanden|Old variable \\(no longer part of the platform\\)"
  )
  all_nas_num = c(-8, -10) # for ohneText data
  gc()
  df_suf = lapply(file_list[selected], read_dta)
  df_suf = lapply(df_suf, \(d) {
    # variables that should be dropped
    drop = names(d)[vapply(
      d[sample.int(nrow(d), 100), ],
      \(x) all(grepl(paste(all_nas_num, collapse = "|"), x)), NA
    )]
    d[, (setdiff(names(d), drop))]
  })
  common_vars_selected = Reduce(intersect, lapply(df_suf, names))
  # drop vars that are not available across data sets
  df_suf = lapply(df_suf, \(x) x[, (intersect(names(x), common_vars_selected))])
  df_suf = do.call(rbind, df_suf)
  df_suf = df_suf[df_suf[[yearvar]] >= since_when, ] # injection
  # make English names
  var_dict = read.csv("data/variable-and-value_labels/variables-metadata_manually.csv")
  var_names_en = with(var_dict, var_en[match(common_vars_selected, var_de)])
  var_names_en = replace(var_names_en, is.na(var_names_en), common_vars_selected[is.na(var_names_en)])

  # rename all vars in the data set
  names(df_suf)[match(common_vars_selected, names(df_suf))] = var_names_en
  if (yearvar == 'ejahr' && 'ad_end_year' %in% names(df_suf) && !('year' %in% names(df_suf))){
    df_suf$year = df_suf$ad_end_year
}
  ## write to disk
  prefix = names(dlist)[[h]]
  suffix = paste(range(df_suf$year), collapse = "-")
  fwrite(df_suf, sprintf("data/%s_%s.csv", prefix, suffix), nThread = getDTthreads()-1L)
  message('`', prefix, suffix, '` has been written.')
  rm(df_suf)
  gc()
}
