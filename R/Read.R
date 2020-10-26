### #
### # Read Solfix File of MiX99 and CSV-File with effect key and code
### # 2020-06-18 (skn)
### # ---------------------------------------------------------------


#' @title Read Solfix File of MiX99
#' @description
#' Read Solfix file generated with MiX99 where the solution
#' of the different fixed effect are in this file.
#'
#' @param pvec_dir vector of directory to Solfix
#' @return tbl_sol_fix with solution of the fixed effects
#'
#' @export read_solfix
read_solfix <- function(pvec_dir){

  #Reading The Solutions from MiX99
  s_solfix_path <- file.path(pvec_dir, "Solfix")
  if (!file.exists(s_solfix_path)) stop(" *** * ERROR: Cannot find solfix file: ", s_solfix_path)
  tbl_sol_fix <- readr::read_delim(file = s_solfix_path, delim = " ")

  # Remove leading and trailing spaces from column names.
  colnames(tbl_sol_fix) <- purrr::map(colnames(tbl_sol_fix), function(x) gsub(pattern = "\\s+", replacement = "", x))

  # Remove spaces from entries in the tibble.
  tbl_sol_fix <- purrr::modify(tbl_sol_fix,  function(x) gsub(pattern = "\\s+", replacement = "", x))

  # Data-types for the columns `Fact.`, `Trt`, `Level`, `N-Obs` and `Solution` are changed.
  tbl_sol_fix$Fact. <- as.integer(tbl_sol_fix$Fact.)
  tbl_sol_fix$Trt <- as.integer(tbl_sol_fix$Trt)
  tbl_sol_fix$Level <- as.integer(tbl_sol_fix$Level)
  tbl_sol_fix$`N-Obs` <- as.integer(tbl_sol_fix$`N-Obs`)
  tbl_sol_fix$Solution <- as.double(tbl_sol_fix$Solution)

  # return the resulting tibble
  return(tbl_sol_fix)

}



#' @title Read CSV-file generate in carcass-process with effect key and code
#' @description
#' Read csv-file generated from EBV-carcass-process
#' countaining the effect key and code.
#'
#' @param pvec_path vector of path to csv-file
#' @return tbl_level_code with code and key of the fixed effects
#'
#' @export read_level_code
read_level_code <- function(pvec_path){

  #Reading The Effect Code and Key
  if (!file.exists(pvec_path)) stop(" *** * ERROR: Cannot find fixed effect file: ", pvec_path)
  tbl_level_code <- readr::read_delim(file = pvec_path, delim = ";")

  # return the resulting tibble
  return(tbl_level_code)

}


