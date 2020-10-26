### #
### # Joint solution and effect key-code
### # 2020-06-18 (skn)
### # ---------------------------------------------------------------


#' @title Get for an effect key/code the solutions
#' @description
#' Read csv-file generate from EBV-carcass-process
#' countaining the effect key and code.
#'
#' @param ptbl_sol_fix tibble with solution of the fixed effect
#' @param ptbl_level_code tiblle with code and key of the fixed effect
#' @param psTrait name of the trait
#' @param psEffectnameInSolani name of the fixed effect in the Solani-file
#' @return tbl_sol_fx_trt with solution as well as code and key of the fixed effects
#'
#' @export getcarcass_sol_fx_trait
getcarcass_sol_fx_trait <- function(ptbl_sol_fix,
                                    ptbl_level_code,
                                    psTrait,
                                    psEffectnameInSolani){

  #Required library
  require(dplyr)

  # Filter solution per effect and trait
  tbl_sol_fx_trt <- ptbl_sol_fix %>% filter(Factor == psEffectnameInSolani & Trait == psTrait)

  # Merge The Levels To the Solutions Based on Code-Level-Mapping
  tbl_sol_fx_trt <- tbl_sol_fx_trt %>% inner_join(ptbl_level_code, by = c('Level' = 'EffectCode'))

  # return the resulting tibble
  return(tbl_sol_fx_trt)

}

