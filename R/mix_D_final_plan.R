#' Transform the pseudo-mixture design into a mixture design
#'
#' Internal function.
#' The function transforms the pseudo-mixture design into a mixture design.
#'
#' @param data The pseudo-mixture design from .pseudo.mixture function.
#' @param mix_plan The mixture design from mix_design function.
#' @param lc_Ingredient1 An integer giving the lower constraint on ingredient 1.
#' @param uc_Ingredient1 An integer giving the upper constraint on ingredient 1.
#' @param lc_Ingredient2 An integer giving the lower constraint on ingredient 2.
#' @param uc_Ingredient2 An integer giving the upper constraint on ingredient 2.
#' @param lc_Ingredient3 An integer giving the lower constraint on ingredient 3.
#' @param uc_Ingredient3 An integer giving the upper constraint on ingredient 3.
#'
#' @return A data frame.
#'
#' @keywords internal
#'
#' @examples \dontrun{
#' .mix_D_final_plan(Dinter[.x, ],
#'                   mix_plan = mixture,
#'                   lc_Ingredient1 = lc.Ingredient1, uc_Ingredient1 = uc.Ingredient1,
#'                   lc_Ingredient2 = lc.Ingredient2, uc_Ingredient2 = uc.Ingredient2,
#'                   lc_Ingredient3 = lc.Ingredient3, uc_Ingredient3 = uc.Ingredient3)
#' }
.mix_D_final_plan <- function(data = NULL, mix_plan = NULL,
                              lc_Ingredient1 = NULL, uc_Ingredient1 = NULL,
                              lc_Ingredient2 = NULL, uc_Ingredient2 = NULL,
                              lc_Ingredient3 = NULL, uc_Ingredient3 = NULL){

  res_ingredient11 = mix_plan
  if(is.na(data$Ingredient1_f)) {res_ingredient12 = res_ingredient11 %>%
    dplyr::filter(Ingredient1_f > lc_Ingredient1 & Ingredient1_f < uc_Ingredient1) %>%
    dplyr::select(Ingredient1_f)} else{
      res_ingredient12 = data %>%
        dplyr::select(Ingredient1_f)
    }


  res_ingredient21 = res_ingredient11 %>%
    dplyr::filter(Ingredient1_f %in% res_ingredient12$Ingredient1_f)
  if(is.na(data$Ingredient2_f)) {res_ingredient22 = res_ingredient21 %>%
    dplyr::filter(Ingredient2_f > lc_Ingredient2 & Ingredient2_f < uc_Ingredient2) %>%
    dplyr::select(Ingredient2_f)} else{
    res_ingredient22 = data %>%
      dplyr::select(Ingredient2_f)
  }

  res_ingredient31 = res_ingredient21 %>%
    dplyr::filter(Ingredient2_f %in% res_ingredient22$Ingredient2_f)
  if(is.na(data$Ingredient3_f)) {res_ingredient32 = res_ingredient31 %>%
    dplyr::filter(Ingredient3_f > lc_Ingredient3 & Ingredient3_f < uc_Ingredient3) %>%
    dplyr::select(Ingredient3_f)} else{
    res_ingredient32 = data %>%
      dplyr::select(Ingredient3_f)
  }

  res = res_ingredient31 %>%
    dplyr::filter(Ingredient3_f %in% res_ingredient32$Ingredient3_f) %>%
    dplyr::mutate(Df_Ingredient1_f = Ingredient1_f, Df_Ingredient2_f = Ingredient2_f, Df_Ingredient3_f = Ingredient3_f) %>%
    dplyr::select(Df_Ingredient1_f, Df_Ingredient2_f, Df_Ingredient3_f)

  return(res)
}
