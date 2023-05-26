#' Build an extreme vertices design in a constrained mixture space
#'
#' Internal function.
#' This function behaves just like Xvert() from mixexp.
#' The main difference is that the result is expressed in content in the complete recipe and not in relative proportion in the mixing plan.
#'
#' @param Total An integer giving the constant total amount of the sum of mixture components in the design.
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
#' .mix_design(Total = Total,
#'             lc_Ingredient1 = lc.Ingredient1, uc_Ingredient1 = uc.Ingredient1,
#'             lc_Ingredient2 = lc.Ingredient2, uc_Ingredient2 = uc.Ingredient2,
#'             lc_Ingredient3 = lc.Ingredient3, uc_Ingredient3 = uc.Ingredient3)
#' }
.mix_design <- function(Total = NULL,
                       lc_Ingredient1 = NULL, uc_Ingredient1 = NULL,
                       lc_Ingredient2 = NULL, uc_Ingredient2 = NULL,
                       lc_Ingredient3 = NULL, uc_Ingredient3 = NULL){

  cte <- Total / 100

  res <- mixexp::Xvert(nfac = 3,
                       lc = c(lc_Ingredient1 / 100 / cte, lc_Ingredient2 / 100 / cte, lc_Ingredient3 / 100 / cte),
                       uc = c(uc_Ingredient1 / 100 / cte, uc_Ingredient2 / 100 / cte, uc_Ingredient3 / 100 / cte),
                       pseudo = F, ndm = 1)

  res_f <- round(res * 100 * cte, 2) %>%
    dplyr::mutate(Ingredient1_f = x1,
           Ingredient2_f = x2,
           Ingredient3_f = x3) %>%
    dplyr::select(Ingredient1_f, Ingredient2_f, Ingredient3_f)

  return(res_f)

}
