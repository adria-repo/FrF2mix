#' Build a pseudo-mixture design based on an orthogonal array
#'
#' Internal function.
#' The function builds a pseudo-mixture design based on an orthogonal array with mixture constraints.
#'
#' @param data A regular fractional factorial 2-levels designs.
#' @param Total An integer giving the constant total amount of the sum of mixture components in the design.
#' @param lc_Ingredient1 An integer giving the lower constraint on ingredient 1.
#' @param uc_Ingredient1 An integer giving the upper constraint on ingredient 1.
#' @param lc_Ingredient2 An integer giving the lower constraint on ingredient 2.
#' @param uc_Ingredient2 An integer giving the upper constraint on ingredient 2.
#' @param lc_Ingredient3 An integer giving the lower constraint on ingredient 3.
#' @param uc_Ingredient3 An integer giving the upper constraint on ingredient 3.
#'
#' @return A data frame
#'
#' @keywords internal
#'
#' @examples \dontrun{
#' .pseudo.mixture(data = Dinit,
#'                 Total = Total,
#'                 lc_Ingredient1 = lc.Ingredient1, uc_Ingredient1 = uc.Ingredient1,
#'                 lc_Ingredient2 = lc.Ingredient2, uc_Ingredient2 = uc.Ingredient2,
#'                 lc_Ingredient3 = lc.Ingredient3, uc_Ingredient3 = uc.Ingredient3)
#'}
.pseudo.mixture <- function(data = NULL, Total = NULL,
                            lc_Ingredient1 = NULL, uc_Ingredient1 = NULL,
                            lc_Ingredient2 = NULL, uc_Ingredient2 = NULL,
                            lc_Ingredient3 = NULL, uc_Ingredient3 = NULL){

  res <- as.data.frame(data) %>%
    dplyr::mutate(Ingredient1 = ifelse(ingredient1 %in% "-", lc_Ingredient1, uc_Ingredient1),
                  Ingredient2 = ifelse(ingredient2 %in% "-", lc_Ingredient2, uc_Ingredient2),
                  Ingredient3 = ifelse(ingredient3 %in% "-", lc_Ingredient3, uc_Ingredient3)) %>%
    dplyr::mutate(delta = Ingredient1 + Ingredient2 + Ingredient3 - Total) %>%
    dplyr::mutate(Ingredient1_f = ifelse(delta < 0 & ingredient1 %in% "+", Ingredient1,
                                         ifelse(delta > 0 & ingredient1 %in% "-", Ingredient1, NA))) %>%
    dplyr::mutate(Ingredient2_f = ifelse(delta < 0 & ingredient2 %in% "+", Ingredient2,
                                         ifelse(delta > 0 & ingredient2 %in% "-", Ingredient2, NA))) %>%
    dplyr::mutate(Ingredient3_f = ifelse(delta < 0 & ingredient3 %in% "+", Ingredient3,
                                         ifelse(delta > 0 & ingredient3 %in% "-", Ingredient3, NA)))

  return(res)
}
