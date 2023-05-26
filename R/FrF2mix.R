#' Build a fractional factorial design with mixture constraints
#'
#' The function edits a DoE combining process and mixing factors, easily dealing with any mixture constraints for 3 ingredients within the framework of fractional factorial designs.
#'
#' @param nruns A number of runs, must be a power of 2 (4 to 4096), if given.
#' @param process.factor.names A list a nfactors factor names.
#' @param Total An integer giving the constant total amount of the sum of mixture components in the design.
#' @param lc.Ingredient1 An integer giving the lower constraint on ingredient 1.
#' @param uc.Ingredient1 An integer giving the upper constraint on ingredient 1.
#' @param lc.Ingredient2 An integer giving the lower constraint on ingredient 2.
#' @param uc.Ingredient2 An integer giving the upper constraint on ingredient 2.
#' @param lc.Ingredient3 An integer giving the lower constraint on ingredient 3.
#' @param uc.Ingredient3 An integer giving the upper constraint on ingredient 3.
#'
#' @return A list with the fractional factorial design with mixture constraints and the reminder of the regular fractional factorial 2-levels designs.
#' @export
#'
#' @examples \dontrun{
#' FrF2mix(nruns = 16,
#'         process.factor.names = list(time.hydration = c("30s","2min30s"),
#'                                     speed.hydration = c("V1", "V3"),
#'                                     time.emulsification = c("1min30s", "3min30s"),
#'                                     speed.emulsification = c("V1", "V3")
#'         ),
#'         Total = 91.24,
#'         lc.Ingredient1 = 1, uc.Ingredient1 = 5,
#'         lc.Ingredient2 = 20, uc.Ingredient2 = 75,
#'         lc.Ingredient3 = 15, uc.Ingredient3 = 75)
#' }
FrF2mix <- function(nruns = NULL, process.factor.names = NULL,
                    Total = NULL,
                    lc.Ingredient1 = NULL, uc.Ingredient1 = NULL,
                    lc.Ingredient2 = NULL, uc.Ingredient2 = NULL,
                    lc.Ingredient3 = NULL, uc.Ingredient3 = NULL){

  Dinit <- FrF2::FrF2(nruns = nruns, nfactors =  length(process.factor.names) + 3,
                      default.levels = c("-", "+"),
                      factor.names = c(
                        process.factor.names,
                        list(ingredient1 = "",
                             ingredient2 = "",
                             ingredient3 = ""
                        )
                      ),
                      seed = 1986,
                      alias.info = 3)

  Dinter <- .pseudo.mixture(data = Dinit, Total = Total,
                           lc_Ingredient1 = lc.Ingredient1, uc_Ingredient1 = uc.Ingredient1,
                           lc_Ingredient2 = lc.Ingredient2, uc_Ingredient2 = uc.Ingredient2,
                           lc_Ingredient3 = lc.Ingredient3, uc_Ingredient3 = uc.Ingredient3)

  mixture <- .mix_design(Total = Total,
                        lc_Ingredient1 = lc.Ingredient1, uc_Ingredient1 = uc.Ingredient1,
                        lc_Ingredient2 = lc.Ingredient2, uc_Ingredient2 = uc.Ingredient2,
                        lc_Ingredient3 = lc.Ingredient3, uc_Ingredient3 = uc.Ingredient3)


  Dfinal <- purrr::map_dfr(
    1:nrow(Dinter),
    ~ .mix_D_final_plan(Dinter[.x, ], mix_plan = mixture,
                       lc_Ingredient1 = lc.Ingredient1, uc_Ingredient1 = uc.Ingredient1,
                       lc_Ingredient2 = lc.Ingredient2, uc_Ingredient2 = uc.Ingredient2,
                       lc_Ingredient3 = lc.Ingredient3, uc_Ingredient3 = uc.Ingredient3)
  )

  Plan <- cbind(Dinter, Dfinal) %>%
    tibble::rownames_to_column("Id") %>%
    dplyr::select(Id, names(process.factor.names), Df_Ingredient1_f, Df_Ingredient2_f, Df_Ingredient3_f)

  return(list(Plan = Plan, Dinit = Dinit))
}
