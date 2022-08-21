#' Demographic data
#'
#' @export


demo_data <- function(seed = candidate_number, n = 150, age_m = 20.19, age_sd = 2.5,
                      attr_m = 5.67, attr_sd = 1.34, d = 1, mark = FALSE, messy = TRUE) {
  df <- red_data(seed = seed, n = n, age_m = age_m, age_sd = age_sd,
                        attr_m = attr_m, attr_sd = attr_sd, d = d, mark = TRUE, messy = messy)[ , 1:2]
  df$gender <- sample(1:4, nrow(df), replace = TRUE, prob = c(.505, .41, .065, .02))
  df$withdraw <- NA
  df$withdraw[sample(1:nrow(df), sample(4:7, 1))] <- "Yes"

  if (!mark) {
    var_names <- list(
      id = c("id", "id_code"),
      age = c("age", "years", "age_years"),
      gender = c("gender", "gen"),
      withdraw = c("withdraw", "delete", "remove")
    )
    names(df) <- unname(unlist(lapply(var_names, sample, 1)))
    if (sample(c(T, F), 1)) names(df) <- toupper(names(df))
    df <- df[ , c(1, sample(2:3), 4)]
  }
  return(df)
}
