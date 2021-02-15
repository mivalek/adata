#' Demographic data

demo_data <- function(seed = candidate_number, n = 150, age_m = 20.19, age_sd = 2.5,
                      attr_m = 5.67, attr_sd = 1.34, d = 1, mark = FALSE, messy = TRUE) {
  df <- red_data(seed = seed, n = n, age_m = age_m, age_sd = age_sd,
                        attr_m = attr_m, attr_sd = attr_sd, d = d, mark = mark, messy = messy)[ , 1:2]
  df$gender <- sample(1:4, nrow(df), replace = TRUE, prob = c(.505, .41, .065, .02))
  df$withdraw <- NA
  df$withdraw[sample(1:nrow(df), sample(4:7, 1))] <- "Yes"
  names(df) <- if (sample(0:1, 1)) {tolower(names(df))} else {toupper(names(df))}
  return(df)
}
