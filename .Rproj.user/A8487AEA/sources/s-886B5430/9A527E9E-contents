#' Red data
#'
#' Simulates data based on Elliot, et al. (2010)
#'
#' Elliot, A. J., Niesta Kayser, D., Greitemeyer, T., Lichtenfeld, S., Gramzow, R. H., Maier, M. A., & Liu, H. (2010). Red, rank, and romance in women viewing men. Journal of Experimental Psychology: General, 139(3), 399.

red_data <- function(seed = candidate_number,
                     n = 60, age_m = 20.19, age_sd = 2.5, attr_m = 5.67, attr_sd = 1.34, d = 1) {
  set.seed(seed)

  age_m <- age_m + rnorm(1, 0, .2)
  age_sd <- age_sd + rnorm(1, 0, .2)
  attr_m <- attr_m  + rnorm(1, 0, .3)
  attr_sd <- attr_sd  + rnorm(1, 0, .05)
  diff <- d + rnorm(1, 0, .3)

  data <- tibble::tibble(
    id = factor(rep(replicate(n, paste(sample(LETTERS, 4, replace = T), collapse = "")), each = 21)),
    age = rep(round(rnorm(n, age_m, age_sd)), each = 21),
    condition = rep(rep(0:1, c(11, 10)), n),
    trial = rep(c(1:11, 1:10), n),
    rating = round(rnorm(n * 21, attr_m + condition * diff, attr_sd)))

  data$rating[data$rating > 9] <- 9
  data$rating[data$rating < 1] <- 1
  data$condition <- factor(data$condition, labels = c("control", "experimental"))

  return(data)
}
