#' Green data
#'
#' Simulates data based on Griskevicius, Tybur, & Van den Bergh (2010)
#'
#' Griskevicius, V., Tybur, J. M., & Van den Bergh, B. (2010). Going green to be seen: status, reputation, and conspicuous conservation. Journal of personality and social psychology, 98(3), 392.

green_data <- function(seed = candidate_number,
                       n = 168, age_m = 20.19, age_sd = 2.5, es = .15) {
  set.seed(seed)

  age_m <- age_m + rnorm(1, 0, .2)
  age_sd <- age_sd + rnorm(1, 0, .2)

  data <- tibble::tibble(
    id = factor(rep(replicate(n, paste(sample(LETTERS, 4, replace = T), collapse = "")), each = 3)),
    age = rep(round(rnorm(n, age_m, age_sd)), each = 3),
    gender = factor(rep(rbinom(n, 2, c(0.613, 0.363, 0.024)), each = 3),
                    labels = c("male", "female", "other")),
    condition = rep(0:1, each = n/2*3),
    product = factor(rep(0:2, n), labels = c("car", "cleaner", "dishwasher")),
    selection = 0)

  for (i in unique(data$condition)) {
    for (j in levels(data$product)) {
      data$selection[data$condition == i & data$product == j] <- rbinom(n/2, 1, .37 + (es + rnorm(1, 0, .02)) * i)
    }
  }

  data$selection <- factor(data$selection, labels = c("luxury", "green"))
  data$condition <- factor(data$condition, labels = c("control", "experimental"))
  return(data)
}
