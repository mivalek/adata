#' Red data
#'
#' Simulates data based on Elliot, et al. (2010)
#'
#' Elliot, A. J., Niesta Kayser, D., Greitemeyer, T., Lichtenfeld, S., Gramzow, R. H., Maier, M. A., & Liu, H. (2010). Red, rank, and romance in women viewing men. Journal of Experimental Psychology: General, 139(3), 399.

red_data <- function(seed = candidate_number,
                     n = 60, age_m = 20.19, age_sd = 2.5, attr_m = 5.67, attr_sd = 1.34, d = 1, mark = F) {
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


  length(data)
  typo <- sample(n, 1)
  typo_cat <- data$condition[typo] + 1
  data$condition[typo] <- 3

  cond_lab <- c("control", "experimental")
  data$condition <- factor(data$condition, labels = c(
    sample(cond_lab),
    # randomly remove a letter from cond_lab[typo_cat] and make that label of data$condition == 3
    paste(unlist(str_split(cond_lab[typo_cat], ""))[-sample(nchar(cond_lab[typo_cat]), 1)], collapse = "")))


  # introduce a 2-9[qwertyuio] sting in age
  typo_age <- as.character(sample(unique(data$id), 1))
  data$age[data$id == typo_age][sample(1:3, 1)] <- paste0(str_split(data$age[data$id == typo_age][1], "", simplify = T)[1],
                                                          sample(str_split("qwertyuio", "", simplify = T), 1))
  age_na <- as.character(sample(setdiff(unique(data$id), typo_age), sample(4:7, 1)))
  data$age[data$id %in% age_na] <- NA
  minors <- sample(setdiff(unique(data$id), c(typo_age, age_na)), sample(1:3, 1))
  data$age[data$id %in% minors] <- sample(1:14, length(minors))

  if (!mark) {
    var_names <- list(
      id = c("id", "id_code"),
      age = c("age", "years", "age_years"),
      condition = c("condition", "group"),
      trial = c("trial", "trial_n", "trial_N"),
      rating = c("rating", "score")
    )

    names(data) <- unname(unlist(lapply(var_names, sample, 1)))
    if (sample(c(T, F), 1)) names(data) <- toupper(names(data))
    data <- data[ , c(1, sample(2:3), sample(4:5))]
  }
  return(data)
}
