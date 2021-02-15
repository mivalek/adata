#' Red data
#'
#' Simulates data based on Elliot, et al. (2010)
#'
#' Elliot, A. J., Niesta Kayser, D., Greitemeyer, T., Lichtenfeld, S., Gramzow, R. H., Maier, M. A., & Liu, H. (2010). Red, rank, and romance in women viewing men. Journal of Experimental Psychology: General, 139(3), 399.

red_data <- function(seed = candidate_number, n = 150, age_m = 20.19, age_sd = 2.5,
                     attr_m = 5.67, attr_sd = 1.34, d = 1, mark = FALSE, messy = TRUE) {
  RNGkind(sample.kind = "Rejection") # set correct RNG kind
  set.seed(seed)

  age_m <- age_m + rnorm(1, 0, .2)
  age_sd <- age_sd + rnorm(1, 0, .2)
  attr_m <- attr_m  + rnorm(1, 0, .3)
  attr_sd <- attr_sd  + rnorm(1, 0, .05)
  diff <- d + rnorm(1, 0, .3)

  n_ctrl <- sample((n/2-5):(n/2+5), 1)
  n_exp <- n - n_ctrl

  data <- tibble::tibble(
    id = factor(rep(replicate(n, paste(sample(LETTERS, 4, replace = T), collapse = "")), each = 3)),
    age = rep(round(rnorm(n, age_m, age_sd)), each = 3),
    condition = rep(rep(0:1, c(n_ctrl, n_exp)), each = 3),
    item = rep(1:3, n),
    rating = round(rnorm(n * 3, attr_m + condition * diff, attr_sd)))

  data$rating[data$rating > 9] <- 9
  data$rating[data$rating < 1] <- 1

  data <- tidyr::pivot_wider(data,
                             c(id, age, condition),
                             names_from = item,
                             names_prefix = "item_",
                             values_from = rating)

  if (messy) {
    typo <- sample(n, 1)
    typo_cat <- data$condition[typo] + 1
    data$condition[typo] <- 3

    cond_lab <- sample(c("control", "experimental"))
    data$condition <- factor(data$condition, labels = c(
      cond_lab,
      # randomly remove a letter from cond_lab[typo_cat] and make that label of data$condition == 3
      paste(unlist(stringr::str_split(cond_lab[typo_cat], ""))[-sample(nchar(cond_lab[typo_cat]), 1)], collapse = "")))
    data$condition <- factor(as.character(data$condition))


    # introduce a 2-9[qwertyuio] sting in age
    typo_age <- as.character(sample(data$id, 1))
    data$age[data$id == typo_age] <- paste0(
      stringr::str_split(data$age[data$id == typo_age][1], "", simplify = T)[1],
      sample(stringr::str_split("qwertyuio", "", simplify = T), 1))
    age_na <- as.character(sample(setdiff(data$id, typo_age), sample(2:3, 1)))
    data$age[data$id %in% age_na] <- NA
    minors <- sample(setdiff(data$id, c(typo_age, age_na)), sample(1:2, 1))

    data$age[data$id %in% minors] <- sample(1:14, length(minors))
  } else {
    cond_lab <- sample(c("control", "experimental"))
    data$condition <- factor(data$condition, labels = cond_lab)
  }

  if (!mark) {
    var_names <- list(
      id = c("id", "id_code"),
      age = c("age", "years", "age_years"),
      condition = c("condition", "group")
    )

    names(data)[1:3] <- unname(unlist(lapply(var_names, sample, 1)))
    names(data) <- gsub("item", sample(c("item", "question"), 1), names(data))
    if (sample(c(T, F), 1)) names(data) <- toupper(names(data))
    data <- data[ , c(1, sample(2:3), 4:6)]
  }
  return(data)
}
