#' Green data
#'
#' Simulates data based on Griskevicius, Tybur, & Van den Bergh (2010)
#'
#' Griskevicius, V., Tybur, J. M., & Van den Bergh, B. (2010). Going green to be seen: status, reputation, and conspicuous conservation. Journal of personality and social psychology, 98(3), 392.

green_data <- function(seed = candidate_number,
                       n = 168, age_m = 20.19, age_sd = 2.5, es = .15, mark = F) {
  set.seed(seed)

  age_m <- age_m + rnorm(1, 0, .2)
  age_sd <- age_sd + rnorm(1, 0, .2)

  data <- tibble::tibble(
    id = factor(rep(replicate(n, paste(sample(LETTERS, 4, replace = T), collapse = "")), each = 3)),
    age = rep(round(rnorm(n, age_m, age_sd)), each = 3),
    gender = factor(rep(rbinom(n, 2, c(0.613, 0.363, 0.024)), each = 3),
                    labels = c(sample(c("male", "female")), "other")),
    condition = rep(0:1, each = n/2*3),
    product = factor(rep(0:2, n), labels = sample(c("car", "cleaner", "dishwasher"))),
    selection = 0)

  for (i in unique(data$condition)) {
    for (j in levels(data$product)) {
      data$selection[data$condition == i & data$product == j] <- rbinom(n/2, 1, .37 + (es + rnorm(1, 0, .02)) * i)
    }
  }

  length(data)
  typo <- sample(n, 1)
  typo_cat <- data$condition[typo] + 1
  data$condition[typo] <- 3

  cond_lab <- sample(c("control", "experimental"))
  data$selection <- factor(data$selection, labels = c("luxury", "green"))
  data$condition <- factor(data$condition, labels = c(
    cond_lab,
    # randomly remove a letter from cond_lab[typo_cat] and make that label of data$condition == 3
    paste(unlist(stringr::str_split(cond_lab[typo_cat], ""))[-sample(nchar(cond_lab[typo_cat]), 1)], collapse = "")))
  data$condition <- factor(as.character(data$condition))

  # introduce a 2-9[qwertyuio] sting in age
  typo_age <- as.character(sample(unique(data$id), 1))
  data$age[data$id == typo_age][sample(1:3, 1)] <- paste0(stringr::str_split(data$age[data$id == typo_age][1], "", simplify = T)[1],
                                                          sample(stringr::str_split("qwertyuio", "", simplify = T), 1))
  age_na <- as.character(sample(setdiff(unique(data$id), typo_age), sample(4:7, 1)))
  data$age[data$id %in% age_na] <- NA
  minors <- sample(setdiff(unique(data$id), c(typo_age, age_na)), sample(1:3, 1))
  data$age[data$id %in% minors] <- sample(1:14, length(minors))

  if (!mark) {
    var_names <- list(
      id = c("id", "id_code"),
      age = c("age", "years", "age_years"),
      gender = "gender",
      condition = c("condition", "group"),
      product = "product",
      selection = c("selection", "category", "product_type")
    )

    names(data) <- unname(unlist(lapply(var_names, sample, 1)))
    if (sample(c(T, F), 1)) names(data) <- toupper(names(data))
    data <- data[ , c(1, sample(2:3), 4, sample(5:6))]
  }
  return(data)
}
