#' Datasets for AnD 2021 lab report
#'
#' @export

red2021 <- function(seed = candidate_number, n = 150, age_m = 20.19, age_sd = 2.5,
                      attr_m = 5.67, attr_sd = 1.34, d = 1.5, mark = FALSE, messy = FALSE) {
  data <- adata::red_data(seed = seed, n = n, age_m = age_m, age_sd = age_sd,
                 attr_m = attr_m, attr_sd = attr_sd, d = d, mark = TRUE, messy = messy)[ , c(1, 3:6)]

  if (!mark) {
    var_names <- list(
      id = c("id", "id_code"),
      condition = c("condition", "group")
    )

    names(data)[1:2] <- unname(unlist(lapply(var_names, sample, 1)))
    names(data) <- gsub("item", sample(c("item", "question"), 1), names(data))
    if (sample(c(T, F), 1)) names(data) <- toupper(names(data))
  }
  return(data)
}

green2021 <- function(seed = candidate_number, n = 168, age_m = 20.19,
                      age_sd = 2.5, es = .05, mark = FALSE, messy = FALSE) {
  adata::green_data(seed = seed, n = n, age_m = age_m, age_sd = age_sd,
           es = es, mark = mark, messy = messy)[ , c(1, 4:6)]
}
