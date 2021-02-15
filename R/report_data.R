#' Datasets for AnD 2021 lab report

red2021 <- function(seed = candidate_number, n = 150, age_m = 20.19, age_sd = 2.5,
                      attr_m = 5.67, attr_sd = 1.34, d = 1.5, mark = FALSE, messy = FALSE) {
  adata::red_data(seed = seed, n = n, age_m = age_m, age_sd = age_sd,
                 attr_m = attr_m, attr_sd = attr_sd, d = d, mark = mark, messy = messy)[ , c(1, 4:6)]
}

green2021 <- function(seed = candidate_number, n = 168, age_m = 20.19,
                      age_sd = 2.5, es = .05, mark = FALSE, messy = FALSE) {
  adata::green_data(seed = seed, n = n, age_m = age_m, age_sd = age_sd,
           es = es, mark = mark, messy = messy)[ , c(1, 4:6)]
}
