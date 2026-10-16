source("app/model_parameters.R")
source("app/predict.R")
source("tests/synthetic_cases.R")

for (model_id in names(synthetic_cases)) {
  cases <- synthetic_cases[[model_id]]
  for (i in seq_along(cases$profiles)) {
    actual <- predict_cdai(model_id, cases$profiles[[i]])
    stopifnot(length(actual) == 5L, all(is.finite(actual)), all(actual >= 0),
              abs(sum(actual) - 1) < 1e-12,
              max(abs(actual - cases$probabilities[i, ])) < 1e-12)
  }
  profile <- cases$profiles[[2]]
  for (field in names(model_parameters$models[[model_id]]$limits)) {
    bounds <- model_parameters$models[[model_id]]$limits[[field]]
    for (value in list(NA_real_, Inf, -Inf, NULL, "invalid", bounds["min"] - 1, bounds["max"] + 1)) {
      invalid <- profile
      invalid[field] <- list(value)
      failed <- inherits(try(predict_cdai(model_id, invalid), silent = TRUE), "try-error")
      stopifnot(failed)
    }
  }
  for (field in c("gender", "previous_ibd_related_surge")) {
    invalid <- profile; invalid[[field]] <- "unknown"
    stopifnot(inherits(try(predict_cdai(model_id, invalid), silent = TRUE), "try-error"))
  }
}
stopifnot(all(diff(model_parameters$group_boundaries) > 0),
          length(model_parameters$group_labels) == 5L,
          model_parameters$development_n == 94L)
cat("PASS: both models match rms reference probabilities; invalid and missing inputs are rejected.\n")
