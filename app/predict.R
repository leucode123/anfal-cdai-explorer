# The three-knot restricted cubic spline uses rms/Hmisc norm = 2.
age_spline <- function(age, knots) {
  k1 <- knots[1]; k2 <- knots[2]; k3 <- knots[3]
  (pmax(age - k1, 0)^3 - pmax(age - k2, 0)^3 * (k3 - k1) / (k3 - k2) +
     pmax(age - k3, 0)^3 * (k2 - k1) / (k3 - k2)) / (k3 - k1)^2
}

predict_cdai <- function(model_id, values) {
  if (!model_id %in% names(model_parameters$models)) stop("Choose a model.")
  model <- model_parameters$models[[model_id]]
  labels <- c(age_years = "Age", cdai_score_no = "Baseline CDAI", nlr_8 = "NLR", lmr_8 = "LMR",
              plr_8 = "PLR", neutrophile_8 = "Neutrophils", platelet_8 = "Platelets",
              lymphocyte_8 = "Lymphocytes", monocyte_8 = "Monocytes")
  for (v in names(model$limits)) {
    x <- values[[v]]; bounds <- model$limits[[v]]
    if (length(x) != 1L || !is.numeric(x) || !is.finite(x)) {
      stop(paste("Enter a valid value for", labels[v]), call. = FALSE)
    }
    if (x < bounds["min"] - 1e-12 || x > bounds["max"] + 1e-12) {
      stop(sprintf("%s must be within the development range (%g to %g).", labels[v], bounds["min"], bounds["max"]), call. = FALSE)
    }
  }
  if (!values$gender %in% c("Male", "Female")) stop("Choose male or female.")
  if (!values$previous_ibd_related_surge %in% c("No", "Yes")) stop("Choose the prior surgery status.")
  design <- c("gender=Female" = as.integer(values$gender == "Female"),
              age_years = values$age_years, "age_years'" = age_spline(values$age_years, model$age_knots),
              "previous_ibd_related_surge=Yes" = as.integer(values$previous_ibd_related_surge == "Yes"),
              cdai_score_no = values$cdai_score_no)
  if (model_id == "model1") {
    design <- c(design, plr_8 = log(values$plr_8), lmr_8 = log(values$lmr_8), nlr_8 = log(values$nlr_8))
  } else {
    design <- c(design, neutrophile_8 = values$neutrophile_8, platelet_8 = values$platelet_8,
                monocyte_8 = values$monocyte_8, lymphocyte_8 = values$lymphocyte_8)
  }
  eta <- sum(design * model$coefficients[names(design)])
  cumulative <- plogis(model$intercepts + eta)
  probabilities <- c(1 - cumulative[1], head(cumulative, -1) - tail(cumulative, -1), tail(cumulative, 1))
  if (any(!is.finite(probabilities)) || any(probabilities < 0) || abs(sum(probabilities) - 1) > 1e-12) {
    stop("The model could not calculate valid probabilities.")
  }
  unname(probabilities)
}
