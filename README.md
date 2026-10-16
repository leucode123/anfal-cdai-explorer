# Week 12 CDAI Explorer

A Shiny research app for exploring the five-group week-12 CDAI models in manuscript V2 dated 16 October 2026. It supports the ratio-based model and the absolute-count model, with the same clinical covariates and fixed coefficients as the analysis.

The app runs locally in R or in the browser through Shinylive and GitHub Pages. It does not refit models, load a patient dataset, collect identifiers, or store entered values. Only fixed aggregate coefficients, age spline knots, outcome boundaries, and development ranges are included.

## Run locally

Install `shiny`, then run:

```r
shiny::runApp("app")
```

## Build the browser version

Install `shinylive`, then run from the repository root:

```r
shinylive::export("app", "docs", template_params = list(title = "Week 12 CDAI Explorer"))
httpuv::runStaticServer("docs")
```

GitHub Pages serves the generated `docs/` directory. To publish an update, run the tests, rebuild `docs/`, and commit the source and generated files. Patient data and deployment credentials are not required.

## Models and interpretation

Both proportional odds models adjust for age, sex, baseline CDAI, and prior IBD-related surgery. Age uses a three-knot restricted cubic spline at 18, 26, and 43 years with `rms` normalization. Model 1 uses natural-log NLR, LMR, and PLR. Model 2 uses untransformed neutrophil, platelet, monocyte, and lymphocyte counts.

The outcome groups are CDAI <=23.2, >23.2 to <=62, >62 to <=108.8, >108.8 to <=185, and >185. Missing outcomes were excluded when fitting the two models, each with 94 complete cases. Fixed parameters reflect the corrected PLR analysis.

These are exploratory internally validated models. Bootstrap-corrected C-statistics were 0.649 and 0.651, and diagnostics suggested proportional odds departures. External validation and evaluation of clinical utility are required. Predictions describe CDAI groups and do not estimate treatment failure or recommend treatment changes.

## Numerical verification

The prediction function reproduces `rms::predict(..., type = "fitted.ind")`. Tests cover synthetic profiles, spline knots, boundary inputs, invalid and missing inputs, both sex and surgery levels, and probability sums. A private author-side verification additionally checks the development cases against the fitted V2 models. Patient-level verification data and fitted RDS objects are not in this repository.

Run the public tests with:

```sh
Rscript tests/test_predictions.R
```

`app/model_parameters.R` is generated from author-held fitted models using `scripts/export_models.R`. The file uses exact R numeric literals to preserve coefficient precision. Regenerate it rather than editing coefficients by hand. Deployment credentials, research datasets, and fitted model objects must remain outside this repository.
