# Run from the repository root after installing shinylive.
source("tests/test_predictions.R")
shinylive::export("app", "docs", template_params = list(title = "Week 12 CDAI Explorer"))
file.create("docs/.nojekyll")
