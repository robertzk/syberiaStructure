context('syberia_models')

local({
  syberia_models <- force(syberia_models)
  environment(syberia_models)$is.syberia_project <- function(...) TRUE
  environment(syberia_models)$file.exists <- function(...) TRUE
  environment(syberia_models)$syberia_root <- function(...) ''

  test_that('it can discriminate between directoried and non-directoried models', {
    models <- c('model_one/model_one.r', 'model_one/helper.r', 'model_two.r')
    environment(syberia_models)$list.files <- function(...) models
    expect_identical(syberia_models(by_mtime = FALSE), models[c(1,3)])
  })

  test_that('it can use modified time to sort models', {
    models <- c("model1.r", "model2.r")
    environment(syberia_models)$list.files <- function(...) models
    environment(syberia_models)$file.info <- 
      function(f) list(mtime = as.integer(gsub('[^0-9]', '', f)))
    expect_identical(syberia_models(by_mtime = TRUE), rev(models))
  })

})

