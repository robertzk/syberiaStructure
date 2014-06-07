context('syberia_models')

local({
  syberia_objects <- force(syberia_objects)
  environment(syberia_objects) <- new.env(parent = environment(syberia_objects))
  environment(syberia_objects)$is.syberia_project <- function(...) TRUE
  environment(syberia_objects)$file.exists <- function(...) TRUE
  environment(syberia_objects)$syberia_root <- function(...) ''
  environment(syberia_models)$syberia_objects <- syberia_objects

  test_that('it can discriminate between directoried and non-directoried models', {
    models <- c('model_one/model_one.r', 'model_one/helper.r', 'model_two.r')
    environment(syberia_objects)$list.files <- function(...) models
    expect_true(setequal(syberia_models('dev', by_mtime = FALSE),
                file.path('dev', models[c(1,3)])))
  })

  test_that('it can use modified time to sort models', {
    models <- c("model1.r", "model2.r")
    environment(syberia_objects)$list.files <- function(...) models
    environment(syberia_objects)$file.info <- 
      function(f) list(mtime = as.integer(gsub('[^0-9]', '', f)))
    expect_identical(syberia_models('dev', by_mtime = TRUE),
                     rev(file.path('dev', models)))
  })

  # TODO(RK): Write a test for smart interpolation on v.s. off

})

