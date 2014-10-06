require(testthatsomemore)
context('syberia_models')

test_that('it can discriminate between directoried and non-directoried models', {
  within_file_structure(list('syberia.config', models = list(dev = list(
      model_one = list(model_one.r = 'foo', helper.r = 'bar'),
      model_two.r = 'blah'))), {
    models <- c('model_one/model_one.r', 'model_one/helper.r', 'model_two.r')
    expect_true(setequal(syberia_models('dev', root = tempdir, by_mtime = FALSE),
                file.path('dev', models[c(1,3)])))
  })
})

test_that('it can use modified time to sort models', {
  within_file_structure(list('syberia.config',
                             models = list(dev = list('model1.r', 'model2.r'))), {
    Sys.sleep(1) # Touch the second model to make it modified later.
    writeLines('', file.path(tempdir, 'models', 'dev', 'model2.r'))
    models <- c('model1.r', 'model2.r')
    expect_identical(syberia_models('dev', root = tempdir, by_mtime = TRUE),
                     rev(file.path('dev', models)))
  })
})

test_that('it excludes non-R files', {
  within_file_structure(list('syberia.config', models = list(dev = list('model1.r', 'model2.md'))), {
    expect_identical(syberia_models('dev', root = tempdir), file.path('dev', 'model1.r'))
  })
})

# TODO(RK): Write a test for smart interpolation on v.s. off

