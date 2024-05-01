test_that("flow() produces a Flow object", {
  fl <- flow()
  expect_s3_class(fl, 'Flow')
  rm(fl)
})
