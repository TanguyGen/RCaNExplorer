test_that("basic structure of output is correct.", {
  CaNsample_long <- transform_CaNSample(CaNsample)
  expect_s3_class(CaNsample_long, "data.frame")
  expect_named(CaNsample_long, c("Sample_id", "Var", "Year", "value"))
})

test_that("dimensions of output are correct.", {
  CaNsample_long <- transform_CaNSample(CaNsample)
  expect_equal(nrow(CaNsample_long),
               ncol(CaNsample$mcmc[[1]])*nrow(CaNsample$mcmc[[1]])
               )
})
