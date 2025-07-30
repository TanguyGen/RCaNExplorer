test_that("RatioProductionBiomass is not making errors.", {
  expect_no_error(RatioProductionBiomass(Data=CaNsample_long,
                                          param=tested_components,
                                          info=Info_data,
                                          group=FALSE,
                                          grouplabel=NULL,
                                          session=NULL))
})

test_that("RatioProductionBiomass is making a plot.", {
  expect_true(inherits(RatioProductionBiomass(Data=CaNsample_long,
                                               param=tested_components,
                                               info=Info_data,
                                               group=FALSE,
                                               grouplabel=NULL,
                                               session=NULL),
                       "ggplot")
  )
})

test_that("RatioProductionBiomass is not making errors when grouping.", {
  expect_no_error(RatioProductionBiomass(Data=CaNsample_long,
                                          param=tested_components,
                                          info=Info_data,
                                          group=TRUE,
                                          grouplabel="Group",
                                          session=NULL))
})