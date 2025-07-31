test_that("ConsumptionSeries is not making errors.", {
  expect_no_error(ConsumptionSeries(Data=CaNsample_long,
                                  param=tested_components,
                                  info=Info_data,
                                  group=FALSE,
                                  grouplabel=NULL,
                                  session=NULL))
})

test_that("ConsumptionSeries is making a plot.", {
  expect_true(inherits(ConsumptionSeries(Data=CaNsample_long,
                                       param=tested_components,
                                       info=Info_data,
                                       group=FALSE,
                                       grouplabel=NULL,
                                       session=NULL),
                       "ggplot")
  )
})

test_that("ConsumptionSeries is not making errors when grouping.", {
  expect_no_error(ConsumptionSeries(Data=CaNsample_long,
                                  param=tested_components,
                                  info=Info_data,
                                  group=TRUE,
                                  grouplabel="Group",
                                  session=NULL))
})