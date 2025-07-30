test_that("BiomassSeries is not making errors.", {
  expect_no_error(BiomassSeries(Data=CaNsample_long,
                                            param=tested_components,
                                            info=Info_data,
                                            group=FALSE,
                                            grouplabel=NULL,
                                            session=NULL))
})

test_that("BiomassSeries is making a plot.", {
  expect_true(inherits(BiomassSeries(Data=CaNsample_long,
                                     param=tested_components,
                                     info=Info_data,
                                     group=FALSE,
                                     grouplabel=NULL,
                                     session=NULL),
                       "ggplot")
  )
})

test_that("BiomassSeries is not making errors when grouping.", {
  expect_no_error(BiomassSeries(Data=CaNsample_long,
                                param=tested_components,
                                info=Info_data,
                                group=TRUE,
                                grouplabel="Group",
                                session=NULL))
})