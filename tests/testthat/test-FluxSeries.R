tested_fluxes<-c("PhytoAndBacteria_HerbZooplankton", "OmniZooplankton_Fishery")

test_that("FluxSeries is not making errors.", {
  expect_no_error(FluxSeries(Data=CaNsample_long,
                                param=tested_fluxes,
                                info=Info_data,
                                group=FALSE,
                                grouplabel=NULL,
                                session=NULL))
})

test_that("FluxSeries is making a plot.", {
  expect_true(inherits(FluxSeries(Data=CaNsample_long,
                                     param=tested_fluxes,
                                     info=Info_data,
                                     group=FALSE,
                                     grouplabel=NULL,
                                     session=NULL),
                       "ggplot")
  )
})

test_that("FluxSeries is not making errors when grouping.", {
  expect_no_error(FluxSeries(Data=CaNsample_long,
                                param=tested_fluxes,
                                info=Info_data,
                                group=TRUE,
                                grouplabel="Group",
                                session=NULL))
})