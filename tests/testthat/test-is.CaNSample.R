test_that("if it recognises a CaNSample file.", {
  expect_true(is.CaNSample(CaNsample))
})

test.files <- list(system.file("tests/extdata",
                               "NotRCaN-1.RData", package = "RCaNExplorer"),
                   system.file("tests/extdata",
                               "NotRCaN-2.RData", package = "RCaNExplorer"),
                   system.file("tests/extdata",
                               "NotRCaN-3.RData", package = "RCaNExplorer"),
                   system.file("tests/extdata",
                               "NotRCaN-4.RData", package = "RCaNExplorer")
)

test1<-load_CaNSample(test.files[[1]])
test2<-load_CaNSample(test.files[[2]])
test3<-load_CaNSample(test.files[[3]])
test4<-load_CaNSample(test.files[[4]])

test_that("if does not recognise a completely different file.", {
  expect_false(is.CaNSample(test1))
})

test_that("if does not recognise an object with only CaNmod.", {
  expect_false(is.CaNSample(test2))
})

test_that("if does not recognise an object with faulty mcmc.", {
  expect_false(is.CaNSample(test3))
})

test_that("if does not recognise an object with faulty CaNmod.", {
  expect_false(is.CaNSample(test4))
})