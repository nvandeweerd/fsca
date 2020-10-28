context("Micro Checks")

load("test.sents.rdata")

df <- synt.units.fcn(test.sents[["b.47.1"]])

test_that("Coordinated clauses", {
  expect_equal(df[["CO_CLAUSES"]][["NUMBER"]], 2)
  expect_equal(df[["CO_CLAUSES"]][["LENGTHS"]], c(21, 9))
})

df <- synt.units.fcn(test.sents[["b.4.1"]])

test_that("Dependent clauses", {
  expect_equal(df[["DEP_CLAUSES"]][["NUMBER"]], 2)
  expect_equal(df[["DEP_CLAUSES"]][["LENGTHS"]], c(12, 6))
})

df <- synt.units.fcn(test.sents[["b.23.1"]])

test_that("Noun phrases", {
  expect_equal(df[["NOUN_PHRASES"]][["NUMBER"]], 9)
  expect_equal(df[["NOUN_PHRASES"]][["LENGTHS"]], c(4, 2, 3, 10, 6, 4, 2, 3, 2))
})
