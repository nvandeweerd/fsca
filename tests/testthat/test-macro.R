context("Macro Checks")

load("test.sents.rdata")
df <- synt.units.fcn(test.sents[["b.208.1"]])

test_that("Correct unit counts", {
  expect_equal(df[["SENTENCES"]][["NUMBER"]], 1)
  expect_equal(df[["CLAUSES"]][["NUMBER"]], 2)
  expect_equal(df[["DEP_CLAUSES"]][["NUMBER"]], 1)
  expect_equal(df[["CO_CLAUSES"]][["NUMBER"]], 0)
  expect_equal(df[["T_UNITS"]][["NUMBER"]], 1)
  expect_equal(df[["NOUN_PHRASES"]][["NUMBER"]], 1)
  expect_equal(df[["VERB_PHRASES"]][["NUMBER"]], 3)
})

test_that("Correct unit lengths", {
  expect_equal(df[["SENTENCES"]][["LENGTHS"]], 12)
  expect_equal(df[["CLAUSES"]][["LENGTHS"]], c(12, 10))
  expect_equal(df[["DEP_CLAUSES"]][["LENGTHS"]], 10)
  expect_equal(df[["CO_CLAUSES"]][["LENGTHS"]], NULL)
  expect_equal(df[["T_UNITS"]][["LENGTHS"]], 12)
  expect_equal(df[["NOUN_PHRASES"]][["LENGTHS"]], 4)
  expect_equal(df[["VERB_PHRASES"]][["LENGTHS"]], c(11, 8, 5))
})


