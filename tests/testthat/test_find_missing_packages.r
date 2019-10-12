

test_that("not fooled when r code missing", {
  expect_error(find_missing_packages("no_scripts"))
})

test_that("r code present, but no package calls", {
  out <- find_missing_packages("no_packages")
  expect_true(length(out$required) == 0)
})

test_that("library works", {
  out <- find_missing_packages("library")
  expect_true(length(out$required) == 2)
  expect_true(length(out$missing) == 1)
})

test_that("comment works", {
  out <- find_missing_packages("comment")
  expect_true(length(out$required) == 2)
  expect_true(length(out$missing) == 1)
})

test_that("hard comment works", {
  out <- find_missing_packages("hard_comment")
  expect_true(length(out$required) == 2)
  expect_true(length(out$missing) == 1)
})

test_that("require and library both work", {
  out <- find_missing_packages("require_library")
  expect_true(length(out$required) == 2)
  expect_true(length(out$missing) == 1)
})

test_that("multiple files work", {
  out <- find_missing_packages("multiple_files")
  expect_true(length(out$required) == 2)
  expect_true(length(out$missing) == 1)
})

test_that("multiple nested files work", {
  out <- find_missing_packages("nested_files")
  expect_true(length(out$required) == 2)
  expect_true(length(out$missing) == 1)
})
