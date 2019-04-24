#Summary statistics tests
context("Hypothesis tests test")

example<-load.example("example1",F)
data <- example$data
tf <- example$tf
design <- svydesign(~0, data = data)
questionnaire <- example$questionnaire

## hypothesis_test_chisquared select one
test_that("hypothesis test chisquared sanitation works",{
  expect_null(hypothesis_test_chisquared_select_one(tf$select_one_many_cat[2], tf$select_one[1],design = design)$results) ## too many categories for ChiSquared
  expect_equal(hypothesis_test_chisquared_select_one(tf$select_one[2], tf$select_one_many_cat[1],design = design)$name, "too many (>=20) unique values in independent variable")
  expect_equal(hypothesis_test_chisquared_select_one(tf$select_one[2], tf$select_one[1],design = design)$name, "Pearson's X^2: Rao & Scott adjustment")
  })


##hypothesis_test_chisquared_select_multiple

test_that("hypothesis test chisquared select multiple sanitation works",{
  sm.columns <- questionnaire$choices_for_select_multiple(tf$select_multiple[1], data = data)
  expect_is(hypothesis_test_chisquared_select_multiple(tf$select_multiple[1], dependent.var.sm.cols = sm.columns, independent.var =  tf$select_one[1], design = design), "list")
  expect_equal(hypothesis_test_chisquared_select_multiple(tf$select_multiple[1], dependent.var.sm.cols = sm.columns, independent.var =  tf$select_one_many_cat[1], design = design)$name, "too many (>=20) unique values in independent variable")
  expect_equal(hypothesis_test_chisquared_select_multiple(tf$select_multiple[1], dependent.var.sm.cols = sm.columns, independent.var =  tf$select_one_NA_heavy[1], design = design)$name %>% levels, "Pearson's X^2: Rao & Scott adjustment") ## name gives you back a vector with levels equal to name
  expect_warning(hypothesis_test_chisquared_select_multiple(tf$select_multiple[2], dependent.var.sm.cols = sm.columns, independent.var =  tf$select_one[1], design = design)) ## doesnt throw an error because it essentially doesnt need the name of SM variable (only column indices)
})


## hypothesis test limit (t test one sample)
test_that("Hypothesis test limit works",{
  expect_gt(hypothesis_test_t_one_sample(tf$numeric[2], limit = 3,design = design)$result$t, -150)
  expect_equal(hypothesis_test_t_one_sample(tf$select_one[1], limit = 3,design = design)$name, "less than 3 records have valid values in the dependent variable and in the independent variable") ## throws this error but the real problem is dependent not numeric (switch around always sanitise before?)
  expect_equal(hypothesis_test_t_one_sample(tf$select_one[2], limit = tf$select_one[1], design = design)$name, "less than 3 records have valid values in the dependent variable and in the independent variable")
})

## hypothesis test limit (t test one sample)
test_that("Hypothesis test two sample works",{
  expect_gt(hypothesis_test_t_two_sample(tf$numeric[2], tf$select_one[2],design = design)$result$t, 6.77) ## too many categories for ChiSquared
  expect_equal(hypothesis_test_t_two_sample(tf$numeric[2], independent = tf$select_one_many_cat[1],design = design)$name, "too many (>=20) unique values in independent variable")
  expect_equal(hypothesis_test_t_two_sample(tf$numeric[1], tf$select_one_NA_heavy[1],design = design)$name, "two sample ttest on difference in means (two sided)")
})

