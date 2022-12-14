################################################################################
#####
# TESTS FOR BINARY METRICS
#####
################################################################################



################################################################################
# Point estimates - Standard

prediction <- c(rep(0, 50), rep(1, 50))
reference <- c(rep(0, 35), rep(1, 40), rep(0, 25))

data <- data.frame(prediction, reference)

ci.type <- FALSE
ci.level <- 0


# Confusion Entropy: 0.944
# Modified Confusion Entropy: 1.018
# G Measure: 0.559

test_that("calc_prevalence works with valid input", {
  expect_equal(
    calc_prevalence(data, "prediction", "reference", ci.type, ci.level)[1],
    0.4, # pycm
    tolerance = 0.001
  )
})
test_that("calc_acc works with valid input", {
  expect_equal(
    calc_acc(data, "prediction", "reference", ci.type, ci.level)[1],
    0.6, # pycm
    tolerance = 0.001
  )
})
test_that("calc_tpr works with valid input", {
  expect_equal(
    calc_tpr(data, "prediction", "reference", ci.type, ci.level)[1],
    0.625, # pycm
    tolerance = 0.001
  )
})
test_that("calc_tnr works with valid input", {
  expect_equal(
    calc_tnr(data, "prediction", "reference", ci.type, ci.level)[1],
    0.583, # pycm
    tolerance = 0.001
  )
})
test_that("calc_precision works with valid input", {
  expect_equal(
    calc_precision(data, "prediction", "reference", ci.type, ci.level)[1],
    0.5, # pycm
    tolerance = 0.001
  )
})
test_that("calc_npv works with valid input", {
  expect_equal(
    calc_npv(data, "prediction", "reference", ci.type, ci.level)[1], # negative predictive value,
    0.7, # pycm
    tolerance = 0.001
  )
})
test_that("calc_fnr works with valid input", {
  expect_equal(
    calc_fnr(data, "prediction", "reference", ci.type, ci.level)[1], # false negative rate,
    0.375, # pycm
    tolerance = 0.001
  )
})
test_that("calc_fpr works with valid input", {
  expect_equal(
    calc_fpr(data, "prediction", "reference", ci.type, ci.level)[1], # false positive rate,
    0.4167, # pycm
    tolerance = 0.001
  )
})
test_that("calc_fdr works with valid input", {
  expect_equal(
    calc_fdr(data, "prediction", "reference", ci.type, ci.level)[1], # false discovery rate,
    0.5, # pycm
    tolerance = 0.001
  )
})
test_that("calc_for works with valid input", {
  expect_equal(
    calc_for(data, "prediction", "reference", ci.type, ci.level)[1], # false omission rate,
    0.300, # pycm
    tolerance = 0.001
  )
})
test_that("calc_plr works with valid input", {
  expect_equal(
    unname(calc_plr(data, "prediction", "reference", ci.type, ci.level)[1]), # positive likelihood ratio,
    1.500, # pycm
    tolerance = 0.001
  )
})
test_that("calc_nlr works with valid input", {
  expect_equal(
    unname(calc_nlr(data, "prediction", "reference", ci.type, ci.level)[1]), # negative likelihood ratio,
    0.6428, # pycm
    tolerance = 0.001
  )
})
test_that("calc_err works with valid input", {
  expect_equal(
    calc_err(data, "prediction", "reference", ci.type, ci.level)[1], # error rate,
    0.4, # pycm
    tolerance = 0.001
  )
})
test_that("calc_f05 works with valid input", {
  expect_equal(
    calc_f(data, "prediction", "reference", 0.5),
    0.521, # pycm
    tolerance = 0.001
  )
})
test_that("calc_f1 works with valid input", {
  expect_equal(
    calc_f(data, "prediction", "reference", 1),
    0.556, # pycm
    tolerance = 0.001
  )
})
test_that("calc_f2 works with valid input", {
  expect_equal(
    calc_f(data, "prediction", "reference", 2),
    0.595, # pycm
    tolerance = 0.001
  )
})
test_that("calc_phi works with valid input", {
  expect_equal(
    calc_phi(data, "prediction", "reference"),
    0.204, # psych
    tolerance = 0.001
  )
})
test_that("calc_informedness works with valid input", {
  expect_equal(
    calc_informedness(data, "prediction", "reference"),
    0.2083, # pycm
    tolerance = 0.001
  )
})
test_that("calc_markedness works with valid input", {
  expect_equal(
    calc_markedness(data, "prediction", "reference"),
    0.1999, # pycm
    tolerance = 0.001
  )
})
test_that("calc_dor works with valid input", {
  expect_equal(
    unname(calc_dor(data, "prediction", "reference")),
    2.333, # pycm
    tolerance = 0.001
  )
})
test_that("calc_racc works with valid input", {
  expect_equal(
    calc_racc(data, "prediction", "reference", unbiased = FALSE),
    0.2, # pycm
    tolerance = 0.001
  )
})
test_that("calc_raccu works with valid input", {
  expect_equal(
    calc_racc(data, "prediction", "reference", unbiased = TRUE),
    0.2025, # pycm
    tolerance = 0.001
  )
})
test_that("calc_ooc works with valid input", {
  expect_equal(
    calc_ooc(data, "prediction", "reference"), # otsuki-ochiai coefficient,
    0.559, # pycm
    tolerance = 0.001
  )
})
test_that("calc_oc works with valid input", {
  expect_equal(
    calc_oc(data, "prediction", "reference"), # overlap coefficient,
    0.625, # pycm
    tolerance = 0.001
  )
})
test_that("calc_bbs works with valid input", {
  expect_equal(
    calc_bbs(data, "prediction", "reference"), # braun-blanquet similarity,
    0.5, # pycm
    tolerance = 0.001
  )
})
test_that("calc_af works with valid input", {
  expect_equal(
    calc_af(data, "prediction", "reference"), # adjusted f-score,
    0.6329, # pycm
    tolerance = 0.001
  )
})
test_that("calc_agm works with valid input", {
  expect_equal(
    calc_gmean(data, "prediction", "reference", TRUE), # adjusted geometric mean,
    0.596, # pycm
    tolerance = 0.001
  )
})
test_that("calc_q works with valid input", {
  expect_equal(
    calc_q(data, "prediction", "reference"), # yule's q,
    0.4, # pycm
    tolerance = 0.001
  )
})
test_that("calc_mcc works with valid input", {
  expect_equal(
    calc_mcc(data, "prediction", "reference"), # matthew's correlation,
    0.204, # pycm
    tolerance = 0.001
  )
})
test_that("calc_ji works with valid input", {
  expect_equal(
    calc_jaccard(data, "prediction", "reference"), # jaccard index,
    0.3846, # pycm
    tolerance = 0.001
  )
})
test_that("calc_is works with valid input", {
  expect_equal(
    calc_is(data, "prediction", "reference"), # information score,
    0.3219, # pycm
    tolerance = 0.001
  )
})
test_that("calc_auroc works with valid input", {
  expect_equal(
    calc_auroc(data, "prediction", "reference"),
    0.604, # pycm
    tolerance = 0.001
  )
})
test_that("calc_aupr works with valid input", {
  expect_equal(
    calc_aupr(data, "prediction", "reference"),
    0.5625, # pycm
    tolerance = 0.001
  )
})
test_that("calc_dind works with valid input", {
  expect_equal(
    calc_dind(data, "prediction", "reference"), # distance index,
    0.5605, # pycm
    tolerance = 0.001
  )
})
test_that("calc_sind works with valid input", {
  expect_equal(
    calc_sind(data, "prediction", "reference"), # similarity index,
    0.6036, # pycm
    tolerance = 0.001
  )
})
test_that("calc_dp works with valid input", {
  expect_equal(
    calc_dp(data, "prediction", "reference"), # discriminant power,
    0.2028, # pycm
    tolerance = 0.001
  )
})
test_that("calc_gini works with valid input", {
  expect_equal(
    calc_gini(data, "prediction", "reference"), # gini index
    0.2083, # pycm
    tolerance = 0.001
  )
})
test_that("calc_lift works with valid input", {
  expect_equal(
    calc_lift(data, "prediction", "reference"), # lift score
    1.25, # pycm
    tolerance = 0.001
  )
})
test_that("calc_op works with valid input", {
  expect_equal(
    calc_op(data, "prediction", "reference"), # optimized precision
    0.5655, # pycm
    tolerance = 0.001
  )
})
test_that("calc_iba works with valid input", {
  expect_equal(
    calc_iba(data, "prediction", "reference"), # index of balanced accuracy
    0.3797, # pycm
    tolerance = 0.001
  )
})
test_that("calc_bcd works with valid input", {
  expect_equal(
    calc_bcd(data, "prediction", "reference"), # bray-curtis dissimilarity
    0.05, # pycm
    tolerance = 0.001
  )
})
test_that("calc_gmean works with valid input", {
  expect_equal(
    calc_gmean(data, "prediction", "reference"), # geometric mean
    0.6038, # pycm
    tolerance = 0.001
  )
})
test_that("calc_icsi works with valid input", {
  expect_equal(
    calc_icsi(data, "prediction", "reference"), # icsi
    0.125, # pycm
    tolerance = 0.001
  )
})
