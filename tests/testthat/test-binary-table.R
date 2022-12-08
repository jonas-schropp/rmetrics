################################################################################
#####
# TESTS FOR BINARY METRICS
#####
################################################################################



################################################################################
# Point estimates - Standard

prediction <- c(rep(0, 50), rep(1, 50))
reference <- c(rep(0, 35), rep(1, 40), rep(0, 25))

tbl <- table(prediction, reference)

ci.type <- FALSE
ci.level <- 0


# Confusion Entropy: 0.944
# Modified Confusion Entropy: 1.018
# G Measure: 0.559

test_that("calc_prevalence works with valid input", {
  expect_equal(
    calc_prevalence(tbl, ci.type, ci.level)[1],
    0.4, # pycm
    tolerance = 0.001
  )
})
test_that("calc_acc works with valid input", {
  expect_equal(
    calc_acc(tbl, ci.type, ci.level)[1],
    0.6, # pycm
    tolerance = 0.001
  )
})
test_that("calc_tpr works with valid input", {
  expect_equal(
    calc_tpr(tbl, ci.type, ci.level)[1],
    0.625, # pycm
    tolerance = 0.001
  )
})
test_that("calc_tnr works with valid input", {
  expect_equal(
    calc_tnr(tbl, ci.type, ci.level)[1],
    0.583, # pycm
    tolerance = 0.001
  )
})
test_that("calc_precision works with valid input", {
  expect_equal(
    calc_precision(tbl, ci.type, ci.level)[1],
    0.5, # pycm
    tolerance = 0.001
  )
})
test_that("calc_npv works with valid input", {
  expect_equal(
    calc_npv(tbl, ci.type, ci.level)[1], # negative predictive value,
    0.7, # pycm
    tolerance = 0.001
  )
})
test_that("calc_fnr works with valid input", {
  expect_equal(
    calc_fnr(tbl, ci.type, ci.level)[1], # false negative rate,
    0.375, # pycm
    tolerance = 0.001
  )
})
test_that("calc_fpr works with valid input", {
  expect_equal(
    calc_fpr(tbl, ci.type, ci.level)[1], # false positive rate,
    0.4167, # pycm
    tolerance = 0.001
  )
})
test_that("calc_fdr works with valid input", {
  expect_equal(
    calc_fdr(tbl, ci.type, ci.level)[1], # false discovery rate,
    0.5, # pycm
    tolerance = 0.001
  )
})
test_that("calc_for works with valid input", {
  expect_equal(
    calc_for(tbl, ci.type, ci.level)[1], # false omission rate,
    0.300, # pycm
    tolerance = 0.001
  )
})
test_that("calc_plr works with valid input", {
  expect_equal(
    unname(calc_plr(tbl, ci.type, ci.level)[1]), # positive likelihood ratio,
    1.500, # pycm
    tolerance = 0.001
  )
})
test_that("calc_nlr works with valid input", {
  expect_equal(
    unname(calc_nlr(tbl, ci.type, ci.level)[1]), # negative likelihood ratio,
    0.6428, # pycm
    tolerance = 0.001
  )
})
test_that("calc_err works with valid input", {
  expect_equal(
    calc_err(tbl, ci.type, ci.level)[1], # error rate,
    0.4, # pycm
    tolerance = 0.001
  )
})
test_that("calc_f05 works with valid input", {
  expect_equal(
    calc_f(tbl, 0.5),
    0.521, # pycm
    tolerance = 0.001
  )
})
test_that("calc_f1 works with valid input", {
  expect_equal(
    calc_f(tbl, 1),
    0.556, # pycm
    tolerance = 0.001
  )
})
test_that("calc_f2 works with valid input", {
  expect_equal(
    calc_f(tbl, 2),
    0.595, # pycm
    tolerance = 0.001
  )
})
test_that("calc_phi works with valid input", {
  expect_equal(
    calc_phi(tbl),
    0.204, # psych
    tolerance = 0.001
  )
})
test_that("calc_informedness works with valid input", {
  expect_equal(
    calc_informedness(tbl),
    0.2083, # pycm
    tolerance = 0.001
  )
})
test_that("calc_markedness works with valid input", {
  expect_equal(
    calc_markedness(tbl),
    0.1999, # pycm
    tolerance = 0.001
  )
})
test_that("calc_dor works with valid input", {
  expect_equal(
    unname(calc_dor(tbl)),
    2.333, # pycm
    tolerance = 0.001
  )
})
test_that("calc_racc works with valid input", {
  expect_equal(
    calc_racc(tbl, unbiased = FALSE),
    0.2, # pycm
    tolerance = 0.001
  )
})
test_that("calc_raccu works with valid input", {
  expect_equal(
    calc_racc(tbl, unbiased = TRUE),
    0.2025, # pycm
    tolerance = 0.001
  )
})
test_that("calc_ooc works with valid input", {
  expect_equal(
    calc_ooc(tbl), # otsuki-ochiai coefficient,
    0.559, # pycm
    tolerance = 0.001
  )
})
test_that("calc_oc works with valid input", {
  expect_equal(
    calc_oc(tbl), # overlap coefficient,
    0.625, # pycm
    tolerance = 0.001
  )
})
test_that("calc_bbs works with valid input", {
  expect_equal(
    calc_bbs(tbl), # braun-blanquet similarity,
    0.5, # pycm
    tolerance = 0.001
  )
})
test_that("calc_af works with valid input", {
  expect_equal(
    calc_af(tbl), # adjusted f-score,
    0.6329, # pycm
    tolerance = 0.001
  )
})
test_that("calc_agm works with valid input", {
  expect_equal(
    calc_agm(tbl), # adjusted geometric mean,
    0.596, # pycm
    tolerance = 0.001
  )
})
test_that("calc_q works with valid input", {
  expect_equal(
    calc_q(tbl), # yule's q,
    0.4, # pycm
    tolerance = 0.001
  )
})
test_that("calc_mcc works with valid input", {
  expect_equal(
    calc_mcc(tbl), # matthew's correlation,
    0.204, # pycm
    tolerance = 0.001
  )
})
test_that("calc_ji works with valid input", {
  expect_equal(
    calc_jaccard(tbl), # jaccard index,
    0.3846, # pycm
    tolerance = 0.001
  )
})
test_that("calc_is works with valid input", {
  expect_equal(
    calc_is(tbl), # information score,
    0.3219, # pycm
    tolerance = 0.001
  )
})
test_that("calc_auroc works with valid input", {
  expect_equal(
    calc_auroc(tbl),
    0.604, # pycm
    tolerance = 0.001
  )
})
test_that("calc_aupr works with valid input", {
  expect_equal(
    calc_aupr(tbl),
    0.5625, # pycm
    tolerance = 0.001
  )
})
test_that("calc_dind works with valid input", {
  expect_equal(
    calc_dind(tbl), # distance index,
    0.5605, # pycm
    tolerance = 0.001
  )
})
test_that("calc_sind works with valid input", {
  expect_equal(
    calc_sind(tbl), # similarity index,
    0.6036, # pycm
    tolerance = 0.001
  )
})
test_that("calc_dp works with valid input", {
  expect_equal(
    calc_dp(tbl), # discriminant power,
    0.2028, # pycm
    tolerance = 0.001
  )
})
test_that("calc_gini works with valid input", {
  expect_equal(
    calc_gini(tbl), # gini index
    0.2083, # pycm
    tolerance = 0.001
  )
})
test_that("calc_lift works with valid input", {
  expect_equal(
    calc_lift(tbl), # lift score
    1.25, # pycm
    tolerance = 0.001
  )
})
test_that("calc_op works with valid input", {
  expect_equal(
    calc_op(tbl), # optimized precision
    0.5655, # pycm
    tolerance = 0.001
  )
})
test_that("calc_iba works with valid input", {
  expect_equal(
    calc_iba(tbl), # index of balanced accuracy
    0.3797, # pycm
    tolerance = 0.001
  )
})
test_that("calc_bcd works with valid input", {
  expect_equal(
    calc_bcd(tbl), # bray-curtis dissimilarity
    0.05, # pycm
    tolerance = 0.001
  )
})
test_that("calc_gmean works with valid input", {
  expect_equal(
    calc_gmean(tbl), # geometric mean
    0.6038, # pycm
    tolerance = 0.001
  )
})
test_that("calc_icsi works with valid input", {
  expect_equal(
    calc_icsi(tbl), # icsi
    0.125, # pycm
    tolerance = 0.001
  )
})


