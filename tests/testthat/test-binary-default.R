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

n <- sum(tbl)

tp <- tbl[2,2]
tn <- tbl[1,1]
fp <- tbl[2,1]
fn <- tbl[1,2]

pos <- sum(tbl[,2]) # positives in reference
neg <- sum(tbl[,1]) # negatives in reference
ppos <- tp + fp     # positives in prediction
pneg <- tn + fn     # negatives in prediction

ci.type <- FALSE
ci.level <- 0


# Confusion Entropy: 0.944
# Modified Confusion Entropy: 1.018
# G Measure: 0.559

test_that("calc_prevalence works with valid input", {
  expect_equal(
    calc_prevalence(pos, neg, ci.type, ci.level)[1],
    0.4, # pycm
    tolerance = 0.001
  )
})
test_that("calc_acc works with valid input", {
  expect_equal(
    calc_acc(tp, tn, fp, fn, ci.type, ci.level)[1],
    0.6, # pycm
    tolerance = 0.001
  )
})
test_that("calc_tpr works with valid input", {
  expect_equal(
    calc_tpr(tp, fn, ci.type, ci.level)[1],
    0.625, # pycm
    tolerance = 0.001
  )
})
test_that("calc_tnr works with valid input", {
  expect_equal(
    calc_tnr(tn, fp, ci.type, ci.level)[1],
    0.583, # pycm
    tolerance = 0.001
  )
})
test_that("calc_precision works with valid input", {
  expect_equal(
    calc_precision(tp, fp, ci.type, ci.level)[1],
    0.5, # pycm
    tolerance = 0.001
  )
})
test_that("calc_npv works with valid input", {
  expect_equal(
    calc_npv(tn, fn, ci.type, ci.level)[1], # negative predictive value,
    0.7, # pycm
    tolerance = 0.001
  )
})
test_that("calc_fnr works with valid input", {
  expect_equal(
    calc_fnr(fn, tp, ci.type, ci.level)[1], # false negative rate,
    0.375, # pycm
    tolerance = 0.001
  )
})
test_that("calc_fpr works with valid input", {
  expect_equal(
    calc_fpr(fp, tn, ci.type, ci.level)[1], # false positive rate,
    0.4167, # pycm
    tolerance = 0.001
  )
})
test_that("calc_fdr works with valid input", {
  expect_equal(
    calc_fdr(fp, tp, ci.type, ci.level)[1], # false discovery rate,
    0.5, # pycm
    tolerance = 0.001
  )
})
test_that("calc_for works with valid input", {
  expect_equal(
    calc_for(fn, tn, ci.type, ci.level)[1], # false omission rate,
    0.300, # pycm
    tolerance = 0.001
  )
})
test_that("calc_plr works with valid input", {
  expect_equal(
    unname(calc_plr(tp, fn, fp, tn, ci.type, ci.level)[1]), # positive likelihood ratio,
    1.500, # pycm
    tolerance = 0.001
  )
})
test_that("calc_nlr works with valid input", {
  expect_equal(
    unname(calc_nlr(tp, fn, fp, tn, ci.type, ci.level)[1]), # negative likelihood ratio,
    0.6428, # pycm
    tolerance = 0.001
  )
})
test_that("calc_err works with valid input", {
  expect_equal(
    calc_err(tp, tn, fp, fn, ci.type, ci.level)[1], # error rate,
    0.4, # pycm
    tolerance = 0.001
  )
})
test_that("calc_f05 works with valid input", {
  expect_equal(
    calc_f05(tp, fp, fn),
    0.521, # pycm
    tolerance = 0.001
  )
})
test_that("calc_f1 works with valid input", {
  expect_equal(
    calc_f1(tp, fp, fn),
    0.556, # pycm
    tolerance = 0.001
  )
})
test_that("calc_f2 works with valid input", {
  expect_equal(
    calc_f2(tp, fp, fn),
    0.595, # pycm
    tolerance = 0.001
  )
})
test_that("calc_phi works with valid input", {
  expect_equal(
    calc_phi(tp, tn, fp, fn),
    0.204, # psych
    tolerance = 0.001
  )
})
test_that("calc_informedness works with valid input", {
  expect_equal(
    calc_informedness(tp, fn, tn, fp),
    0.2083, # pycm
    tolerance = 0.001
  )
})
test_that("calc_markedness works with valid input", {
  expect_equal(
    calc_markedness(tp, fn, tn, fp),
    0.1999, # pycm
    tolerance = 0.001
  )
})
test_that("calc_dor works with valid input", {
  expect_equal(
    unname(calc_dor(tp, fn, tn, fp)),
    2.333, # pycm
    tolerance = 0.001
  )
})
test_that("calc_racc works with valid input", {
  expect_equal(
    calc_racc(tp, fp, fn, n, unbiased = FALSE),
    0.2, # pycm
    tolerance = 0.001
  )
})
test_that("calc_raccu works with valid input", {
  expect_equal(
    calc_racc(tp, fp, fn, n, unbiased = TRUE),
    0.2025, # pycm
    tolerance = 0.001
  )
})
test_that("calc_ooc works with valid input", {
  expect_equal(
    calc_ooc(tp, fp, fn), # otsuki-ochiai coefficient,
    0.559, # pycm
    tolerance = 0.001
  )
})
test_that("calc_oc works with valid input", {
  expect_equal(
    calc_oc(tp, fp, fn), # overlap coefficient,
    0.625, # pycm
    tolerance = 0.001
  )
})
test_that("calc_bbs works with valid input", {
  expect_equal(
    calc_bbs(tp, ppos, pos), # braun-blanquet similarity,
    0.5, # pycm
    tolerance = 0.001
  )
})
test_that("calc_af works with valid input", {
  expect_equal(
    calc_af(tp, fp, fn, tn), # adjusted f-score,
    0.6329, # pycm
    tolerance = 0.001
  )
})
test_that("calc_agm works with valid input", {
  expect_equal(
    calc_agm(tn, fp, tp, fn), # adjusted geometric mean,
    0.596, # pycm
    tolerance = 0.001
  )
})
test_that("calc_q works with valid input", {
  expect_equal(
    calc_q(tp, tn, fp, fn), # yule's q,
    0.4, # pycm
    tolerance = 0.001
  )
})
test_that("calc_mcc works with valid input", {
  expect_equal(
    calc_mcc(tp, tn, fp, fn), # matthew's correlation,
    0.204, # pycm
    tolerance = 0.001
  )
})
test_that("calc_ji works with valid input", {
  expect_equal(
    calc_jaccard(tp, fn, fp), # jaccard index,
    0.3846, # pycm
    tolerance = 0.001
  )
})
test_that("calc_is works with valid input", {
  expect_equal(
    calc_is(tp, fp, fn, n), # information score,
    0.3219, # pycm
    tolerance = 0.001
  )
})
test_that("calc_auroc works with valid input", {
  expect_equal(
    calc_auroc(tn, fp, tp, fn),
    0.604, # pycm
    tolerance = 0.001
  )
})
test_that("calc_aupr works with valid input", {
  expect_equal(
    calc_aupr(tp, fp, fn),
    0.5625, # pycm
    tolerance = 0.001
  )
})
test_that("calc_dind works with valid input", {
  expect_equal(
    calc_dind(tn, fp, tp, fn), # distance index,
    0.5605, # pycm
    tolerance = 0.001
  )
})
test_that("calc_sind works with valid input", {
  expect_equal(
    calc_sind(tn, fp, tp, fn), # similarity index,
    0.6036, # pycm
    tolerance = 0.001
  )
})
test_that("calc_dp works with valid input", {
  expect_equal(
    calc_dp(tn, fp, tp, fn), # discriminant power,
    0.2028, # pycm
    tolerance = 0.001
  )
})
test_that("calc_gini works with valid input", {
  expect_equal(
    calc_gini(tn, fp, tp, fn), # gini index
    0.2083, # pycm
    tolerance = 0.001
  )
})
test_that("calc_lift works with valid input", {
  expect_equal(
    calc_lift(tp, fp, pos, neg), # lift score
    1.25, # pycm
    tolerance = 0.001
  )
})
test_that("calc_op works with valid input", {
  expect_equal(
    calc_op(tn, fp, tp, fn), # optimized precision
    0.5655, # pycm
    tolerance = 0.001
  )
})
test_that("calc_iba works with valid input", {
  expect_equal(
    calc_iba(tn, fp, tp, fn), # index of balanced accuracy
    0.3797, # pycm
    tolerance = 0.001
  )
})
test_that("calc_bcd works with valid input", {
  expect_equal(
    calc_bcd(ppos, pos, n), # bray-curtis dissimilarity
    0.05, # pycm
    tolerance = 0.001
  )
})
test_that("calc_gmean works with valid input", {
  expect_equal(
    calc_gmean(tn, fp, tp, fn), # geometric mean
    0.6038, # pycm
    tolerance = 0.001
  )
})
test_that("calc_icsi works with valid input", {
  expect_equal(
    calc_icsi(tp, fn, fp), # icsi
    0.125, # pycm
    tolerance = 0.001
  )
})


