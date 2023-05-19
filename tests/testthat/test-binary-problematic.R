################################################################################
#####
# TESTS FOR BINARY METRICS
#####
################################################################################



################################################################################
# Point estimates - Standard

tbl <- list()
tbl[[1]] <- as.table(matrix(c(8, 8, 8, 8), 2, 2))
tbl[[2]] <- as.table(matrix(c(0, 0, 8, 6), 2, 2))
tbl[[3]] <- as.table(matrix(c(0, 8, 0, 6), 2, 2))
tbl[[4]] <- as.table(matrix(c(6, 0, 8, 0), 2, 2))
tbl[[5]] <- as.table(matrix(c(0,5,9, 0), 2,2, byrow = T))

ci.type <- FALSE
ci.level <- 0



for (i in 1:5) {
  test_that("calc_prevalence runs with problematic input", {
    expect_no_error(
      calc_prevalence(tbl[[i]], ci.type, ci.level)
    )
  })
  test_that("calc_acc runs with problematic input", {
    expect_no_error(
      calc_acc(tbl[[i]], ci.type, ci.level)
    )
  })
  test_that("calc_tpr runs with problematic input", {
    expect_no_error(
      calc_tpr(tbl[[i]], ci.type, ci.level)
    )
  })
  test_that("calc_tnr runs with problematic input", {
    expect_no_error(
      calc_tnr(tbl[[i]], ci.type, ci.level)
    )
  })
  test_that("calc_precision runs with problematic input", {
    expect_no_error(
      calc_precision(tbl[[i]], ci.type, ci.level)
    )
  })
  test_that("calc_npv runs with problematic input", {
    expect_no_error(
      calc_npv(tbl[[i]], ci.type, ci.level)
    )
  })
  test_that("calc_fnr runs with problematic input", {
    expect_no_error(
      calc_fnr(tbl[[i]], ci.type, ci.level)
    )
  })
  test_that("calc_fpr runs with problematic input", {
    expect_no_error(
      calc_fpr(tbl[[i]], ci.type, ci.level)
    )
  })
  test_that("calc_fdr runs with problematic input", {
    expect_no_error(
      calc_fdr(tbl[[i]], ci.type, ci.level)
    )
  })
  test_that("calc_for runs with problematic input", {
    expect_no_error(
      calc_for(tbl[[i]], ci.type, ci.level)
    )
  })
  test_that("calc_plr runs with problematic input", {
    expect_no_error(
      calc_plr(tbl[[i]], ci.type, ci.level)
    )
  })
  test_that("calc_nlr runs with problematic input", {
    expect_no_error(
      calc_nlr(tbl[[i]], ci.type, ci.level)
    )
  })
  test_that("calc_err runs with problematic input", {
    expect_no_error(
      calc_err(tbl[[i]], ci.type, ci.level)
    )
  })
  test_that("calc_f05 runs with problematic input", {
    expect_no_error(
      calc_f(tbl[[i]], 0.5)
    )
  })
  test_that("calc_f1 runs with problematic input", {
    expect_no_error(
      calc_f(tbl[[i]], 1)
    )
  })
  test_that("calc_f2 runs with problematic input", {
    expect_no_error(
      calc_f(tbl[[i]], 2)
    )
  })
  test_that("calc_phi runs with problematic input", {
    expect_no_error(
      calc_phi(tbl[[i]])
    )
  })
  test_that("calc_informedness runs with problematic input", {
    expect_no_error(
      calc_informedness(tbl[[i]])
    )
  })
  test_that("calc_markedness runs with problematic input", {
    expect_no_error(
      calc_markedness(tbl[[i]])
    )
  })
  test_that("calc_dor runs with problematic input", {
    expect_no_error(
      calc_dor(tbl[[i]])
    )
  })
  test_that("calc_racc runs with problematic input", {
    expect_no_error(
      calc_racc(tbl[[i]], unbiased = FALSE)
    )
  })
  test_that("calc_raccu runs with problematic input", {
    expect_no_error(
      calc_racc(tbl[[i]], unbiased = TRUE)
    )
  })
  test_that("calc_ooc runs with problematic input", {
    expect_no_error(
      calc_ooc(tbl[[i]])
    )
  })
  test_that("calc_oc runs with problematic input", {
    expect_no_error(
      calc_oc(tbl[[i]])
    )
  })
  test_that("calc_bbs runs with problematic input", {
    expect_no_error(
      calc_bbs(tbl[[i]])
    )
  })
  test_that("calc_af runs with problematic input", {
    expect_no_error(
      calc_af(tbl[[i]])
    )
  })
  test_that("calc_agm runs with problematic input", {
    expect_no_error(
      calc_gmean(tbl[[i]], T)
    )
  })
  test_that("calc_q runs with problematic input", {
    expect_no_error(
      calc_q(tbl[[i]])
    )
  })
  test_that("calc_mcc runs with problematic input", {
    expect_no_error(
      calc_mcc(tbl[[i]])
    )
  })
  test_that("calc_ji runs with problematic input", {
    expect_no_error(
      calc_jaccard(tbl[[i]])
    )
  })
  test_that("calc_is runs with problematic input", {
    expect_no_error(
      calc_is(tbl[[i]])
    )
  })
  test_that("calc_auroc runs with problematic input", {
    expect_no_error(
      calc_auroc(tbl[[i]])
    )
  })
  test_that("calc_aupr runs with problematic input", {
    expect_no_error(
      calc_aupr(tbl[[i]])
    )
  })
  test_that("calc_dind runs with problematic input", {
    expect_no_error(
      calc_dind(tbl[[i]])
    )
  })
  test_that("calc_sind runs with problematic input", {
    expect_no_error(
      calc_sind(tbl[[i]])
    )
  })
  test_that("calc_dp runs with problematic input", {
    expect_no_error(
      calc_dp(tbl[[i]])
    )
  })
  test_that("calc_gini runs with problematic input", {
    expect_no_error(
      calc_gini(tbl[[i]])
    )
  })
  test_that("calc_lift runs with problematic input", {
    expect_no_error(
      calc_lift(tbl[[i]])
    )
  })
  test_that("calc_op runs with problematic input", {
    expect_no_error(
      calc_op(tbl[[i]])
    )
  })
  test_that("calc_iba runs with problematic input", {
    expect_no_error(
      calc_iba(tbl[[i]])
    )
  })
  test_that("calc_bcd runs with problematic input", {
    expect_no_error(
      calc_bcd(tbl[[i]])
    )
  })
  test_that("calc_gmean runs with problematic input", {
    expect_no_error(
      calc_gmean(tbl[[i]])
    )
  })
  test_that("calc_icsi runs with problematic input", {
    expect_no_error(
      calc_icsi(tbl[[i]])
    )
  })
}




