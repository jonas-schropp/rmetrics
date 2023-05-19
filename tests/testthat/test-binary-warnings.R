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

  x <- calc_prevalence(tbl[[i]], ci.type, ci.level)
  if(is.na(x[1])) {
    test_that("calc_prevalence returns a warning if it can not be calculated", {
        expect_warning(calc_prevalence(tbl[[i]], ci.type, ci.level))
    })
    }

  x <- calc_acc(tbl[[i]], ci.type, ci.level)
  if(is.na(x[1])) { test_that("calc_acc returns a warning if it can not be calculated", {
      expect_warning(calc_acc(tbl[[i]], ci.type, ci.level))
  })}

  x <- calc_tpr(tbl[[i]], ci.type, ci.level)
  if(is.na(x[1])) { test_that("calc_tpr returns a warning if it can not be calculated", {
      expect_warning(calc_tpr(tbl[[i]], ci.type, ci.level))
  })}

  x <- calc_tnr(tbl[[i]], ci.type, ci.level)
  if(is.na(x[1])) { test_that("calc_tnr returns a warning if it can not be calculated", {
      expect_warning(calc_tnr(tbl[[i]], ci.type, ci.level))
  })}

  x <- calc_precision(tbl[[i]], ci.type, ci.level)
  if(is.na(x[1])) { test_that("calc_precision returns a warning if it can not be calculated", {
      expect_warning(calc_precision(tbl[[i]], ci.type, ci.level))
  })}

  x <- calc_npv(tbl[[i]], ci.type, ci.level)
  if(is.na(x[1])) { test_that("calc_npv returns a warning if it can not be calculated", {
      expect_warning(calc_npv(tbl[[i]], ci.type, ci.level))
  })}

  x <- calc_fnr(tbl[[i]], ci.type, ci.level)
  if(is.na(x[1])) { test_that("calc_fnr returns a warning if it can not be calculated", {
      expect_warning(calc_fnr(tbl[[i]], ci.type, ci.level))
  })}

  x <- calc_fpr(tbl[[i]], ci.type, ci.level)
  if(is.na(x[1])) { test_that("calc_fpr returns a warning if it can not be calculated", {
      expect_warning(calc_fpr(tbl[[i]], ci.type, ci.level))
  })}

  x <- calc_fdr(tbl[[i]], ci.type, ci.level)
  if(is.na(x[1])) { test_that("calc_fdr returns a warning if it can not be calculated", {
      expect_warning(calc_fdr(tbl[[i]], ci.type, ci.level))
  })}

  x <- calc_for(tbl[[i]], ci.type, ci.level)
  if(is.na(x[1])) { test_that("calc_for returns a warning if it can not be calculated", {
      expect_warning(calc_for(tbl[[i]], ci.type, ci.level))
  })}

  x <- calc_plr(tbl[[i]], ci.type, ci.level)
  if(is.na(x[1])) { test_that("calc_plr returns a warning if it can not be calculated", {
      expect_warning(calc_plr(tbl[[i]], ci.type, ci.level))
  })}

  x <- calc_nlr(tbl[[i]], ci.type, ci.level)
  if(is.na(x[1])) { test_that("calc_nlr returns a warning if it can not be calculated", {
      expect_warning(calc_nlr(tbl[[i]], ci.type, ci.level))
  })}

  x <- calc_err(tbl[[i]], ci.type, ci.level)
  if(is.na(x[1])) { test_that("calc_err returns a warning if it can not be calculated", {
      expect_warning(calc_err(tbl[[i]], ci.type, ci.level))
  })}

  x <- calc_f(tbl[[i]], 0.5)
  if(is.na(x[1])) { test_that("calc_f05 returns a warning if it can not be calculated", {
      expect_warning(calc_f(tbl[[i]], 0.5))
  })}

  x <- calc_f(tbl[[i]], 1)
  if(is.na(x[1])) { test_that("calc_f1 returns a warning if it can not be calculated", {
      expect_warning(calc_f(tbl[[i]], 1))
  })}

  x <- calc_f(tbl[[i]], 2)
  if(is.na(x[1])) { test_that("calc_f2 returns a warning if it can not be calculated", {
      expect_warning(calc_f(tbl[[i]], 2))
  })}

  x <- calc_phi(tbl[[i]])
  if(is.na(x[1])) { test_that("calc_phi returns a warning if it can not be calculated", {
      expect_warning(calc_phi(tbl[[i]]))
  })}

  x <- calc_informedness(tbl[[i]])
  if(is.na(x[1])) { test_that("calc_informedness returns a warning if it can not be calculated", {
      expect_warning(calc_informedness(tbl[[i]]))
  })}

  x <- calc_markedness(tbl[[i]])
  if(is.na(x[1])) { test_that("calc_markedness returns a warning if it can not be calculated", {
      expect_warning(calc_markedness(tbl[[i]]))
  })}

  x <- calc_dor(tbl[[i]])
  if(is.na(x[1])) { test_that("calc_dor returns a warning if it can not be calculated", {
      expect_warning(calc_dor(tbl[[i]]))
  })}

  x <- calc_racc(tbl[[i]], unbiased = FALSE)
  if(is.na(x[1])) { test_that("calc_racc returns a warning if it can not be calculated", {
      expect_warning(calc_racc(tbl[[i]], unbiased = FALSE))
  })}

  x <- calc_racc(tbl[[i]], unbiased = TRUE)
  if(is.na(x[1])) { test_that("calc_raccu returns a warning if it can not be calculated", {
      expect_warning(calc_racc(tbl[[i]], unbiased = TRUE))
  })}

  x <- calc_ooc(tbl[[i]])
  if(is.na(x[1])) { test_that("calc_ooc returns a warning if it can not be calculated", {
      expect_warning(calc_ooc(tbl[[i]]))
  })}

  x <- calc_oc(tbl[[i]])
  if(is.na(x[1])) { test_that("calc_oc returns a warning if it can not be calculated", {
      expect_warning(calc_oc(tbl[[i]]))
  })}

  x <- calc_bbs(tbl[[i]])
  if(is.na(x[1])) { test_that("calc_bbs returns a warning if it can not be calculated", {
      expect_warning(calc_bbs(tbl[[i]]))
  })}

  x <- calc_af(tbl[[i]])
  if(is.na(x[1])) { test_that("calc_af returns a warning if it can not be calculated", {
      expect_warning(calc_af(tbl[[i]]))
  })}

  x <- calc_gmean(tbl[[i]], T)
  if(is.na(x[1])) { test_that("calc_agm returns a warning if it can not be calculated", {
      expect_warning(calc_gmean(tbl[[i]], T))
  })}

  x <- calc_q(tbl[[i]])
  if(is.na(x[1])) { test_that("calc_q returns a warning if it can not be calculated", {
      expect_warning(calc_q(tbl[[i]]))
  })}

  x <- calc_mcc(tbl[[i]])
  if(is.na(x[1])) { test_that("calc_mcc returns a warning if it can not be calculated", {
      expect_warning(calc_mcc(tbl[[i]]))
  })}

  x <- calc_jaccard(tbl[[i]])
  if(is.na(x[1])) { test_that("calc_ji returns a warning if it can not be calculated", {
      expect_warning(calc_jaccard(tbl[[i]]))
  })}

  x <- calc_is(tbl[[i]])
  if(is.na(x[1])) { test_that("calc_is returns a warning if it can not be calculated", {
      expect_warning(calc_is(tbl[[i]]))
  })}

  x <- calc_auroc(tbl[[i]])
  if(is.na(x[1])) { test_that("calc_auroc returns a warning if it can not be calculated", {
      expect_warning(calc_auroc(tbl[[i]]))
  })}

  x <- calc_aupr(tbl[[i]])
  if(is.na(x[1])) { test_that("calc_aupr returns a warning if it can not be calculated", {
      expect_warning(calc_aupr(tbl[[i]]))
  })}

  x <- calc_dind(tbl[[i]])
  if(is.na(x[1])) { test_that("calc_dind returns a warning if it can not be calculated", {
      expect_warning(calc_dind(tbl[[i]]))
  })}

  x <- calc_sind(tbl[[i]])
  if(is.na(x[1])) { test_that("calc_sind returns a warning if it can not be calculated", {
      expect_warning(calc_sind(tbl[[i]]))
  })}

  x <- calc_dp(tbl[[i]])
  if(is.na(x[1])) { test_that("calc_dp returns a warning if it can not be calculated", {
      expect_warning(calc_dp(tbl[[i]]))
  })}

  x <- calc_gini(tbl[[i]])
  if(is.na(x[1])) { test_that("calc_gini returns a warning if it can not be calculated", {
      expect_warning(calc_gini(tbl[[i]]))
  })}

  x <- calc_lift(tbl[[i]])
  if(is.na(x[1])) { test_that("calc_lift returns a warning if it can not be calculated", {
      expect_warning(calc_lift(tbl[[i]]))
  })}

  x <- calc_op(tbl[[i]])
  if(is.na(x[1])) { test_that("calc_op returns a warning if it can not be calculated", {
      expect_warning(calc_op(tbl[[i]]))
  })}

  x <- calc_iba(tbl[[i]])
  if(is.na(x[1])) { test_that("calc_iba returns a warning if it can not be calculated", {
      expect_warning(calc_iba(tbl[[i]]))
  })}

  x <- calc_bcd(tbl[[i]])
  if(is.na(x[1])) { test_that("calc_bcd returns a warning if it can not be calculated", {
      expect_warning(calc_bcd(tbl[[i]]))
  })}

  x <- calc_gmean(tbl[[i]])
  if(is.na(x[1])) { test_that("calc_gmean returns a warning if it can not be calculated", {
      expect_warning(calc_gmean(tbl[[i]]))
  })}

  x <- calc_icsi(tbl[[i]])
  if(is.na(x[1])) { test_that("calc_icsi returns a warning if it can not be calculated", {
      expect_warning(calc_icsi(tbl[[i]]))
  })}
}


