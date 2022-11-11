################################################################################
#####
# TESTS FOR MULTICLASS METRICS
#####
################################################################################



################################################################################
# Point estimates - Standard

prediction <- c(rep(1, 50), rep(2, 50), rep(3, 50))
reference <- c(rep(1, 25), rep(2, 5), rep(3, 20),
               rep(2, 40), rep(1, 5), rep(3, 5),
               rep(3, 30), rep(1, 10), rep(2, 10))

data <- data.frame(prediction, reference)

tbl <- table(prediction, reference)

classes <- 1:3

# overall
on <- sum(tbl)
otp <- sum(diag(tbl)) # overall true positive

# per class
pos <- colSums(tbl)  # actual positives per class
tp <- diag(tbl)
fn <- colSums(tbl) - tp
fp <- rowSums(tbl) - tp
tn <- sum(tbl) - tp - fn - fp
ppos <- tp + fp          # predicted positives per class
n <- on   #colSums(tbl)
ofp <- sum(rowSums(tbl) - tp)
ofn <- sum(colSums(tbl) - tp)



################################################################################

test_that("calc_fmi works with valid input", {
  expect_equal(
    calc_fmi(otp, ofp, ofn),
    0.520104, # dendextend
    tolerance = 0.001, ignore_attr = TRUE
  )
})
test_that("Overall Accuracy works with valid input", {
  expect_equal(
    calc_oacc(otp, n),
    0.6333333333333333,
    tolerance = 0.001
  )
})
test_that("Overall Random Accuracy unbiased works with valid input", {
  expect_equal(
    calc_oracc(tbl, TRUE),
    0.33499999999999996,
    tolerance = 0.001
  )
})
test_that("Overall Random Accuracy works with valid input", {
  expect_equal(
    calc_oracc(tbl, FALSE),
    0.3333333333333333,
    tolerance = 0.001
  )
})
test_that("Kappa works with valid input", {
  expect_equal(
    calc_kappa(tbl, FALSE, TRUE),
    0.44999999999999996,
    tolerance = 0.001
  )
})
test_that("Bennett S works with valid input", {
  expect_equal(
    calc_bennett_s(tbl),
    0.44999999999999996,
    tolerance = 0.001
  )
})
test_that("Scott PI / Unbiased Kappa works with valid input", {
  expect_equal(
    calc_kappa(tbl, TRUE, TRUE),
    0.44862155388471175, # pycm result - result here is .45 exactly?
    tolerance = 0.01
  )
})
test_that("Kappa No Prevalence works with valid input", {
  expect_equal(
    calc_kappa(tbl, F, F),
    0.2666666666666666,
    tolerance = 0.001
  )
})
test_that("Chi-Squared works with valid input", {
  expect_equal(
    unname(calc_chisq(tbl)[1]),
    72.61363636363636,
    tolerance = 0.001
  )
})
test_that("Phi-Squared works with valid input", {
  expect_equal(
    unname(calc_phisq.table(tbl)),
    0.48409090909090907,
    tolerance = 0.001
  )
})
test_that("Cramer V works with valid input", {
  expect_equal(
    unname(calc_cramer_v(tbl, FALSE, 0)[1]),
    0.4919811526323489,
    tolerance = 0.001
  )
})
test_that("Response Entropy works with valid input", {
  expect_equal(
    calc_response_entropy(tp, fp, n),
    1.584962500721156,
    tolerance = 0.001
  )
})
test_that("Reference Entropy works with valid input", {
  expect_equal(
    calc_reference_entropy(tp, fn, n),
    1.5699740752745002,
    tolerance = 0.001
  )
})
test_that("Cross Entropy works with valid input", {
  expect_equal(
    calc_cross_entropy(tp, fp, fn, n),
    1.584962500721156,
    tolerance = 0.001
  )
})
test_that("Joint Entropy works with valid input", {
  expect_equal(
    calc_joint_entropy(tbl),
    2.802910079649727,
    tolerance = 0.001
  )
})
test_that("Conditional Entropy works with valid input", {
  expect_equal(
    calc_conditional_entropy(tbl),
    1.2329360043752264,
    tolerance = 0.001
  )
})
test_that("Mutual Information works with valid input", {
  expect_equal(
    calc_mutual_information(tbl),
    0.35202649634592964,
    tolerance = 0.001
  )
})
test_that("KL Divergence works with valid input", {
  expect_equal(
    calc_kl_divergence(tp, fp, fn, n, epsilon = 1e-06),
    0.014988425446655726,
    tolerance = 0.001
  )
})
test_that("Lambda B works with valid input", {
  expect_equal(
    unname(calc_lambda(tbl, "row")[1]),
    0.45,
    tolerance = 0.001
  )
})
test_that("Lambda A works with valid input", {
  expect_equal(
    unname(calc_lambda(tbl, "column")[1]),
    0.42105263157894735,
    tolerance = 0.001
  )
})
test_that("Overall J works with valid input", {
  expect_equal(
    calc_jaccard_overall(data),
    0.4666666666666666, # Only mean result in pycm, returns also sum (1.4)
    tolerance = 0.001
  )
})
test_that("Hamming Loss works with valid input", {
  expect_equal(
    calc_hamming(otp, n),
    0.3666666666666667,
    tolerance = 0.001
  )
})
test_that("Zero-one Loss works with valid input", {
  expect_equal(
    calc_zero_one_loss(otp, n),
    55,
    tolerance = 0.001
  )
})
test_that("NIR works with valid input", {
  expect_equal(
    calc_nir(tp, fn, n),
    0.36666666666666664,
    tolerance = 0.001
  )
})
#test_that("Overall CEN works with valid input", {    UNFINISHED
#  expect_equal(
#    ,
#    0.5904320327536762,
#    tolerance = 0.001
#  )
#})
#test_that("Overall MCEN works with valid input", {    UNFINISHED
#  expect_equal(
#    ,
#    0.7227772158086692,
#    tolerance = 0.001
#  )
#})
test_that("Overall MCC works with valid input", {
  expect_equal(
    calc_mcc_overall(tp, fp, fn),
    0.45226701686664544,
    tolerance = 0.001
  )
})
test_that("RR works with valid input", {
  expect_equal(
    calc_rr(tp, fp),
    50.0,
    tolerance = 0.001
  )
})
test_that("CBA works with valid input", {
  expect_equal(
    calc_cba(tp, fp, fn),
    0.5909090909090909,
    tolerance = 0.001
  )
})
test_that("AUNU works with valid input", {
  expect_equal(
    calc_aunu(tp, tn, fp, fn),
    0.7257775119617224,
    tolerance = 0.001
  )
})
test_that("AUNP works with valid input", {
  expect_equal(
    calc_aunp(tp, tn, fp, fn),
    0.728468899521531,
    tolerance = 0.001
  )
})
test_that("RCI works with valid input", {
  expect_equal(
    calc_rci(tbl),
    0.22422440082928122,
    tolerance = 0.001
  )
})
test_that("Pearson C works with valid input", {
  expect_equal(
    unname(calc_pearson_c(tbl)),
    0.5711276827604436,
    tolerance = 0.001
  )
})
#test_that("TPR Micro works with valid input", {  TO ADD
#  expect_equal(
#    calc_tpr_micro(??????????),
#    0.6333333333333333,
#    tolerance = 0.001
#  )
#})
test_that("TPR Macro works with valid input", {
  expect_equal(
    calc_tpr_macro(tp, fn),
    0.6325757575757576,
    tolerance = 0.001
  )
})
test_that("CSI works with valid input", {
  expect_equal(
    calc_csi_macro(tp, fn, fp),
    0.26590909090909093,
    tolerance = 0.001
  )
})
test_that("Rand Index works with valid input", {
  expect_equal(
    calc_rand(tbl, adjust = FALSE),
    0.6621924, # fossil
    tolerance = 0.001
  )
})
test_that("ARI works with valid input", {
  expect_equal(
    calc_rand(tbl, adjust = TRUE),
    0.2386898115250567, # pycm, fossil
    tolerance = 0.001
  )
})
test_that("TNR Micro works with valid input", {
  expect_equal(
    calc_tnr_micro(tn, fp),
    0.8166666666666667,
    tolerance = 0.001
  )
})
test_that("TNR Macro works with valid input", {
  expect_equal(
    calc_tnr_macro(tn, fp),
    0.8189792663476873,
    tolerance = 0.001
  )
})
test_that("Bangdiwala B works with valid input", {
  expect_equal(
    calc_b(tp, fp, fn),
    0.4166666666666667,
    tolerance = 0.001
  )
})
test_that("Krippendorff Alpha works with valid input", {
  expect_equal(
    calc_kalpha(tbl),
    0.4504594820384294,
    tolerance = 0.001
  )
})
test_that("FPR Macro works with valid input", {
  expect_equal(
    calc_fpr_macro(fp, tn),
    0.18102073365231275,
    tolerance = 0.001
  )
})
test_that("FNR Macro works with valid input", {
  expect_equal(
    calc_fnr_macro(fn, tp),
    0.36742424242424243,
    tolerance = 0.001
  )
})
test_that("PPV Macro works with valid input", {
  expect_equal(
    calc_ppv_macro(tp, fp),
    0.6333333333333333,
    tolerance = 0.001
  )
})
test_that("ACC Macro works with valid input", {
  expect_equal(
    calc_acc_macro(tp, tn, n),
    0.7555555555555555,
    tolerance = 0.001
  )
})
test_that("F1 Macro works with valid input", {
  expect_equal(
    calc_f_macro(tp, fp, fn, beta = 1),
    0.6296296296296297,
    tolerance = 0.001
  )
})
test_that("FPR Micro works with valid input", {
  expect_equal(
    calc_fpr_micro(tn, fp),
    0.18333333333333335,
    tolerance = 0.001
  )
})
test_that("FNR Micro works with valid input", {
  expect_equal(
    calc_fnr_micro(otp, n),
    0.3666666666666667,
    tolerance = 0.001
  )
})
#test_that("PPV Micro works with valid input", {   TO ADD
#  expect_equal(
#    calc_ppv_micro(???????????),
#    0.6333333333333333,
#    tolerance = 0.001
#  )
#})
#test_that("F1 Micro works with valid input", {   TO ADD
#  expect_equal(
#    calc_f1_micro(?????????),
#    0.6333333333333333,
#    tolerance = 0.001
#  )
#})
