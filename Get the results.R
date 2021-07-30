#Get the results
#get the results part
measures <- matrix(results3[1:18], nrow = 2, dimnames = list(c("RMSE", "KLD"), c("FLR",
                                                                                 "BLR",
                                                                                 "BMA",
                                                                                 "Stacking",
                                                                                 "Lasso",
                                                                                 "Ridge",
                                                                                 "Elastic.net",
                                                                                 "RF",
                                                                                 "SSVS")))
matrix(results3[19:20], nrow = 2, dimnames = list(c("Total", "Best"), c("PMP")))

matrix(results3[21:153], nrow = 19, dimnames = list(c("Intercept", names(x)), c("FLR",
                                                                                "BLR",
                                                                                "BMA",
                                                                                "Lasso",
                                                                                "Ridge",
                                                                                "Elastic.net",
                                                                                "SSVS")))

