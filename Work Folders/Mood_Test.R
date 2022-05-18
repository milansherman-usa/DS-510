dummy <- data.frame(replicate(10,sample(0:100,52,rep=TRUE)))
dummy$period <- 'previous'
for (i in 47:52) {dummy$period[i] <- 'current'}
mood.medtest(dummy$X1, dummy$period, exact = TRUE)$p.value
mood.medtest(dummy$X2, dummy$period, exact = TRUE)$p.value
mood.medtest(dummy$X3, dummy$period, exact = TRUE)$p.value
mood.medtest(dummy$X4, dummy$period, exact = TRUE)$p.value
mood.medtest(dummy$X5, dummy$period, exact = TRUE)$p.value
mood.medtest(dummy$X6, dummy$period, exact = TRUE)$p.value
mood.medtest(dummy$X7, dummy$period, exact = TRUE)$p.value
mood.medtest(dummy$X8, dummy$period, exact = TRUE)$p.value
mood.medtest(dummy$X9, dummy$period, exact = TRUE)$p.value
mood.medtest(dummy$X10, dummy$period, exact = TRUE)$p.value

dummy_median7 <- median(dummy$X7)
dummy_previous7 <- filter(dummy, dummy$period == 'previous')
dummy_current7 <- filter(dummy, dummy$period == 'current')
dummy_previous7$above_median <- nrow(subset(dummy_previous7, dummy_previous7$X7 > dummy_median7))
dummy_current7$above_median <- nrow(subset(dummy_current7, dummy_current7$X7 > dummy_median7))

for (i in 1:52) {
  dummy$X11[i] <- i*cos(i)
}

dummy_test <- matrix(c(nrow(subset(dummy_previous6, dummy_previous6$X6 > dummy_median6)), 
                       46 - nrow(subset(dummy_previous6, dummy_previous6$X6 > dummy_median6)),
                       nrow(subset(dummy_current6, dummy_current6$X6 > dummy_median6)),
                        6 - nrow(subset(dummy_current6, dummy_current6$X6 > dummy_median6))),
                     nrow = 2)

fisher.test(dummy_test, alternative = "two.sided")

chisq.test(dummy_test)
