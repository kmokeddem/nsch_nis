a = read.csv("joined_data.csv")

sum(a$asd_prev * a$asd_count)/sum(a$asd_count)
sum(a$allergies_prev * a$allergies_count)/sum(a$allergies_count)
sum(a$adhd_prev * a$adhd_count)/sum(a$adhd_count)
sum(a$asthma_prev * a$asthma_count)/sum(a$asthma_count)
sum(a$epilepsy_prev * a$epilepsy_count)/sum(a$epilepsy_count)
sum(a$tourettes_prev * a$tourettes_count)/sum(a$tourettes_count)


library(scipub)
correltable(a[,c("asd_prev", "allergies_prev", "adhd_prev",  "asthma_prev", "epilepsy_prev", "tourettes_prev")], tri="all")

a$DTaP.3 = a$DTaP.1Dose.3 
a$DTaP.5 = a$DTaP.2Doses.5
a$DTaP.7 = a$DTaP.3Doses.7
a$DTaP.13 = a$DTaP.3Doses.13 - a$DTaP.3Doses.7 
a$DTaP.19 = a$DTaP.3Doses.19 - a$DTaP.3Doses.13
a$DTaP.24 = (a$DTaP.3Doses.24 - a$DTaP.3Doses.19) + (a$DTaP.4Doses.24 - a$DTaP.4Doses.19)
a$DTaP.35 = (a$DTaP.3Doses.35 - a$DTaP.3Doses.24) + (a$DTaP.4Doses.35 - a$DTaP.4Doses.24)

a$HepB.3 = a$HepB.1Dose.3 
a$HepB.5 = a$HepB.2Doses.5
a$HepB.7 =  a$HepB.2Doses.7 - a$HepB.2Doses.5 
a$HepB.13 = (a$HepB.2Doses.13 - a$HepB.2Doses.7) + (a$HepB.3Doses.13 - a$HepB.3Doses.7)
a$HepB.19 = a$HepB.3Doses.19 - a$HepB.3Doses.13
a$HepB.24 = a$HepB.3Doses.24 - a$HepB.3Doses.19
a$HepB.35 = a$HepB.3Doses.35 - a$HepB.3Doses.24

a$Hib.3 = a$Hib.1Dose.3 
a$Hib.5 = a$Hib.2Doses.5 
a$Hib.7 = a$Hib.2Doses.7 - a$Hib.2Doses.5
a$Hib.13 = (a$Hib.2Doses.13 - a$Hib.2Doses.7) + (a$Hib.3Doses.13 - a$Hib.3Doses.7)
a$Hib.19 = a$Hib.3Doses.19 - a$Hib.3Doses.13
a$Hib.24 = a$Hib.3Doses.24 - a$Hib.3Doses.19
a$Hib.35 = a$Hib.3Doses.35 - a$Hib.3Doses.24

a$PCV.3 = a$PCV.1Dose.3
a$PCV.5 = a$PCV.2Doses.5
a$PCV.7 = a$PCV.3Doses.7
a$PCV.13 = a$PCV.3Doses.13 - a$PCV.3Doses.7
a$PCV.19 = a$PCV.3Doses.19 - a$PCV.3Doses.13
a$PCV.24 = (a$PCV.3Doses.24 - a$PCV.3Doses.19) + (a$PCV.4Doses.24 - a$PCV.4Doses.19)
a$PCV.35 = (a$PCV.3Doses.35 - a$PCV.3Doses.24) + (a$PCV.4Doses.35 - a$PCV.4Doses.24)
  
a$Polio.3 = a$Polio.1Dose.3
a$Polio.5 = a$Polio.2Doses.5 
a$Polio.7 = a$Polio.2Doses.7 - a$Polio.2Doses.5 
a$Polio.13 = a$Polio.2Doses.13 - a$Polio.2Doses.7
a$Polio.19 = a$Polio.3Doses.19
a$Polio.24 = a$Polio.3Doses.24 - a$Polio.3Doses.19
a$Polio.35 = a$Polio.3Doses.35 - a$Polio.3Doses.24

a$All.3 = a$DTaP.3 + a$HepB.3 + a$Hib.3 + a$PCV.3 + a$Polio.3
a$All.5 = a$DTaP.5 + a$HepB.5 + a$Hib.5 + a$PCV.5 + a$Polio.5
a$All.7 = a$DTaP.7 + a$HepB.7 + a$Hib.7 + a$PCV.7 + a$Polio.7
a$All.13 = a$DTaP.13 + a$HepB.13 + a$Hib.13 + a$PCV.13 + a$Polio.13
a$All.19 = a$DTaP.19 + a$HepB.19 + a$Hib.19 + a$PCV.19 + a$Polio.19
a$All.24 = a$DTaP.24 + a$HepB.24 + a$Hib.24 + a$PCV.24 + a$Polio.24
a$All.35 = a$DTaP.35 + a$HepB.35 + a$Hib.35 + a$PCV.35 + a$Polio.35

fit_asd_3 <- lm("asd_prev ~ All.3", a, weights=a$asd_count)
fit_allergies_3 <- lm("allergies_prev ~ All.3", a, weights=a$allergies_count)
fit_adhd_3 <- lm("adhd_prev ~ All.3", a, weights=a$adhd_count)
fit_asthma_3 <- lm("asthma_prev ~ All.3", a, weights=a$asthma_count)
fit_epilepsy_3 <- lm("epilepsy_prev ~ All.3", a, weights=a$epilepsy_count)
fit_tourettes_3 <- lm("tourettes_prev ~ All.3", a, weights=a$tourettes_count)

summary(fit_asd_3)
summary(fit_allergies_3)
summary(fit_adhd_3)
summary(fit_asthma_3)
summary(fit_epilepsy_3)
summary(fit_tourettes_3)

fit_asd_5 <- lm("asd_prev ~ All.5", a, weights=a$asd_count)
fit_allergies_5 <- lm("allergies_prev ~ All.5", a, weights=a$allergies_count)
fit_adhd_5 <- lm("adhd_prev ~ All.5", a, weights=a$adhd_count)
fit_asthma_5 <- lm("asthma_prev ~ All.5", a, weights=a$asthma_count)
fit_epilepsy_5 <- lm("epilepsy_prev ~ All.5", a, weights=a$epilepsy_count)
fit_tourettes_5 <- lm("tourettes_prev ~ All.5", a, weights=a$tourettes_count)

summary(fit_asd_5)
summary(fit_allergies_5)
summary(fit_adhd_5)
summary(fit_asthma_5)
summary(fit_epilepsy_5)
summary(fit_tourettes_5)

fit_asd_7 <- lm("asd_prev ~ All.7", a, weights=a$asd_count)
fit_allergies_7 <- lm("allergies_prev ~ All.7", a, weights=a$allergies_count)
fit_adhd_7 <- lm("adhd_prev ~ All.7", a, weights=a$adhd_count)
fit_asthma_7 <- lm("asthma_prev ~ All.7", a, weights=a$asthma_count)
fit_epilepsy_7 <- lm("epilepsy_prev ~ All.7", a, weights=a$epilepsy_count)
fit_tourettes_7 <- lm("tourettes_prev ~ All.7", a, weights=a$tourettes_count)

summary(fit_asd_7)
summary(fit_allergies_7)
summary(fit_adhd_7)
summary(fit_asthma_7)
summary(fit_epilepsy_7)
summary(fit_tourettes_7)

fit_asd_13 <- lm("asd_prev ~ All.13", a, weights=a$asd_count)
fit_allergies_13 <- lm("allergies_prev ~ All.13", a, weights=a$allergies_count)
fit_adhd_13 <- lm("adhd_prev ~ All.13", a, weights=a$adhd_count)
fit_asthma_13 <- lm("asthma_prev ~ All.13", a, weights=a$asthma_count)
fit_epilepsy_13 <- lm("epilepsy_prev ~ All.13", a, weights=a$epilepsy_count)
fit_tourettes_13 <- lm("tourettes_prev ~ All.13", a, weights=a$tourettes_count)

summary(fit_asd_13)
summary(fit_allergies_13)
summary(fit_adhd_13)
summary(fit_asthma_13)
summary(fit_epilepsy_13)
summary(fit_tourettes_13)

fit_asd_19 <- lm("asd_prev ~ All.19", a, weights=a$asd_count)
fit_allergies_19 <- lm("allergies_prev ~ All.19", a, weights=a$allergies_count)
fit_adhd_19 <- lm("adhd_prev ~ All.19", a, weights=a$adhd_count)
fit_asthma_19 <- lm("asthma_prev ~ All.19", a, weights=a$asthma_count)
fit_epilepsy_19 <- lm("epilepsy_prev ~ All.19", a, weights=a$epilepsy_count)
fit_tourettes_19 <- lm("tourettes_prev ~ All.19", a, weights=a$tourettes_count)

summary(fit_asd_19)
summary(fit_allergies_19)
summary(fit_adhd_19)
summary(fit_asthma_19)
summary(fit_epilepsy_19)
summary(fit_tourettes_19)

fit_asd_24 <- lm("asd_prev ~ All.24", a, weights=a$asd_count)
fit_allergies_24 <- lm("allergies_prev ~ All.24", a, weights=a$allergies_count)
fit_adhd_24 <- lm("adhd_prev ~ All.24", a, weights=a$adhd_count)
fit_asthma_24 <- lm("asthma_prev ~ All.24", a, weights=a$asthma_count)
fit_epilepsy_24 <- lm("epilepsy_prev ~ All.24", a, weights=a$epilepsy_count)
fit_tourettes_24 <- lm("tourettes_prev ~ All.24", a, weights=a$tourettes_count)

summary(fit_asd_24)
summary(fit_allergies_24)
summary(fit_adhd_24)
summary(fit_asthma_24)
summary(fit_epilepsy_24)
summary(fit_tourettes_24)

fit_asd_35 <- lm("asd_prev ~ All.35", a, weights=a$asd_count)
fit_allergies_35 <- lm("allergies_prev ~ All.35", a, weights=a$allergies_count)
fit_adhd_35 <- lm("adhd_prev ~ All.35", a, weights=a$adhd_count)
fit_asthma_35 <- lm("asthma_prev ~ All.35", a, weights=a$asthma_count)
fit_epilepsy_35 <- lm("epilepsy_prev ~ All.35", a, weights=a$epilepsy_count)
fit_tourettes_35 <- lm("tourettes_prev ~ All.35", a, weights=a$tourettes_count)

summary(fit_asd_35)
summary(fit_allergies_35)
summary(fit_adhd_35)
summary(fit_asthma_35)
summary(fit_epilepsy_35)
summary(fit_tourettes_35)

correltable(a, vars = c("asd_prev", "allergies_prev", "adhd_prev", "asthma_prev", "epilepsy_prev", "tourettes_prev"), vars2 = c("DTaP.13", "HepB.13", "Hib.13", "PCV.13", "Polio.13"), tri="all")
t.test(c(.24,.26,.30,.20,.13,.09))
t.test(c(.22,.22,.15,.25,.05,.20))
t.test(c(.17,.07,.20,.06,.11,.03))
t.test(c(.25,.28,.32,.22,.10,.14))
t.test(c(.19,.17,.17,.11,.09,.03))


correltable(a, vars = c("asd_prev", "allergies_prev", "adhd_prev", "asthma_prev", "epilepsy_prev", "tourettes_prev"), vars2 = c("DTaP.7", "HepB.7", "Hib.7", "PCV.7", "Polio.7"), tri="all")
t.test(c(-.22, -.21, -.25, -.16, -.13, -.08))
t.test(c(.23,.25,.33,.25,.12,.15))
t.test(c(.21,.23,.26,.20,.14,.15))
t.test(c(-.26,-.24,-.29,-.20,-.13,-.12))
t.test(c(.21,.27,.27,.21,.14,.11))

correltable(a, vars = c("asd_prev", "allergies_prev", "adhd_prev", "asthma_prev", "epilepsy_prev", "tourettes_prev"), vars2 = c("DTaP.19", "HepB.19", "Hib.19", "PCV.19", "Polio.19"), tri="all")
t.test(c(.07,.05,.10,.02,.13,.10))
t.test(c(.11,.12,.14,.07,.10,.10))
t.test(c(-.11,-.12,-.18,-.26,-.07,-.14))
t.test(c(.17,.12,.15,.06,.18,.11))
t.test(c(-.15,-.08,-.11,-.12,-.02,-.06))



correltable(a, vars = c("asd_prev", "allergies_prev", "adhd_prev", "asthma_prev", "epilepsy_prev", "tourettes_prev"), vars2 = c("All.3", "All.5", "All.7", "All.13", "All.19", "All.24", "All.35"), tri="all")
t.test(c(-.20, -.14, -.22, -.16, -.10, -.10))
t.test(c(-.23,-.22,-.27,-.21,-.16,-.11))
t.test(c(-.13,-.07,-.11,-.04,-.05,-.02))
t.test(c(.31,.28,.31,.25,.13,.17))
t.test(c(-.09,-.08,-.14,-.25,-.01,-.10))
t.test(c(.16,.15,.20,.14,.03,.02))
t.test(c(.11,.14,.10,.02,.09,.09))


fit_asd_13_under3 <- lm("asd_prev3 ~ All.13", a, weights=a$asd_count)

summary(fit_asd_13_under3)

correltable(a[a$year == 2011,], vars = c("asd_prev", "allergies_prev", "adhd_prev", "asthma_prev", "epilepsy_prev", "tourettes_prev"), vars2 = c("All.13"), tri="all")
correltable(a[a$year == 2012,], vars = c("asd_prev", "allergies_prev", "adhd_prev", "asthma_prev", "epilepsy_prev", "tourettes_prev"), vars2 = c("All.13"), tri="all")
correltable(a[a$year == 2013,], vars = c("asd_prev", "allergies_prev", "adhd_prev", "asthma_prev", "epilepsy_prev", "tourettes_prev"), vars2 = c("All.13"), tri="all")
correltable(a[a$year == 2014,], vars = c("asd_prev", "allergies_prev", "adhd_prev", "asthma_prev", "epilepsy_prev", "tourettes_prev"), vars2 = c("All.13"), tri="all")
correltable(a[a$year == 2015,], vars = c("asd_prev", "allergies_prev", "adhd_prev", "asthma_prev", "epilepsy_prev", "tourettes_prev"), vars2 = c("All.13"), tri="all")
correltable(a[a$year == 2016,], vars = c("asd_prev", "allergies_prev", "adhd_prev", "asthma_prev", "epilepsy_prev", "tourettes_prev"), vars2 = c("All.13"), tri="all")
correltable(a[a$year == 2017,], vars = c("asd_prev", "allergies_prev", "adhd_prev", "asthma_prev", "epilepsy_prev", "tourettes_prev"), vars2 = c("All.13"), tri="all")

cor_coefs = c(.18,.18,.08,.21,.09,-.04,.07,.16,.35,.01,.27,.10,.37,-.13,.16,.01,-.15,.21,.23,-.03,.38,.1,.17,-.3,.19,.23,.22,.27)
t.test(cor_coefs)

dr <- function(x, a, b)
{
	a * x + b
}

threshold_dose_response <- function(x, a, b) 
{
	response = dr(9,a,b) * x$All.13 + dr(15, a, b) * x$All.19 + dr(21, a, b) * x$All.24 + dr(30, a, b) * x$All.35
	return sum(response - x$asd_prev)^2)
}

min_residuals <- function(data, par) 
{
	with(data, threshold_dose_response(data,par[1],par[2]))
}




