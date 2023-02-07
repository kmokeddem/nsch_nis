a = read.csv("joined_data.csv")

a$DTAP = a$DTaP.3Doses.13 - a$DTaP.3Doses.7
a$HepB = a$HepB.2Doses.13 - a$HepB.2Doses.5
a$Hib = a$Hib.2Doses.13 - a$Hib.2Doses.5
a$MMR = a$MMR.1Dose.24 - a$MMR.1Dose.13
a$PCV = a$PCV.3Doses.13 - a$PCV.3Doses.7
a$Polio = a$Polio.2Doses.13 - a$Polio.2Doses.5

fit_aluminum_asd <- lm("asd_prev ~ I(DTAP + HepB+ Hib + PCV + Polio) ", a, weights=a$asd_count)
fit_aluminum_allergies <- lm("allergies_prev ~ I(DTAP + HepB+ Hib + PCV + Polio) ", a, weights=a$allergies_count)
fit_aluminum_adhd <- lm("adhd_prev ~ I(DTAP + HepB+ Hib + PCV + Polio) ", a, weights=a$adhd_count)
fit_aluminum_asthma <- lm("asthma_prev ~ I(DTAP + HepB+ Hib + PCV + Polio) ", a, weights=a$asthma_count)
fit_aluminum_epilepsy <- lm("epilepsy_prev ~ I(DTAP + HepB+ Hib + PCV + Polio) ", a, weights=a$epilepsy_count)
fit_aluminum_diabetes <- lm("diabetes_prev ~ I(DTAP + HepB+ Hib + PCV + Polio) ", a, weights=a$diabetes_count)
fit_aluminum_tourettes <- lm("tourettes_prev ~ I(DTAP + HepB+ Hib + PCV + Polio) ", a, weights=a$tourettes_count)

summary(fit_aluminum_asd)
summary(fit_aluminum_allergies)
summary(fit_aluminum_adhd)
summary(fit_aluminum_asthma)
summary(fit_aluminum_epilepsy)
summary(fit_aluminum_diabetes)
summary(fit_aluminum_tourettes)
