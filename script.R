a = read.csv("joined_data.csv")

sum(a$asd_prev * a$asd_count)/sum(a$asd_count)
sum(a$allergies_prev * a$allergies_count)/sum(a$allergies_count)
sum(a$adhd_prev * a$adhd_count)/sum(a$adhd_count)
sum(a$arthritis_prev * a$arthritis_count)/sum(a$arthritis_count)
sum(a$asthma_prev * a$asthma_count)/sum(a$asthma_count)
sum(a$diabetes_prev * a$diabetes_count)/sum(a$diabetes_count)
sum(a$epilepsy_prev * a$epilepsy_count)/sum(a$epilepsy_count)
sum(a$tourettes_prev * a$tourettes_count)/sum(a$tourettes_count)


library(scipub)
correltable(a[,c("asd_prev", "allergies_prev", "adhd_prev",  "asthma_prev")], tri="all")

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

summary(fit_asd_3)
summary(fit_allergies_3)
summary(fit_adhd_3)
summary(fit_asthma_3)

fit_asd_5 <- lm("asd_prev ~ All.5", a, weights=a$asd_count)
fit_allergies_5 <- lm("allergies_prev ~ All.5", a, weights=a$allergies_count)
fit_adhd_5 <- lm("adhd_prev ~ All.5", a, weights=a$adhd_count)
fit_asthma_5 <- lm("asthma_prev ~ All.5", a, weights=a$asthma_count)

summary(fit_asd_5)
summary(fit_allergies_5)
summary(fit_adhd_5)
summary(fit_asthma_5)

fit_asd_7 <- lm("asd_prev ~ All.7", a, weights=a$asd_count)
fit_allergies_7 <- lm("allergies_prev ~ All.7", a, weights=a$allergies_count)
fit_adhd_7 <- lm("adhd_prev ~ All.7", a, weights=a$adhd_count)
fit_asthma_7 <- lm("asthma_prev ~ All.7", a, weights=a$asthma_count)

summary(fit_asd_7)
summary(fit_allergies_7)
summary(fit_adhd_7)
summary(fit_asthma_7)

fit_asd_13 <- lm("asd_prev ~ All.13", a, weights=a$asd_count)
fit_allergies_13 <- lm("allergies_prev ~ All.13", a, weights=a$allergies_count)
fit_adhd_13 <- lm("adhd_prev ~ All.13", a, weights=a$adhd_count)
fit_asthma_13 <- lm("asthma_prev ~ All.13", a, weights=a$asthma_count)

summary(fit_asd_13)
summary(fit_allergies_13)
summary(fit_adhd_13)
summary(fit_asthma_13)

fit_asd_19 <- lm("asd_prev ~ All.19", a, weights=a$asd_count)
fit_allergies_19 <- lm("allergies_prev ~ All.19", a, weights=a$allergies_count)
fit_adhd_19 <- lm("adhd_prev ~ All.19", a, weights=a$adhd_count)
fit_asthma_19 <- lm("asthma_prev ~ All.19", a, weights=a$asthma_count)

summary(fit_asd_19)
summary(fit_allergies_19)
summary(fit_adhd_19)
summary(fit_asthma_19)

fit_asd_24 <- lm("asd_prev ~ All.24", a, weights=a$asd_count)
fit_allergies_24 <- lm("allergies_prev ~ All.24", a, weights=a$allergies_count)
fit_adhd_24 <- lm("adhd_prev ~ All.24", a, weights=a$adhd_count)
fit_asthma_24 <- lm("asthma_prev ~ All.24", a, weights=a$asthma_count)

summary(fit_asd_24)
summary(fit_allergies_24)
summary(fit_adhd_24)
summary(fit_asthma_24)

fit_asd_35 <- lm("asd_prev ~ All.35", a, weights=a$asd_count)
fit_allergies_35 <- lm("allergies_prev ~ All.35", a, weights=a$allergies_count)
fit_adhd_35 <- lm("adhd_prev ~ All.35", a, weights=a$adhd_count)
fit_asthma_35 <- lm("asthma_prev ~ All.35", a, weights=a$asthma_count)

summary(fit_asd_35)
summary(fit_allergies_35)
summary(fit_adhd_35)
summary(fit_asthma_35)


correltable(a, vars = c("asd_prev", "allergies_prev", "adhd_prev", "asthma_prev"), vars2 = c("All.3", "All.5", "All.7", "All.13", "All.19", "All.24", "All.35"), tri="all")


