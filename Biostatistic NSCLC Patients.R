#----Information----

# Seminar Work Biostatistics
# NSCLC data analysis
# Claudio B?gli
# Last edited: 30.05.2023
# R version 4.0.3 (2020-10-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)

rm(list=ls())
setwd("C:/Users/claud/OneDrive/Desktop/Studies/Pharmaceutical Sciences/2. Semester/Finished/Biostatistic/Seminar Work")


#----Libraries----

library(datasets) # Data set package
library(DescTools)
library(methods)  # Basic statistical package
library(stats)    # Basic statistical package
library(stringr)   # tools for strings
library(tidyr)     # reshaping data
library(tidyverse) # Tidyverse (inlcudes dplyr)
library(grDevices)
library(lattice)   # Graphical package
library(scales)    # date/time scales for plots
library(ggplot2)  # advanced data visualization
library(utils)    # Basic tools
library(vcd)      # EDA tools
library(openintro) # Statistical curriculum including data
library(rcompanion)
library(multxpert) # Multiplicity package
library(corrplot)
library(dplyr)
library(knitr)
library(kableExtra)

#----Data Import----

nsclc <- read.csv(file="NSCLC.csv", header = TRUE, sep = ";")
head(nsclc)

#----Question 1.1-1.2----

# Checking column values datatypes
mode(nsclc$Case.ID)
mode(nsclc$cohort)
mode(nsclc$Age.at.Histological.Diagnosis)
mode(nsclc$weight)
mode(nsclc$Gender)
mode(nsclc$Smoking.status)
mode(nsclc$Histology)
mode(nsclc$Chemotherapy)
mode(nsclc$Radiation)
mode(nsclc$Recurrence) 
mode(nsclc$Survival.Status)
mode(nsclc$Time.to.Death..days.)
# result:
# nsclc$Time.to.Death..days. has wrong datatype

# Checking values for "special cases" with different values
which(nsclc$cohort != "AMC" & nsclc$cohort != "R01")
which(nsclc$Gender != "Male" & nsclc$Gender != "Female")
which(nsclc$Smoking.status != "Former" & nsclc$Smoking.status != "Current" & nsclc$Smoking.status != "Nonsmoker")
which(nsclc$Histology != "Adenocarcinoma" & nsclc$Histology != "Squamous cell carcinoma")
which(nsclc$Chemotherapy != "Yes" & nsclc$Chemotherapy != "No")
which(nsclc$Radiation != "Yes" & nsclc$Radiation != "No")
which(nsclc$Recurrence != "yes" & nsclc$Recurrence != "no")
which(nsclc$Survival.Status != "Dead" & nsclc$Survival.Status != "Alive")
# result:
# Column Histology: rows 76, 78, 80, 211: NCSLC NOS
# Column Chemotherapy: row 49: not collected 
# Column Radiation: row 49: not collected 
# Column Recurrence: row 49: not collected 

#----Question 1.3----

# Corrections:
nsclc$Days.to.Death <- as.integer(nsclc$Time.to.Death..days.) #introducing integer data type and NAs
mode(nsclc$Days.to.Death)


#----Question 2.1(.1)----

# Tables (contingency and frequency)
# tables: gender
ct_gender <- xtabs(~ cohort + Gender, data=nsclc, addNA=(ifany=TRUE))
corrplot(ct_gender, method = "color", is.corr = FALSE,
         title = "Contingency Table: Gender", xlab = "Gender", ylab = "Cohort",
         col = colorRampPalette(c("white", "steelblue"))(100), cl.pos="n",
         addCoef.col = "black", tl.srt = 0, tl.cex = 1, tl.col = "black", mar = c(4,4,4,4))


ft_gender <- round(prop.table(ct_gender), digits =2)
corrplot(ft_gender, method = "color", title = "Frequency Table: Gender",
         xlab = "Gender", ylab = "Cohort",
         addCoef.col = "black", tl.srt = 0, tl.col = "black", col.lim = c(0,1), mar = c(4,4,4,4))

# tables: smoking
ct_smoking <- xtabs(~ cohort + Smoking.status, data=nsclc, addNA=(ifany=TRUE))
corrplot(ct_smoking, method = "color", is.corr = FALSE, 
         title = "Contingency Table: Smoking Status", xlab = "Smoking Status", ylab = "Cohort",
         col = colorRampPalette(c("white", "steelblue"))(100), cl.pos="n",
         addCoef.col = "black", tl.srt = 0, tl.col = "black", mar = c(4,4,4,4))

ft_smoking <- round(prop.table(ct_smoking), digits =2)
corrplot(ft_smoking, method = "color", title = "Frequency Table: Smoking Status",
         xlab = "Smoking Status", ylab = "Cohort",
         addCoef.col = "black", tl.srt = 0, tl.col = "black", col.lim = c(0,1), mar = c(4,4,4,4))

# tables: histology
ct_histo <- xtabs(~ cohort + Histology, data=nsclc, addNA=(ifany=TRUE))
corrplot(ct_histo, method = "color", is.corr = FALSE, 
         title = "Contingency Table: Histological Finding", xlab = "Histological Finding", ylab = "Cohort",
         col = colorRampPalette(c("white", "steelblue"))(100), cl.pos="n",
         addCoef.col = "black", tl.srt = 0, tl.col = "black", mar = c(4,4,4,4))

# tables: chemotherapy
ct_chemo <- xtabs(~ cohort + Chemotherapy, data=nsclc, addNA=(ifany=TRUE))
corrplot(ct_chemo, method = "color", is.corr = FALSE, 
         title = "Contingency Table: Chemotherapy Status", xlab = "Prior Chemotherapy", ylab = "Cohort",
         col = colorRampPalette(c("white", "steelblue"))(100), cl.pos="n",
         addCoef.col = "black", tl.srt = 0, tl.col = "black", mar = c(4,4,4,4))

# tables: radiation
ct_rad <- xtabs(~ cohort + Radiation, data=nsclc, addNA=(ifany=TRUE))
corrplot(ct_rad, method = "color", is.corr = FALSE, 
         title = "Contingency Table: Radiation Status", xlab = "Prior Radiation", ylab = "Cohort",
         col = colorRampPalette(c("white", "steelblue"))(100), cl.pos="n",
         addCoef.col = "black", tl.srt = 0, tl.col = "black", mar = c(4,4,4,4))

# tables: recurrence
ct_rec <- xtabs(~ cohort + Recurrence, data=nsclc, addNA=(ifany=TRUE))
corrplot(ct_rec, method = "color", is.corr = FALSE, 
         title = "Contingency Table: Recurrence Status", xlab = "Recurrence", ylab = "Cohort",
         col = colorRampPalette(c("white", "steelblue"))(100), cl.pos="n",
         addCoef.col = "black", tl.srt = 0, tl.col = "black", mar = c(4,4,4,4))

# tables: survival status
ct_sur <- xtabs(~ cohort + Survival.Status, data=nsclc, addNA=(ifany=TRUE))
corrplot(ct_sur, method = "color", is.corr = FALSE, 
         title = "Contingency Table: Survival Status", xlab = "Survival Status", ylab = "Cohort",
         col = colorRampPalette(c("white", "steelblue"))(100), cl.pos="n",
         addCoef.col = "black", tl.srt = 0, tl.col = "black", mar = c(4,4,4,4))

ft_sur <- round(prop.table(ct_sur), digits =3)
corrplot(ft_sur, method = "color", title = "Frequency Table: Survival Status",
         xlab = "Survival Status", ylab = "Cohort",
         addCoef.col = "black", tl.srt = 0, tl.col = "black", col.lim = c(0,1), mar = c(4,4,4,4))


#----Question 2.1(.2)----

# Summary Statistics
# summary of age
sum_age <- aggregate(Age.at.Histological.Diagnosis ~ cohort, data=nsclc, summary)
sum_age

boxplot(Age.at.Histological.Diagnosis ~ cohort, data = sum_age, 
        main = "Age Distribution by Cohort", xlab = "Cohort", ylab = "Age [years]",
        col = "steelblue", border = "black")

a_AMC <- length(nsclc$Age.at.Histological.Diagnosis[nsclc$cohort == "AMC" & !is.na(nsclc$Age.at.Histological.Diagnosis)])
a_R01 <- length(nsclc$Age.at.Histological.Diagnosis[nsclc$cohort == "R01" & !is.na(nsclc$Age.at.Histological.Diagnosis)])
a_AMC
a_R01

# summary of weight
sum_weight <- aggregate(weight ~ cohort, data=nsclc, summary)
sum_weight

boxplot(weight ~ cohort, data = sum_weight, 
        main = "Weight Distribution by Cohort", xlab = "Cohort", ylab = "Weight [kg]",
        col = "steelblue", border = "black")

w_AMC <- length(nsclc$weight[nsclc$cohort == "AMC" & !is.na(nsclc$weight)])
w_R01 <- length(nsclc$weight[nsclc$cohort == "R01" & !is.na(nsclc$weight)])
w_AMC
w_R01

#----Question 3.1----

# tables: missing outcome variables conditioned to cohort
ct_out <- xtabs( ~ cohort + is.na(Days.to.Death), data=nsclc, addNA=(ifany=TRUE))
colnames(ct_out) <- c("Collected", "Not collected")
corrplot(ct_out, method = "color", is.corr = FALSE, 
         title = "Contigency Table: Collected and not collected values", xlab = "Number of Cases", ylab = "Cohort",
         col.lim = c(0,105), col = colorRampPalette(c("white", "steelblue"))(100), cl.pos="n", 
         addCoef.col = "black", tl.srt = 0, tl.col = "black", mar = c(4,4,4,4))

ft_out <- round(prop.table(ct_out), digits =2)
corrplot(ft_out, method = "color", title = "Frequency Table: Collected and not collected values",
         xlab = "Frequency of Cases", ylab = "Cohort",
         addCoef.col = "black", tl.srt = 0, tl.col = "black", col.lim = c(0,1), mar = c(4,4,4,4))


#----Question 3.2----

# histogram for the outcome variable in cohort
DTD <- nsclc[!is.na(nsclc$Days.to.Death),] # return dataset without NAs in column Days.to.Death for boxplot

bins <- round(1 + log2(length(unique(DTD$Days.to.Death)))) # calculates the required bins according to Sturges rule

hist(DTD$Days.to.Death, freq = FALSE, breaks = seq(0, max(DTD$Days.to.Death), length.out = bins),
     col = 'steelblue', main="Density of the outcome variable of both cohorts", xlab="Days to death",
     xlim = c(0, 2500), ylim = c(0, 0.00125))         

summary(DTD$Days.to.Death)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 6.0   281.5   557.0   695.4  1038.8  2356.0 

curve(dnorm(x, mean = mean(DTD$Days.to.Death), sd = sd(DTD$Days.to.Death)), add = TRUE, lwd = 1.5)

#----Question 3.3----

# comparison of distribution of outcome variable with theoretical normal and exponential distribution
days <- sort(DTD$Days.to.Death)
m_sd <- mean(days)
sd_sd <- sd(days)

quant_norm <- qnorm(ppoints(length(days)), mean = m_sd, sd = sd_sd)

quant_exp <- qexp(ppoints(length(days)), rate = 1/m_sd)


# QQ-plot for normal distribution with a reference line
plot(quant_norm, days, main = "QQ-Plot: Normal Distribution",
     xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", pch = 1)
qqline(days, col = "red", lwd = 1.5)
abline(0, 1, col = "red", lwd = 1.5)

# QQ-plot for exponential distribution with a reference line
plot(quant_exp, days, main = "QQ-Plot: Exponential Distribution",
     xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles", pch = 1)
qqline(days, col = "red", lwd = 1.5)
abline(0, 1, col = "red", lwd = 1.5)


#----Question 4.2----

# Create separate datasets for regression models. DTD for exclusion of NA values.

AMC <- subset(DTD, cohort == "AMC")
R01 <- subset(DTD, cohort == "R01")

# Regression for AMC
AMC_model <- lm(AMC$Days.to.Death ~ AMC$Age.at.Histological.Diagnosis)
sum_AMC_mod <- summary(AMC_model)
sum_AMC_mod
# Call:
#   lm(formula = AMC$Days.to.Death ~ AMC$Age.at.Histological.Diagnosis)
# 
# Residuals:
#   1      2      3      4      5 
# 87.0 -401.9 -295.4  431.9  178.4 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)                        1252.37     812.58   1.541    0.221
# AMC$Age.at.Histological.Diagnosis   -13.75      13.21  -1.040    0.375
# 
# Residual standard error: 397.8 on 3 degrees of freedom
# Multiple R-squared:  0.2651,	Adjusted R-squared:  0.0201 
# F-statistic: 1.082 on 1 and 3 DF,  p-value: 0.3747

# Derive the estimate and std err
AMC_est <- sum_AMC_mod$coefficients[2, "Estimate"] # get absolute estimate
AMC_std <- sum_AMC_mod$coefficients[2, "Std. Error"] # get std. err.
AMC_rse <- AMC_std / AMC_est # get rse
print(round(AMC_rse,2))

# Regression for R01
R01_model <- lm(R01$Days.to.Death ~ R01$Age.at.Histological.Diagnosis)
sum_R01_mod <- summary(R01_model)
sum_R01_mod
# Call:
#   lm(formula = R01$Days.to.Death ~ R01$Age.at.Histological.Diagnosis)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -761.8 -361.1 -130.1  294.9 1464.7 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)                       1797.568    566.373   3.174  0.00246 **
#   R01$Age.at.Histological.Diagnosis  -15.360      7.999  -1.920  0.06000 . 
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 546.7 on 55 degrees of freedom
# Multiple R-squared:  0.06284,	Adjusted R-squared:  0.0458 
# F-statistic: 3.688 on 1 and 55 DF,  p-value: 0.06

R01_est <- sum_R01_mod$coefficients[2, "Estimate"]
R01_std <- sum_R01_mod$coefficients[2, "Std. Error"]
R01_rse <- R01_std / R01_est
print(round(R01_rse,2))


#----Question 4.3----

# AMC cohort
round(confint(AMC_model, level = 0.9),2)

# R01 cohort
round(confint(R01_model, level = 0.9),2)


#----Question 4.4----

# Obtaining variables for standard deviation
a <- 0.1 # type 1 error rate
# AMC cohort
df_AMC <- sum_AMC_mod$df[2]
t_AMC <- qt(1-(a/2), df_AMC)
# R01 cohort
df_R01 <- sum_R01_mod$df[2]
t_R01 <- qt(1-(a/2), df_R01)

# Plot the respective regression models for each cohort
dev.off()
par(mar=c(4,4,4,4), xpd = TRUE)
plot(AMC$Days.to.Death ~ AMC$Age.at.Histological.Diagnosis, data = AMC,
     #xlim = c(0,100), ylim = c(0,1000),
     main = "Regression for AMC cohort",xlab = "Age at histological diagnosis [years]", ylab = "Survival time [days]")
lines(AMC$Age.at.Histological.Diagnosis, predict(AMC_model), col = "black")
lines(AMC$Age.at.Histological.Diagnosis, predict(AMC_model) - (t_AMC * AMC_std), col = "red", lty = 2)
lines(AMC$Age.at.Histological.Diagnosis, predict(AMC_model) + (t_AMC * AMC_std), col = "red", lty = 2)
legend("bottomleft", c("Patients","Regression line","90% confidence bands"), pch=c(21,NA,NA), lty=c(NA,1,2), 
       col=c('black','black','red'), pt.cex = 1, cex = 0.8)

plot(R01$Days.to.Death ~ R01$Age.at.Histological.Diagnosis, data = R01,
     #xlim = c(0,100), ylim = c(0,2500),
     main = "Regression for R01 cohort", xlab = "Age at histological diagnosis [years]", ylab = "Survival time [days]")
lines(R01$Age.at.Histological.Diagnosis, predict(R01_model), col = "black")
lines(R01$Age.at.Histological.Diagnosis, predict(R01_model) - (t_R01 * R01_std), col = "red", lty = 2)
lines(R01$Age.at.Histological.Diagnosis, predict(R01_model) + (t_R01 * R01_std), col = "red", lty = 2)
legend("topright", c("Patients","Regression line","90% confidence bands"), pch=c(21,NA,NA), lty=c(NA,1,2), 
       col=c('black','black','red'), pt.cex = 1, cex = 0.8)

dev.off()

# Expected difference of survival time for a 5 years age difference 

# AMC cohort
AMC_age_co <- coef(AMC_model)["AMC$Age.at.Histological.Diagnosis"]
AMC_age_diff <- AMC_age_co * 5 # 5 more years at histo. diag. reduce days until death by AMC_age_diff
AMC_diff_year <- round(AMC_age_diff / 365,2)
print(AMC_age_diff) # -68.73
print(AMC_diff_year) # -0.19

# R01 cohort
R01_age_co <- coef(R01_model)["R01$Age.at.Histological.Diagnosis"]
R01_age_diff <- R01_age_co * 5
R01_diff_year <- round(R01_age_diff / 365,2)
print(R01_age_diff) # -76.80
print(R01_diff_year) # -0.21


#----Question 4.5----

# Obtain the residuals from the linear regression model of R01 cohort
R01_res <- residuals(R01_model)

# QQ-Plot
dev.off()
qqnorm(R01_res, main = "Normal QQ-Plot: Distribution of the residuals in the R01 cohort",
       xlab = "Theoretical Quantiles", ylab = "Empirical Quantiles")
qqline(R01_res, col = "red", lwd = 1.5)


#----Question 4.6----

# Standardized residuals from the linear regression model of R01 cohort
R01_res_std <- rstandard(R01_model)

# Residuals by fitted values
plot(fitted(R01_model), R01_res, xlab = "Fitted values", ylab = "Residuals", 
     xlim= c(400,1200), ylim = c(-1000, 1600),
     main = "Residuals vs. fitted values for R01 cohort")
abline(0, 0, lty = 2, lwd = 1, col = "red")

# Standardized residuals by fitted values
plot(fitted(R01_model), R01_res_std, xlab = "Fitted values", ylab = "Standardized Residuals", 
     xlim= c(400,1200), ylim = c(-2, 3),
     main = "Standardized Residuals vs. fitted values for R01 cohort")
abline(0, 0, lty = 2, lwd = 1, col = "red")


#----Question 5.1----

# tables (contingency and frequency)
# tables: of chemotherapy by prior radiation
ct_chemrad <- xtabs(~ Chemotherapy + Radiation, data = nsclc)
corrplot(ct_chemrad, method = "color", is.corr = FALSE, 
         title = "Contingency Table: Prior Treamtent", xlab = "Radiation", ylab = "Chemotherapy",
         col = colorRampPalette(c("white", "steelblue"))(100),addCoef.col = "black", cl.pos="n",
         tl.srt = 0, tl.col = "black", mar = c(4,4,4,4))

ft_chemrad <- round(prop.table(ct_chemrad), digits =2)
corrplot(ft_chemrad, method = "color", title = "Frequency Table: Prior Treatment",
         xlab = "Radiation", ylab = "Chemotherapy",
         addCoef.col = "black", tl.srt = 0, tl.col = "black", col.lim = c(0,1), mar = c(4,4,4,4))


# Create a new variable based on prior chemotherapy and prior radiation

Therapy <- ifelse(nsclc$Chemotherap == "Yes" & nsclc$Radiation == "Yes", "CR",
                       ifelse(nsclc$Chemotherap == "Yes" & nsclc$Radiation == "No", "CN",
                              ifelse(nsclc$Chemotherap == "No" & nsclc$Radiation == "Yes", "NR",
                                     ifelse(nsclc$Chemotherap == "No" & nsclc$Radiation == "No", "NN", "OO"))))


# contingency table 
ct_prtr <- xtabs(~ Therapy, data = Therapy)

# addition of the NR therapy option as it has 0 observations
prtr <- data.frame(Therapy = c("NN","CN","CR","OO","NR"), Freq = c(161, 33, 16, 1, 0))

ft_prtr <- round(prop.table(prtr$Freq), digits =2)

color_start <- "lightsteelblue"
color_end <- "steelblue"
colors <- colorRampPalette(c(color_start, color_end))(5)

barplot(prtr$Freq, names.arg = prtr$Therapy, main = "Contingency: Prior Treatment", 
        xlab = "Prior Treatment", ylab = "Count", legend = TRUE, ylim = c(0,200),
        col = colors)#c("yellowgreen", "steelblue", "firebrick", "gold"))
text(x = barplot(prtr$Freq, plot = FALSE), y = prtr$Freq + 5, labels = prtr$Freq, pos = 3)

barplot(ft_prtr, names.arg = prtr$Therapy, main = "Frequency: Prior Treatment", 
        xlab = "Prior Treatment", ylab = "Frequency", legend = TRUE, ylim = c(0,1),
        col = colors)#c("yellowgreen", "steelblue", "firebrick", "gold"))
text(x = barplot(ft_prtr, plot = FALSE), y = ft_prtr + 0.025, labels = ft_prtr, pos = 3)


#----Question 5.4----

# Prior Therapy ~ Days until Death
Therapy <- factor(Therapy, levels = c("NN", "CR", "CN", "OO"))
nsclc$Prior.Therapy <- as.character(Therapy)
PT <- nsclc[!is.na(nsclc$Days.to.Death),]

anova_mod <- aov(Days.to.Death ~ Prior.Therapy, data = PT)
summary(anova_mod)
# Df   Sum Sq Mean Sq F value Pr(>F)  
# Prior.Therapy  2  1903679  951839   3.368 0.0412 *
#   Residuals     59 16674065  282611                 
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
coefficients(anova_mod)
confint(anova_mod)

tukey_mod <- TukeyHSD(anova_mod)
print(tukey_mod)
# Fit: aov(formula = Days.to.Death ~ Prior.Therapy, data = PT)
# 
# $Prior.Therapy
# diff        lwr       upr     p adj
# CR-CN  353.3295  -240.5649 947.22396 0.3320678
# NN-CN -168.3827  -600.2398 263.47446 0.6189920
# NN-CR -521.7122 -1013.8420 -29.58243 0.0353317

print(round(tukey_mod$Prior.Therapy,4))
# diff        lwr      upr  p adj
# CR-CN  353.3295  -240.5649 947.2240 0.3321
# NN-CN -168.3827  -600.2398 263.4745 0.6190
# NN-CR -521.7122 -1013.8420 -29.5824 0.0353


#----Question 5.6----

# Correlation Plot to check: Independence of Observations
dev.off()
cor <- PT
cor$Chemotherapy <- ifelse(cor$Chemotherapy=="Yes",1,0)
cor$Radiation <- ifelse(cor$Radiation=="Yes",1,0)
cor$Recurrence <- ifelse(cor$Recurrence=="yes",1,0)

cor_matrix <- cor(cor[, c("Days.to.Death", "Age.at.Histological.Diagnosis",
                         "Chemotherapy", "Radiation")])
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black", tl.cex = 0.8,
         add = TRUE, number.cex = 0.8)

gender_counts <- table(PT$Gender)
gender_counts["Male"] #53 
gender_counts["Female"] #9

# Histogram to check: Normality of observations within each group
eij = residuals(anova_mod)

hist(eij, freq = FALSE, col = 'steelblue', main="Histogram of Residuals", xlab="Residuals",
     xlim = c(-1500, 2000), ylim = c(0, 0.0010))
lines(density(eij), lwd = 1.5)

# Box Plot to check: constant within-group variance across all groups
boxplot(Days.to.Death ~ Prior.Therapy, data = PT, main = "Box Plot per Therapy Group",
        xlab = "Prior Therapies", ylab = "Survival Time [days]",
        ylim = c(0, 2500))
stats <- boxplot(Days.to.Death ~ Prior.Therapy, data = PT)
medians <- stats$stats[3, ] #777.0 968.5 441.0
iqrs <- diff(stats$stats[2:3, ]) #395.5 184.5 235.5