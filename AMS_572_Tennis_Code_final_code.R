#install.packages("corrplot")
#install.packages("magrittr") 
#install.packages("dplyr")
#install.packages("readxl")
#install.packages("tidyverse")
#install.packages("cowplot")
#install.packages("missMethods")
#install.packages("mice")

library("corrplot")
library("dplyr") 
library("magrittr")
library("readxl")
library("tidyverse")
library("ggplot2")
library("cowplot")
library("missMethods")
library("mice")

og_file <- read_excel("/Users/taejinpark/Desktop/SBU 2023 Fall/AMS 572/AMS572 Group Project/AMS_572_Tennis.xlsx")

# fix the blank values
og_file["NPW"][is.na(og_file["NPW"])] <- 0
og_file["NPA"][is.na(og_file["NPA"])] <- 0
og_file["DBF"][is.na(og_file["DBF"])] <- 0


fix_factor <- 1:3
new_file <- og_file %>% mutate_at(vars(fix_factor), as.factor)

fix_column <- 4:21
final <- new_file %>% mutate_at(vars(fix_column), as.numeric)

########################### MCAR Missing Value, Multicollinearity & Logistic regression #############################
# create missing values
ds_miss <- delete_MCAR(final, 0.2, cols_mis = c("WNR", "NPW"))
str(ds_miss)

ds_num <- ds_miss%>%
  select("WNR", "NPW", "FSW", "FSP", "NPA")

md.pattern(ds_num)

# check distribution of the variable

value_imputed <- data.frame(
  original = ds_miss$WNR,
  imputed_zero = replace(ds_miss$WNR, is.na(ds_miss$WNR), 0),
  imputed_mean = replace(ds_miss$WNR, is.na(ds_miss$WNR),
                         mean(ds_miss$WNR, na.rm = TRUE)),
  imputed_median = replace(ds_miss$WNR, is.na(ds_miss$WNR),
                           median(ds_miss$WNR, na.rm = TRUE)))

value_imputed

# graph the dist. and compare
f1 <- ggplot(value_imputed, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution of WNR") +
  theme_classic()
f2 <- ggplot(value_imputed, aes(x = imputed_zero)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("Zero-imputed distribution") +
  theme_classic()
f3 <- ggplot(value_imputed, aes(x = imputed_mean)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("Mean-imputed distribution") +
  theme_classic()
f4 <- ggplot(value_imputed, aes(x = imputed_median)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Median-imputed distribution") +
  theme_classic()

plot_grid(f1, f2, f3, f4, nrow = 2, ncol = 2)

# now try mice
mice_imputed <- data.frame(
  original = ds_num$WNR,
  imputed_pmm = complete(mice(ds_num, method = "pmm"))$WNR,
  imputed_cart = complete(mice(ds_num, method = "cart"))$WNR,
  imputed_norm = complete(mice(ds_num, method = "norm"))$WNR)

mice_imputed

h1 <- ggplot(mice_imputed, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution of WNR") +
  theme_classic()
h2 <- ggplot(mice_imputed, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("pmm distribution") +
  theme_classic()
h3 <- ggplot(mice_imputed, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("cart distribution") +
  theme_classic()
h4 <- ggplot(mice_imputed, aes(x = imputed_norm)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("norm distribution") +
  theme_classic()

plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)


# The cart method looks the closest to the og data

imputed_cart = complete(mice(ds_num, method = "cart"))$WNR

new_wnr_values <- c(imputed_cart)

ds_miss$WNR <- new_wnr_values

View(ds_miss)
#Saving for future use in Logit Model
ds_miss_logit = ds_miss

# We have finished imputing the vals for WNR column

# Start imputing the vals for NPW column

mice_imputed2 <- data.frame(
  original2 = ds_num$NPW,
  imputed_pmm2 = complete(mice(ds_num, method = "pmm"))$NPW,
  imputed_cart2 = complete(mice(ds_num, method = "cart"))$NPW,
  imputed_norm2 = complete(mice(ds_num, method = "norm"))$NPW)

mice_imputed2

l1 <- ggplot(mice_imputed2, aes(x = original2)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution for NPW") +
  theme_classic()
l2 <- ggplot(mice_imputed2, aes(x = imputed_pmm2)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("Pmm distribution") +
  theme_classic()
l3 <- ggplot(mice_imputed2, aes(x = imputed_cart2)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("Cart distribution") +
  theme_classic()
l4 <- ggplot(mice_imputed2, aes(x = imputed_norm2)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Norm distribution") +
  theme_classic()

plot_grid(l1, l2, l3, l4, nrow = 2, ncol = 2)

# cart method looks closest again .. 

imputed_cart2 = complete(mice(ds_num, method = "cart"))$NPW

new_npw_values <- c(imputed_cart2)

ds_miss$NPW <- new_npw_values

ds_complete <- ds_miss

#checking correlations for multicollinearity test using original data
final_no_na = ds_complete
final_no_na[is.na(final_no_na)] <- 0
temp_fix_column <- 1:3
final_no_na <- final_no_na %>% mutate_at(vars(temp_fix_column), as.numeric)
#str(final_no_na)
corr_mat <- round(cor(final_no_na,method = "spearman"),2)
print(corr_mat)
#corr_mat[corr_mat < 0.5 & corr_mat > -0.5] <- 0 
#print(corr_mat)
corrplot(corr_mat, method="color")


# Logit Models for MCAR data imputation (before and after models)
ds_miss_logit = as.data.frame(ds_miss_logit)
ds_miss_logit[is.na(ds_miss_logit)] <- 0
model_missing_logit <- glm(Result ~ FSP + FSW + SSW + ACE + DBF + WNR + UFE + BPC + BPW + NPA + NPW + TPW, data = ds_miss_logit, family = "binomial")
#model_missing_logit <- glm(Result ~ FSP + FSW + SSW + DBF + WNR + NPW + BPW + ACE, data = ds_miss_logit, family = "binomial")
summary(model_missing_logit)
1-pchisq(349.35-162.48, 251-239)

ds_complete_logit = as.data.frame(ds_complete)
ds_complete_logit[is.na(ds_complete_logit)] <- 0
model_complete_logit <- glm(Result ~ FSP + FSW + SSW + ACE + DBF + WNR + UFE + BPC + BPW + NPA + NPW + TPW, data = ds_complete_logit, family = "binomial")
summary(model_complete_logit)
1-pchisq(349.35-150.79, 251-239)

########################### MNAR Missing Value & Logistic regression #############################
# create missing values

ds_miss_MNAR <- delete_MNAR_censoring(final, 0.2, cols_mis = c("WNR", "NPW"))
str(ds_miss_MNAR)

ds_num_MNAR <- ds_miss_MNAR%>%
  select("WNR", "NPW", "FSW", "FSP", "NPA")

md.pattern(ds_num_MNAR)

mice_imputed_MNAR <- data.frame(
  original = ds_num_MNAR$WNR,
  imputed_pmm = complete(mice(ds_num_MNAR, method = "pmm"))$WNR,
  imputed_cart = complete(mice(ds_num_MNAR, method = "cart"))$WNR,
  imputed_rf = complete(mice(ds_num_MNAR, method = "rf"))$WNR)

mice_imputed_MNAR

h1 <- ggplot(mice_imputed_MNAR, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution") +
  theme_classic()
h2 <- ggplot(mice_imputed_MNAR, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("pmm distribution") +
  theme_classic()
h3 <- ggplot(mice_imputed_MNAR, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("cart distribution") +
  theme_classic()
h4 <- ggplot(mice_imputed_MNAR, aes(x = imputed_rf)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("rf") +
  theme_classic()

plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)


# The pmm method seems to be closest to the original data set

imputed_pmm_MNAR <- complete(mice(ds_num_MNAR, method = "pmm"))$WNR

new_wnr_values_MNAR <- c(imputed_pmm_MNAR)

ds_miss_MNAR$WNR <- new_wnr_values_MNAR

View(ds_miss_MNAR)
#Saving for future use in Logit Model
ds_miss_logit_MNAR = ds_miss_MNAR

# We have finished imputing the vals for WNR column

# Start imputing the vals for NPW column

mice_imputed2_MNAR <- data.frame(
  original2 = ds_num_MNAR$NPW,
  imputed_pmm2 = complete(mice(ds_num_MNAR, method = "pmm"))$NPW,
  imputed_cart2 = complete(mice(ds_num_MNAR, method = "cart"))$NPW,
  imputed_rf2 = complete(mice(ds_num_MNAR, method = "rf"))$NPW)

mice_imputed2_MNAR

l1 <- ggplot(mice_imputed2_MNAR, aes(x = original2)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution") +
  theme_classic()
l2 <- ggplot(mice_imputed2_MNAR, aes(x = imputed_pmm2)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("Pmm distribution") +
  theme_classic()
l3 <- ggplot(mice_imputed2_MNAR, aes(x = imputed_cart2)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("Cart distribution") +
  theme_classic()
l4 <- ggplot(mice_imputed2_MNAR, aes(x = imputed_rf2)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("rf") +
  theme_classic()

plot_grid(l1, l2, l3, l4, nrow = 2, ncol = 2)

# All distributions are biased, but pmm seems to be the closest?

imputed_pmm2_MNAR <- complete(mice(ds_num_MNAR, method = "pmm"))$NPW

new_npw_values_MNAR <- c(imputed_pmm2_MNAR)

ds_miss_MNAR$NPW <- new_npw_values_MNAR

ds_complete_MNAR <- ds_miss_MNAR

# we have successfully imputed the values for WNR and NPW

# Logit Models for MCAR data imputation (before and after models)
ds_miss_logit_MNAR = as.data.frame(ds_miss_logit_MNAR)
ds_miss_logit_MNAR[is.na(ds_miss_logit_MNAR)] <- 0
model_missing_logit_MNAR <- glm(Result ~ FSP + FSW + SSW + ACE + DBF + WNR + UFE + BPC + BPW + NPA + NPW + TPW, data = ds_miss_logit_MNAR, family = "binomial")
summary(model_missing_logit_MNAR)
1-pchisq(349.35-147.86, 251-239)

ds_complete_logit_MNAR = as.data.frame(ds_complete_MNAR)
ds_complete_logit_MNAR[is.na(ds_complete_logit_MNAR)] <- 0
model_complete_logit_MNAR <- glm(Result ~ FSP + FSW + SSW + ACE + DBF + WNR + UFE + BPC + BPW + NPA + NPW + TPW, data = ds_complete_logit_MNAR, family = "binomial")
summary(model_complete_logit_MNAR)
1-pchisq(349.35-144.64, 251-239)



# Correlation Hypothesis test with data missing using MCAR. 20% missingness --------------------------

# Check Q-Q Plots for the variables of interest.
qqnorm(ds_miss$NPW, main = "Normal Q-Q Plot for Net Points Won")
qqline(ds_miss$NPW)
qqnorm(ds_miss$WNR, main = "Normal Q-Q Plot for Winners Earned")
qqline(ds_miss$WNR)
# Judging by the QQ plots alone, one might think that the data for these variables appear linear, which
# would indicate normality, especially for the QQ plot for winners earned by players. However,
# doing the Shapiro-Wilk normality test shows that we can assume the data for these variables
# is assumed not normal.
shapiro.test(ds_miss$NPW) # p-value = 1.509e-07. Thus, the data is assumed not normal.
shapiro.test(ds_miss$WNR) # p-value = 0.0002459. Thus, the data is assumed not normal.
# r = 0.687, but since the data is not normal, it is recommended to use spearman. However, Spearman and Kendall is usually used
# for ordinal data, so this may not be its best use.
p1_corrCoeff_MCAR_20 = cor(ds_miss$NPW,ds_miss$WNR, use = 'complete.obs')
# r = 0.759 for Spearman, does not rely on normality, we will use this one.
# To improve the accuracy of the correlation when we use Spearman or Kendall methods, we could transform
# the data to be ranked (ordinal), in addition to the data being assumed not normal.
p1_corrCoeff_MCAR_20_spearman = cor(ds_miss$NPW,ds_miss$WNR, use = 'complete.obs', method = 'spearman')
# r = 0.5755 for Kendall. This is more a statistic of dependence between two variables than correlation.
p1_corrCoeff_MCAR_20_kendall = cor(ds_miss$NPW,ds_miss$WNR, use = 'complete.obs', method = 'kendall')
# Plot Net Points Won r.v. vs Winners Earned r.v.
plot(ds_miss$NPW,ds_miss$WNR, xlab = "Net Points Won", ylab = "Winners Earned", main = "Net Points Won vs Winners Earned Match Stats")

# H0: Tau = 0, H1: Tau =/ 0
# This test is valid when the data follows a normal distribution, is ratio or interval data (our case is ratio),
# limited outliers, and approximately linear data. Spearman and Kendall don't rely on the normality assumption.
cor.test(ds_miss$NPW,ds_miss$WNR, method = "kendall")
# sample correlation coefficient is r = 0.5755, p-value of test < 2.2e-16. It's better to use
# Kendall when population is small, and when there are many tied ranks from Spearman.

# H0: Rho = 0, H1: Rho =/ 0
cor.test(ds_miss$NPW,ds_miss$WNR, method = "spearman")
# The population correlation coefficient is the correlation coefficient for the 4 different men's singles 2014 tournaments
# since this test only considers the players in one of the 4 tournaments.
# the sample correlation coefficient is r = 0.759.
# The test statistic is S = 167824 and the corresponding p-value < 2.2e-16
# Since the p-value is < 0.01, reject H0 and we have sufficient evidence to say that the correlation between the net 
# points won by players and the winners earned by players is
# statistically significant (different from 0 in the population).
# Used the Spearman correlation test since the data is assumed not normal.



# Correlation Hypothesis test using imputed values for MCAR data. 20% missingness in the data.----------

# Check Q-Q Plots for the variables of interest.
qqnorm(ds_complete$NPW, main = "Normal Q-Q Plot for Net Points Won")
qqline(ds_complete$NPW)
qqnorm(ds_complete$WNR, main = "Normal Q-Q Plot for Winners Earned")
qqline(ds_complete$WNR)
# Judging by the QQ plots alone, one might think that the data for these variables appear linear, which
# would indicate normality, especially for the QQ plot for winners earned by players. However,
# doing the Shapiro-Wilk normality test shows that we can assume the data for these variables
# is assumed not normal.
shapiro.test(ds_complete$NPW) # p-value = 2.3e-08. Thus, the data is assumed not normal.
shapiro.test(ds_complete$WNR) # p-value = 0.0001198. Thus, the data is assumed not normal.
# r = 0.693
p1_corrCoeff_MCAR_20_imputed = cor(ds_complete$NPW,ds_complete$WNR)
# r = 0.737
p1_corrCoeff_MCAR_20_imputed_spear = cor(ds_complete$NPW,ds_complete$WNR, method = 'spearman')
# r = 0.559
p1_corrCoeff_MCAR_20_imputed_kend = cor(ds_complete$NPW,ds_complete$WNR, method = 'kendall')
# Plot Net Points Won r.v. vs Winners Earned r.v.
plot(ds_complete$NPW,ds_complete$WNR, xlab = "Net Points Won", ylab = "Winners Earned", main = "Net Points Won vs Winners Earned Match Stats")

# H0: Tau = 0, H1: Tau =/ 0
cor.test(ds_complete$NPW,ds_complete$WNR, method = "kendall")
# sample correlation coefficient is r = 0.559, z = 12.833, p-value of test < 2.2e-16. It's better to use
# Kendall when population is small, and when there are many tied ranks from Spearman.

# H0: Rho = 0, H1: Rho =/ 0
cor.test(ds_complete$NPW,ds_complete$WNR, method = "spearman")
# The population correlation coefficient is the correlation coefficient for the 4 different men's singles 2014 tournaments
# since this test only considers the players in one of the 4 tournaments.
# the sample correlation coefficient is r = 0.7372.
# The test statistic is S = 700946 and the corresponding p-value < 2.2e-16
# Since the p-value is < 0.01, reject H0 and we have sufficient evidence to say that the correlation between the net 
# points won by players and the winners earned by players is
# statistically significant (different from 0 in the population).
# Used the Spearman correlation test since the assumption is the data is not normal.



# Correlation Hypothesis test with missing data using MNAR. 20% missingness ------------

# Check Q-Q Plots for the variables of interest.
qqnorm(ds_miss_MNAR$NPW, main = "Normal Q-Q Plot for Net Points Won")
qqline(ds_miss_MNAR$NPW)
qqnorm(ds_miss_MNAR$WNR, main = "Normal Q-Q Plot for Winners Earned")
qqline(ds_miss_MNAR$WNR)
# Judging by the QQ plots alone, one might think that the data for these variables appear linear, which
# would indicate normality, especially for the QQ plot for winners earned by players. However,
# doing the Shapiro-Wilk normality test shows that we can assume the data for these variables
# is assumed not normal.
shapiro.test(ds_miss_MNAR$NPW) # p-value = 2.627e-08. Thus, the data is assumed not normal.
shapiro.test(ds_miss_MNAR$WNR) # p-value = 5.166e-08. Thus, the data is assumed not normal.
# r = 0.453, but since the data is not normal, it is recommended to use spearman. However, Spearman and Kendall is best used
# for ordinal data, so this may not be its best use.
p1_corrCoeff_MNAR_20 = cor(ds_miss_MNAR$NPW,ds_miss_MNAR$WNR, use = 'complete.obs')
# r = 0.471 for Spearman, does not rely on normality, we will use this one.
# To improve the accuracy of the correlation when we use Spearman or Kendall methods, we could transform
# the data to be ranked (ordinal), in addition to the data being assumed not normal.
p1_corrCoeff_MNAR_20_spearman = cor(ds_miss_MNAR$NPW,ds_miss_MNAR$WNR, use = 'complete.obs', method = 'spearman')
# r = 0.3325 for Kendall. This is more a statistic of dependence between two variables than correlation.
p1_corrCoeff_MNAR_20_kendall = cor(ds_miss_MNAR$NPW,ds_miss_MNAR$WNR, use = 'complete.obs', method = 'kendall')
# Plot Net Points Won r.v. vs Winners Earned r.v.
plot(ds_miss_MNAR$NPW,ds_miss_MNAR$WNR, xlab = "Net Points Won", ylab = "Winners Earned", main = "Net Points Won vs Winners Earned Match Stats")

# H0: Tau = 0, H1: Tau =/ 0
# This test is valid when the data follows a normal distribution, is ratio or interval data (our case is ratio),
# limited outliers, and approximately linear data. Spearman and Kendall don't rely on the normality assumption.
cor.test(ds_miss_MNAR$NPW,ds_miss_MNAR$WNR, method = "kendall")
# sample correlation coefficient is r = 0.3325, p-value of test = 1.421e-11. It's better to use
# Kendall when population is small, and when there are many tied ranks from Spearman.

# H0: Rho = 0, H1: Rho =/ 0
cor.test(ds_miss_MNAR$NPW,ds_miss_MNAR$WNR, method = "spearman")
# The population correlation coefficient is the correlation coefficient for the 4 different men's singles 2014 tournaments
# since this test only considers the players in one of the 4 tournaments.
# the sample correlation coefficient is r = 0.471.
# The test statistic is S = 653437 and the corresponding p-value = 3.581e-12
# Since the p-value is < 0.01, we reject H0 at the 0.01 significance level and have sufficient evidence to say that the correlation between the net 
# points won by players and the winners earned by players is
# statistically significant (different from 0 in the population).
# Used the Spearman correlation test since the data is assumed not normal.



# Correlation Hypothesis test using imputed values for MNAR data. 20% missingness in the data.------------------

# Check Q-Q Plots for the variables of interest.
qqnorm(ds_complete_MNAR$NPW, main = "Normal Q-Q Plot for Net Points Won")
qqline(ds_complete_MNAR$NPW)
qqnorm(ds_complete_MNAR$WNR, main = "Normal Q-Q Plot for Winners Earned")
qqline(ds_complete_MNAR$WNR)
# Judging by the QQ plots alone, one might think that the data for these variables appear linear, which
# would indicate normality, especially for the QQ plot for winners earned by players. However,
# doing the Shapiro-Wilk normality test shows that we can assume the data for these variables
# is assumed not normal.
shapiro.test(ds_complete_MNAR$NPW) # p-value = 1.295e-12. Thus, the data is assumed not normal.
shapiro.test(ds_complete_MNAR$WNR) # p-value = 2.551e-10. Thus, the data is assumed not normal.
# r = 0.498
p2_corrCoeff_MNAR_20_imputed = cor(ds_complete_MNAR$NPW,ds_complete_MNAR$WNR)
# r = 0.490
p2_corrCoeff_MNAR_20_imputed_spear = cor(ds_complete_MNAR$NPW,ds_complete_MNAR$WNR, method = 'spearman')
# r = 0.3446
p2_corrCoeff_MNAR_20_imputed_kend = cor(ds_complete_MNAR$NPW,ds_complete_MNAR$WNR, method = 'kendall')
# Plot Net Points Won r.v. vs Winners Earned r.v.
plot(ds_complete_MNAR$NPW,ds_complete_MNAR$WNR, xlab = "Net Points Won", ylab = "Winners Earned", main = "Net Points Won vs Winners Earned Match Stats")

# H0: Tau = 0, H1: Tau =/ 0
cor.test(ds_complete_MNAR$NPW,ds_complete_MNAR$WNR, method = "kendall")
# sample correlation coefficient is r = 0.3446, z = 7.9146, p-value of test = 2.481e-15. It's better to use
# Kendall when population is small, and when there are many tied ranks from Spearman.
# H0: Rho = 0, H1: Rho =/ 0
cor.test(ds_complete_MNAR$NPW,ds_complete_MNAR$WNR, method = "spearman")
# The population correlation coefficient is the correlation coefficient for the 4 different men's singles 2014 tournaments
# since this test only considers the players in one of the 4 tournaments.
# the sample correlation coefficient is r = 0.4903.
# The test statistic is S = 1359419 and the corresponding p-value < 2.2e-16
# Since the p-value is < 0.01, reject H0 and we have sufficient evidence to say that the correlation between the net 
# points won by players and the winners earned by players is
# statistically significant.
# Used the Spearman correlation test since the assumption is the data is not normal.
