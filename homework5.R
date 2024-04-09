#	Clear environment.
rm(list = ls())

#	Set the working directory.
setwd("C:/Users/frits/Documents/MLitt/ECON50580 PhD Econometrics 2/PhD_Econometrics2")

#	Load haven package.
library("haven")

#Import packages for creating clustered standard errors.
library("multiwayvcov")
library("lmtest")

#	Load data.
data <- read_dta("data_ps5.dta")

#	Defining a function for clustered standard errors.
clustered.summary <- function(reg, clusters) {
  vcov <- cluster.vcov(get(reg), eval(getCall(get(reg))$data)[[clusters]])
  reg_summary <- summary(get(reg))
  reg_coeftest <- coeftest(get(reg), vcov)
  reg_summary$coefficients <- reg_coeftest
  return(reg_summary)
}

#	Question 2.1
reg.1.lnavwage <- lm(ln_avwage ~ as.factor(lowwage) + as.factor(post) + as.factor(lowwage) * as.factor(post), data = data)
reg.1.netpcm <- lm(net_pcm ~ as.factor(lowwage) + as.factor(post) + as.factor(lowwage) * as.factor(post), data = data)

#	Question 2.2
reg.2.lnavwage.summary <- clustered.summary("reg.1.lnavwage", "regno")
reg.2.netpcm.summary <- clustered.summary("reg.1.netpcm", "regno")

#	Question 2.3
reg.3.lnavwage <- lm(ln_avwage ~ as.factor(regno) + as.factor(year) + as.factor(lowwage) * as.factor(post), data = data)
reg.3.lnavwage.summary <- clustered.summary("reg.3.lnavwage", "regno")

reg.3.netpcm <- lm(net_pcm ~ as.factor(regno) + as.factor(year) + as.factor(lowwage) * as.factor(post), data = data)
reg.3.netpcm.summary <- clustered.summary("reg.3.netpcm", "regno")

#	Question 2.4
reg.4.lnavwage.industrytrend <- lm(ln_avwage ~ as.factor(regno) + year * as.factor(sic2) + as.factor(lowwage) * as.factor(post), data = data)
reg.4.lnavwage.industrytrend.summary <- clustered.summary("reg.4.lnavwage.industrytrend", "regno")

reg.4.netpcm.industrytrend <- lm(net_pcm ~ as.factor(regno) + year * as.factor(sic2) + as.factor(lowwage) * as.factor(post), data = data)
reg.4.netpcm.industrytrend.summary <- clustered.summary("reg.4.netpcm.industrytrend", "regno")

reg.4.lnavwage.firmtrend <- lm(ln_avwage ~ as.factor(regno) + year * as.factor(regno) + as.factor(lowwage) * as.factor(post), data = data)
reg.4.lnavwage.firmtrend.summary <- clustered.summary("reg.4.lnavwage.firmtrend", "regno")

reg.4.netpcm.firmtrend <- lm(net_pcm ~ as.factor(regno) + year * as.factor(regno) + as.factor(lowwage) * as.factor(post), data = data)
reg.4.netpcm.firmtrend.summary <- clustered.summary("reg.4.netpcm.firmtrend", "regno")
