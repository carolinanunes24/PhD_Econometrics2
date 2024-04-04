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


#	Defining a function to calculate the Moulton factor.
moulton <- function(reg, data, cluster, var)
{
	data$res <- residuals(reg)

	if (length(var) > 1)
	{
		data$interaction <- data[[var[1]]] * data[[var[2]]]
		anova_var <- anova(lm(data$interaction ~ data[[cluster]]))
	} else {
		anova_var <- anova(lm(data[[var]] ~ data[[cluster]]))
	}

	MSb_var <- anova_var$'Mean Sq'[1]
	MSw_var <- anova_var$'Mean Sq'[2]

	anova_res <- anova(lm(data$res ~ data[[cluster]]))
	MSb_res <- anova_res$'Mean Sq'[1]
	MSw_res <- anova_res$'Mean Sq'[2]

	clusters <- as.numeric(levels(factor(data[[cluster]])))
	nclusters <- sapply(clusters, function(i) nrow(data[data[[cluster]] == i, ]))
	n <- mean(nclusters)

	rho_var <- (MSb_var - MSw_var) / (MSb_var + (n - 1) * MSw_var)
	rho_res <- (MSb_res - MSw_res) / (MSb_res + (n - 1) * MSw_res)

	Moulton <- sqrt(1 + (var(nclusters) / n + pi - 1) * rho_var * rho_res)
	return(Moulton)
}


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

reg.1.lnavwage.se <- sqrt(sd(data[data$lowwage == 1 & data$post == 1,]$ln_avwage)^2/length(data[data$lowwage == 1 & data$post == 1,]$ln_avwage) + sd(data[data$lowwage == 1 & data$post == 0,]$ln_avwage)^2/length(data[data$lowwage == 1 & data$post == 0,]$ln_avwage))
reg.1.netpcm.se <- sqrt(sd(data[data$lowwage == 1 & data$post == 1,]$net_pcm)^2/length(data[data$lowwage == 1 & data$post == 1,]$net_pcm) + sd(data[data$lowwage == 1 & data$post == 0,]$net_pcm)^2/length(data[data$lowwage == 1 & data$post == 0,]$net_pcm))


#	Question 2.2
reg.1.lnavwage.moulton <- c()
reg.1.netpcm.moulton <- c()

for (i in c("year", "sic2", "regno"))
{
	reg.1.lnavwage.moulton <- append(reg.1.lnavwage.moulton,
							sapply(list("lowwage", "post", c("lowwage", "post")), function(j) moulton(reg.1.lnavwage, data, i, j)))
}

for (i in c("year", "sic2", "regno"))
{
	reg.1.netpcm.moulton <- append(reg.1.netpcm.moulton,
							sapply(list("lowwage", "post", c("lowwage", "post")), function(j) moulton(reg.1.netpcm, data, i, j)))
}

reg.1.lnavwage.moulton <- matrix(reg.1.lnavwage.moulton, nrow = 3, byrow = TRUE)
reg.1.netpcm.moulton <- matrix(reg.1.netpcm.moulton, nrow = 3, byrow = TRUE)

rownames(reg.1.lnavwage.moulton) <- c("year", "sic2", "regno")
rownames(reg.1.netpcm.moulton) <- c("year", "sic2", "regno")

colnames(reg.1.lnavwage.moulton) <- c("lowwage", "post", "interaction")
colnames(reg.1.netpcm.moulton) <- c("lowwage", "post", "interaction")

reg.1.lnavwage.summary <- clustered.summary("reg.1.lnavwage", "sic2")
reg.1.netpcm.summary <- clustered.summary("reg.1.netpcm", "sic2")


#	Question 2.3
reg.3.lnavwage <- lm(ln_avwage ~ as.factor(regno) + as.factor(year) + as.factor(lowwage) * as.factor(post), data = data)
reg.3.lnavwage.summary <- clustered.summary("reg.3.lnavwage", "regno")

reg.3.netpcm <- lm(net_pcm ~ as.factor(regno) + as.factor(year) + as.factor(lowwage) * as.factor(post), data = data)
reg.3.netpcm.summary <- clustered.summary("reg.3.netpcm", "regno")


#	Question 2.4
reg.4.lnavwage.industrytrend <- lm(ln_avwage ~ as.factor(regno) + as.factor(year) * as.factor(sic2) + as.factor(lowwage) * as.factor(post), data = data)
reg.4.lnavwage.industrytrend.summary <- clustered.summary("reg.4.lnavwage.industrytrend", "regno")

reg.4.netpcm.industrytrend <- lm(net_pcm ~ as.factor(regno) + as.factor(year) * as.factor(sic2) + as.factor(lowwage) * as.factor(post), data = data)
reg.4.netpcm.industrytrend.summary <- clustered.summary("reg.4.netpcm.industrytrend", "regno")

reg.4.lnavwage.firmtrend <- lm(ln_avwage ~ as.factor(regno) + as.factor(year) * as.factor(regno) + as.factor(lowwage) * as.factor(post), data = data)
reg.4.lnavwage.firmtrend.summary <- clustered.summary("reg.4.lnavwage.firmtrend", "regno")

reg.4.netpcm.firmtrend <- lm(net_pcm ~ as.factor(regno) + as.factor(year) * as.factor(regno) + as.factor(lowwage) * as.factor(post), data = data)
reg.4.netpcm.firmtrend.summary <- clustered.summary("reg.4.netpcm.firmtrend", "regno")
