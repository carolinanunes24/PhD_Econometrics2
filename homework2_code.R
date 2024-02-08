#	Clear environment.
rm(list = ls())

#	Set seed.
set.seed(231)

#	Load required packages.
library("MASS")

#	Question 2.
#	Setting parameters.
gamma0 <- 0
gamma1 <- 0.25
alpha <- 0
beta <- 0.75
mean.vector.xez <- c(3, 0, 2)

#	Sub-question A.
varcov.matrix.xez <- rbind(c(1, 0.4, 0.5), c(0.4, 1, 0), c(0.5, 0, 1))
colour <- 1
for (n in c(50, 100, 250, 1000)) {
	beta.ols <- c()
	beta.iv <- c()
	cor.zx <- c()
	cor.xe <- c()
	cor.ze <- c()
	for (i in 1:10000){
		df <- mvrnorm(n, mean.vector.xez, varcov.matrix.xez)
		x <- df[,1]
		epsilon <- df[,2]
		z <- df[,3]
		y <- alpha + beta * x + epsilon
		u <- x - gamma0 + gamma1 * z
		cor.zx <- append(cor.zx, cor(z, x, method = "pearson"))
		cor.xe <- append(cor.xe, cor(x, epsilon, method = "pearson"))
		cor.ze <- append(cor.ze, cor(z, epsilon, method = "pearson"))
		ols <- lm(y ~ x)
		first.stage <- lm(x ~ z)
		x.fitted <- first.stage$fitted.values
		second.stage <- lm(y ~ x.fitted)
		beta.ols <- append(beta.ols, ols$coefficients[2])
		beta.iv <- append(beta.iv, second.stage$coefficients[2])
	}
	if (colour == 1){
		plot(density(beta.ols), xlim = c(0, 2), ylim = c(0, 15), 
		xlab = "", col = colour, 
		main = bquote("Distributions of" ~ beta[OLS] ~ "and" ~ beta[IV]))
		lines(density(beta.iv), col = colour + 1)
	} else {
		lines(density(beta.ols), col = colour)
		lines(density(beta.iv), col = colour + 1)
	}
	colour <- colour + 2
}
legend("topright",
	 sapply(c(bquote(beta[OLS]~","~N==50), bquote(beta[IV]~","~N==50),
		    bquote(beta[OLS]~","~N==100), bquote(beta[IV]~","~N==100),
		    bquote(beta[OLS]~","~N==250), bquote(beta[IV]~","~N==250),
		    bquote(beta[OLS]~","~N==1000), bquote(beta[IV]~","~N==1000)), 
	 as.expression),
	 col = 1:8, lty = 1)

#	Sub-question B.
varcov.matrix.xez <- rbind(c(1, 0.4, 0.15), c(0.4, 1, 0), c(0.15, 0, 1))
colour <- 1
for (n in c(50, 100, 250, 1000)) {
	beta.ols <- c()
	beta.iv <- c()
	cor.zx <- c()
	cor.xe <- c()
	cor.ze <- c()
	for (i in 1:10000){
		df <- mvrnorm(n, mean.vector.xez, varcov.matrix.xez)
		x <- df[,1]
		epsilon <- df[,2]
		z <- df[,3]
		y <- alpha + beta * x + epsilon
		u <- x - gamma0 + gamma1 * z
		cor.zx <- append(cor.zx, cor(z, x, method = "pearson"))
		cor.xe <- append(cor.xe, cor(x, epsilon, method = "pearson"))
		cor.ze <- append(cor.ze, cor(z, epsilon, method = "pearson"))
		ols <- lm(y ~ x)
		first.stage <- lm(x ~ z)
		x.fitted <- first.stage$fitted.values
		second.stage <- lm(y ~ x.fitted)
		beta.ols <- append(beta.ols, ols$coefficients[2])
		beta.iv <- append(beta.iv, second.stage$coefficients[2])
	}
	if (colour == 1){
		plot(density(beta.ols), xlim = c(-1, 2), ylim = c(0, 15), 
		xlab = "", col = colour, 
		main = bquote("Distributions of" ~ beta[OLS] ~ "and" ~ beta[IV]))
		lines(density(beta.iv, n = 1e+6), col = colour + 1)
	} else {
		lines(density(beta.ols), col = colour)
		lines(density(beta.iv, n = 1e+6), col = colour + 1)
	}
	colour <- colour + 2
}
legend("topleft",
	 sapply(c(bquote(beta[OLS]~","~N==50), bquote(beta[IV]~","~N==50),
		    bquote(beta[OLS]~","~N==100), bquote(beta[IV]~","~N==100),
		    bquote(beta[OLS]~","~N==250), bquote(beta[IV]~","~N==250),
		    bquote(beta[OLS]~","~N==1000), bquote(beta[IV]~","~N==1000)), 
	 as.expression),
	 col = 1:8, lty = 1)

########################################################################

#	Clear environment.
rm(list = ls())

#	Load required packages.
library("haven")

#	Set the working directory.
setwd("C:/Users/frits/Documents/MLitt/ECON50580 PhD Econometrics 2/PhD_Econometrics2")

#	Import assign2.dta .
data <- read_dta("assign2.dta")

#	Question 3.

#	Sub-question A.
reg <- lm(schooling ~ logearn + age + I(age^2) + I(age^3) + I(age^4) + yob + I(yob^2) + I(yob^3) + I(yob^4), data)
reg <- summary(reg)