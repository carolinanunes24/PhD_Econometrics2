#	Clear environment.
rm(list = ls())

#	Set seed.
set.seed(231)


correlate <- function(y, rho) {
	x <- rnorm(length(y))
	y.perp <- residuals(lm(x ~ y))
	rho * sd(y.perp) * y + y.perp * sd(y) * sqrt(1 - rho^2)
}

for (n in c(50, 100, 250, 1000)) {
	cor.zx <- c()
	cor.xe <- c()
	cor.ze <- c()

	beta.ols <- c()
	beta.iv <- c()
	for (i in 1:10000){
		gamma0 <- 0
		gamma1 <- 0.25
		alpha <- 0
		beta <- 1
		epsilon <- rnorm(n)
		x <- correlate(epsilon, 0.4)
		z <- correlate(x, 0.5)
		y <- alpha + beta * x + epsilon
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
	hist(beta.ols, main = bquote("Distribution of" ~ beta[OLS] ~ "for" ~ N == .(n)), xlab = "")
	hist(beta.iv, main = bquote("Distribution of" ~ beta[IV] ~ "for" ~ N == .(n)), xlab = "")
}
