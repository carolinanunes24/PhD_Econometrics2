#	Set seed for reproducability.
set.seed(54)


#	Parameters for question 2.
n <- 100000
pi.0 <- 0
pi.1 <- 0.25
pi.2 <- 1
beta.0 <- 1
beta.1 <- 1
theta <- 0.05


#	Question 2.1
X.i <- rnorm(n, 0, 1)
Z.star.i <- rnorm(n, 0, 1)
epsilon.i <- rnorm(n, 0, 0.5)
U.D <- runif(n)

U.0i <- 1.5 * (U.D - 0.5) - U.D^2 + epsilon.i
U.1i <- -0.5 * (U.D - 0.5) + U.D^2 + epsilon.i

v.i <- min(U.D) + (max(U.D) - min(U.D)) * U.D
D.star.i <- pi.0 + pi.1 * Z.star.i + pi.2 * X.i + v.i

Y.0i <- U.0i + beta.0 * X.i + theta * X.i^2
Y.1i <- U.1i + beta.1 * X.i + theta * X.i^2

D.i <- rep(0, n)
D.i[D.star.i >= 0] <- 1
Y.i <- D.i * Y.1i + (1 - D.i) * Y.0i

first.stage <- glm(D.i ~ Z.star.i + X.i, family = binomial(link = "probit"))
propensity.score <- first.stage$fitted

treated.propensity.score <- propensity.score[D.i == 1]
untreated.propensity.score <- propensity.score[D.i == 0]
histogram.treated <- hist(treated.propensity.score)
histogram.untreated <- hist(untreated.propensity.score)
plot(histogram.treated, col = rgb(0, 0, 1, 0.5), xlab = 'propensity score', 
	main = 'Histogram of propensity scores of treated and untreated obs.')
plot(histogram.untreated, add = TRUE, col = rgb(1, 0, 0, 0.5))
legend('top', legend = c('Treated', 'Untreated'), fill = c(rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5)))
abline(v = min(propensity.score[D.i == 1])+0.01, lty = 2)
abline(v = max(propensity.score[D.i == 0])-0.01, lty = 2)
text(x = 0.5, y = 25000, labels = 'common support')
arrows(x0 = 0.5, y0 = 22500, x1 = min(propensity.score[D.i == 1])+0.01+0.01, y1 = 22500, length = 0.1)
arrows(x0 = 0.5, y0 = 22500, x1 = max(propensity.score[D.i == 0])-0.01-0.01, y1 = 22500, length = 0.1)


#	Question 2.2
runs <- 100
mte.curves <- matrix(nrow = length(seq(0, 1, 0.02)), ncol = runs)
ATE <- c()
ATU <- c()
ATT <- c()
for (i in 1:runs) {
	X.i <- rnorm(n, 0, 1)
	Z.star.i <- rnorm(n, 0, 1)
	epsilon.i <- rnorm(n, 0, 0.5)
	U.D <- runif(n)
	
	U.0i <- 1.5 * (U.D - 0.5) - U.D^2 + epsilon.i
	U.1i <- -0.5 * (U.D - 0.5) + U.D^2 + epsilon.i
	
	v.i <- min(U.D) + (max(U.D) - min(U.D)) * U.D
	D.star.i <- pi.0 + pi.1 * Z.star.i + pi.2 * X.i + v.i
	
	Y.0i <- U.0i + beta.0 * X.i + theta * X.i^2
	Y.1i <- U.1i + beta.1 * X.i + theta * X.i^2
	
	D.i <- rep(0, n)
	D.i[D.star.i >= 0] <- 1
	Y.i <- D.i * Y.1i + (1 - D.i) * Y.0i
	
	first.stage <- glm(D.i ~ Z.star.i + X.i, family = binomial(link = "probit"))
	propensity.score <- first.stage$fitted
	
	mte.reg <- lm(Y.i ~ X.i + I(X.i^2) + propensity.score * X.i + propensity.score * I(X.i^2) + propensity.score + I(propensity.score^2) + I(propensity.score^3))
	gamma.hat.0 <-  mte.reg$coefficients['X.i']
	gamma.hat.1 <-  mte.reg$coefficients['I(X.i^2)']
	beta.hat.1 <- mte.reg$coefficients['X.i:propensity.score']
	beta.hat.2 <- mte.reg$coefficients['I(X.i^2):propensity.score']
	pi.hat.1 <- mte.reg$coefficients['propensity.score']
	pi.hat.2 <- mte.reg$coefficients['I(propensity.score^2)']
	pi.hat.3 <- mte.reg$coefficients['I(propensity.score^3)']
	mte.derivative <- beta.hat.1 * X.i + beta.hat.2 * X.i^2 + pi.hat.1 + 2 * pi.hat.2 * propensity.score + 3 * pi.hat.3 * propensity.score^2
	
	ATE <- append(ATE, mean(mte.derivative))
	ATU <- append(ATU, mean(mte.derivative[D.i == 0]))
	ATT <- append(ATT, mean(mte.derivative[D.i == 1]))

	mte.curve <- c()
	propensity.score <- round(propensity.score, 2)
	for (j in seq(0, 1, 0.02)){
		mte.curve <- append(mte.curve, mean(mte.derivative[propensity.score == j]))
	}
	mte.curves <- cbind(mte.curves, matrix(mte.curve))
}

mte.curve.avg <- c()
for (k in 1:length(seq(0, 1, 0.02))) {
	mte.curve.avg <- append(mte.curve.avg, mean(mte.curves[k,], na.rm = TRUE))
}
plot(seq(0, 1, 0.02)[!is.na(mte.curve.avg)], mte.curve.avg[!is.na(mte.curve.avg)], type = 'l',
	main = bquote('MTE curve with cubic term for'~K(p)), xlab = 'propensity score/unobserved gain from treatment',
	ylab = 'treatment effect')
segments(y0 = mean(ATE), y1 = mean(ATE), x0 = 0, x1 = 1, col = 'black', lty = 2)
segments(y0 = mean(ATU), y1 = mean(ATU), x0 = 0 , x1 = max(propensity.score[D.i == 0])-0.01-0.01, col = 'red', lty = 2)
segments(y0 = mean(ATT), y1 = mean(ATT), x0 = min(propensity.score[D.i == 1])+0.01+0.01, x1 = 1, col = 'blue', lty = 2)
legend('top', legend = c('MTE curve', 'ATE', 'ATU', 'ATT'), lty = c(1, 2, 2, 2), col = c('black', 'black', 'red', 'blue'))
abline(v = min(propensity.score[D.i == 1])+0.01, lty = 2)
abline(v = max(propensity.score[D.i == 0])-0.01, lty = 2)


#	Question 2.3
runs <- 100
mte.curves <- matrix(nrow = length(seq(0, 1, 0.02)), ncol = runs)
ATE <- c()
ATU <- c()
ATT <- c()
for (i in 1:runs) {
	X.i <- rnorm(n, 0, 1)
	Z.star.i <- rnorm(n, 0, 1)
	epsilon.i <- rnorm(n, 0, 0.5)
	U.D <- runif(n)
	
	U.0i <- 1.5 * (U.D - 0.5) - U.D^2 + epsilon.i
	U.1i <- -0.5 * (U.D - 0.5) + U.D^2 + epsilon.i
	
	v.i <- min(U.D) + (max(U.D) - min(U.D)) * U.D
	D.star.i <- pi.0 + pi.1 * Z.star.i + pi.2 * X.i + v.i
	
	Y.0i <- U.0i + beta.0 * X.i + theta * X.i^2
	Y.1i <- U.1i + beta.1 * X.i + theta * X.i^2
	
	D.i <- rep(0, n)
	D.i[D.star.i >= 0] <- 1
	Y.i <- D.i * Y.1i + (1 - D.i) * Y.0i
	
	first.stage <- glm(D.i ~ Z.star.i + X.i, family = binomial(link = "probit"))
	propensity.score <- first.stage$fitted
	
	mte.reg <- lm(Y.i ~ X.i + I(X.i^2) + propensity.score * X.i + propensity.score * I(X.i^2) + propensity.score + I(propensity.score^2))
	gamma.hat.0 <-  mte.reg$coefficients['X.i']
	gamma.hat.1 <-  mte.reg$coefficients['I(X.i^2)']
	beta.hat.1 <- mte.reg$coefficients['X.i:propensity.score']
	beta.hat.2 <- mte.reg$coefficients['I(X.i^2):propensity.score']
	pi.hat.1 <- mte.reg$coefficients['propensity.score']
	pi.hat.2 <- mte.reg$coefficients['I(propensity.score^2)']
	mte.derivative <- beta.hat.1 * X.i + beta.hat.2 * X.i^2 + pi.hat.1 + 2 * pi.hat.2 * propensity.score
	
	ATE <- append(ATE, mean(mte.derivative))
	ATU <- append(ATU, mean(mte.derivative[D.i == 0]))
	ATT <- append(ATT, mean(mte.derivative[D.i == 1]))

	mte.curve <- c()
	propensity.score <- round(propensity.score, 2)
	for (j in seq(0, 1, 0.02)){
		mte.curve <- append(mte.curve, mean(mte.derivative[propensity.score == j]))
	}
	mte.curves <- cbind(mte.curves, matrix(mte.curve))
}

mte.curve.avg <- c()
for (k in 1:length(seq(0, 1, 0.02))) {
	mte.curve.avg <- append(mte.curve.avg, mean(mte.curves[k,], na.rm = TRUE))
}
plot(seq(0, 1, 0.02)[!is.na(mte.curve.avg)], mte.curve.avg[!is.na(mte.curve.avg)], type = 'l',
	main = bquote('MTE curve with squared term for'~K(p)), xlab = 'propensity score/unobserved gain from treatment',
	ylab = 'treatment effect')
segments(y0 = mean(ATE), y1 = mean(ATE), x0 = 0, x1 = 1, col = 'black', lty = 2)
segments(y0 = mean(ATU), y1 = mean(ATU), x0 = 0 , x1 = max(propensity.score[D.i == 0])-0.01-0.01, col = 'red', lty = 2)
segments(y0 = mean(ATT), y1 = mean(ATT), x0 = min(propensity.score[D.i == 1])+0.01+0.01, x1 = 1, col = 'blue', lty = 2)
legend('top', legend = c('MTE curve', 'ATE', 'ATU', 'ATT'), lty = c(1, 2, 2, 2), col = c('black', 'black', 'red', 'blue'))
abline(v = min(propensity.score[D.i == 1])+0.01, lty = 2)
abline(v = max(propensity.score[D.i == 0])-0.01, lty = 2)


#	Question 2.4
runs <- 100
mte.curves <- matrix(nrow = length(seq(0, 1, 0.02)), ncol = runs)
ATE <- c()
ATU <- c()
ATT <- c()
for (i in 1:runs) {
	X.i <- rnorm(n, 0, 1)
	Z.star.i <- rnorm(n, 0, 1)
	epsilon.i <- rnorm(n, 0, 0.5)
	U.D <- runif(n)
	
	U.0i <- 1.5 * (U.D - 0.5) - U.D^2 + epsilon.i
	U.1i <- -0.5 * (U.D - 0.5) + U.D^2 + epsilon.i
	
	v.i <- min(U.D) + (max(U.D) - min(U.D)) * U.D
	D.star.i <- pi.0 + pi.1 * Z.star.i + pi.2 * X.i + v.i
	
	Y.0i <- U.0i + beta.0 * X.i + theta * X.i^2
	Y.1i <- U.1i + beta.1 * X.i + theta * X.i^2
	
	D.i <- rep(0, n)
	D.i[D.star.i >= 0] <- 1
	Y.i <- D.i * Y.1i + (1 - D.i) * Y.0i
	
	first.stage <- glm(D.i ~ Z.star.i + X.i, family = binomial(link = "probit"))
	propensity.score <- first.stage$fitted
	
	mte.reg <- lm(Y.i ~ X.i + propensity.score * X.i + propensity.score + I(propensity.score^2) + I(propensity.score^3))
	gamma.hat.0 <-  mte.reg$coefficients['X.i']
	beta.hat.1 <- mte.reg$coefficients['X.i:propensity.score']
	pi.hat.1 <- mte.reg$coefficients['propensity.score']
	pi.hat.2 <- mte.reg$coefficients['I(propensity.score^2)']
	pi.hat.3 <- mte.reg$coefficients['I(propensity.score^3)']
	mte.derivative <- beta.hat.1 * X.i + pi.hat.1 + 2 * pi.hat.2 * propensity.score + 3 * pi.hat.3 * propensity.score^2
	
	ATE <- append(ATE, mean(mte.derivative))
	ATU <- append(ATU, mean(mte.derivative[D.i == 0]))
	ATT <- append(ATT, mean(mte.derivative[D.i == 1]))

	mte.curve <- c()
	propensity.score <- round(propensity.score, 2)
	for (j in seq(0, 1, 0.02)){
		mte.curve <- append(mte.curve, mean(mte.derivative[propensity.score == j]))
	}
	mte.curves <- cbind(mte.curves, matrix(mte.curve))
}

mte.curve.avg <- c()
for (k in 1:length(seq(0, 1, 0.02))) {
	mte.curve.avg <- append(mte.curve.avg, mean(mte.curves[k,], na.rm = TRUE))
}
plot(seq(0, 1, 0.02)[!is.na(mte.curve.avg)], mte.curve.avg[!is.na(mte.curve.avg)], type = 'l',
	main = bquote('MTE curve without terms for'~X[i]^2), xlab = 'propensity score/unobserved gain from treatment',
	ylab = 'treatment effect')
segments(y0 = mean(ATE), y1 = mean(ATE), x0 = 0, x1 = 1, col = 'black', lty = 2)
segments(y0 = mean(ATU), y1 = mean(ATU), x0 = 0 , x1 = max(propensity.score[D.i == 0])-0.01-0.01, col = 'red', lty = 2)
segments(y0 = mean(ATT), y1 = mean(ATT), x0 = min(propensity.score[D.i == 1])+0.01+0.01, x1 = 1, col = 'blue', lty = 2)
legend('top', legend = c('MTE curve', 'ATE', 'ATU', 'ATT'), lty = c(1, 2, 2, 2), col = c('black', 'black', 'red', 'blue'))
abline(v = min(propensity.score[D.i == 1])+0.01, lty = 2)
abline(v = max(propensity.score[D.i == 0])-0.01, lty = 2)