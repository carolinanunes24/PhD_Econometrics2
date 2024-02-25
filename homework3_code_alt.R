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


#	Set up model.
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


#	Question 2.2

#	Regression.
mte.reg <- lm(Y.i ~ X.i + I(X.i^2) + propensity.score * X.i + propensity.score * I(X.i^2) + propensity.score + I(propensity.score^2) + I(propensity.score^3))
gamma.hat.0 <-  mte.reg$coefficients['X.i']
gamma.hat.1 <-  mte.reg$coefficients['I(X.i^2)']
beta.hat.1 <- mte.reg$coefficients['X.i:propensity.score']
beta.hat.2 <- mte.reg$coefficients['I(X.i^2):propensity.score']
pi.hat.1 <- mte.reg$coefficients['propensity.score']
pi.hat.2 <- mte.reg$coefficients['I(propensity.score^2)']
pi.hat.3 <- mte.reg$coefficients['I(propensity.score^3)']

#	Terms of Xp:
#	beta.hat.1 & beta.hat.2

#	Terms of p:
#	pi.hat.1, pi.hat.2 & pi.hat.3

u <- seq(0, 1, 0.01)
matrix.Xp <- beta.hat.1 * X.i + beta.hat.2 * X.i^2
matrix.Kp <- pi.hat.1 + 2 * pi.hat.2 * u + 3 * pi.hat.3 * u^2
matrix.mte <- outer(matrix.Xp, matrix.Kp, `+`)

ATE.p <- rowMeans(matrix.mte, na.rm = TRUE)
ATE <- mean(ATE.p)

weights.ATT <- outer(propensity.score, u, `>=`)
ATT.p <- rowSums(matrix.mte * weights.ATT, na.rm = TRUE) / rowSums(weights.ATT, na.rm = TRUE)
ATT <- mean(ATT.p[D.i == 1])

weights.ATU <- outer(propensity.score, u, `<`)
ATU.p <- rowSums(matrix.mte * weights.ATU, na.rm = TRUE) / rowSums(weights.ATU, na.rm = TRUE)
ATU <- mean(ATU.p[D.i == 0])

mte.curve <- c()
for (j in 1:length(seq(0, 1, 0.01))){
	mte.curve <- append(mte.curve, mean(matrix.Xp) + matrix.Kp[j])
}
plot(seq(0, 1, 0.01), mte.curve, type = 'l',
	main = bquote('MTE curve with cubic term for'~K(p)), xlab = 'propensity score/unobserved gain from treatment',
	ylab = 'treatment effect')


#	Question 2.3

#	Regression.
mte.reg <- lm(Y.i ~ X.i + I(X.i^2) + propensity.score * X.i + propensity.score * I(X.i^2) + propensity.score + I(propensity.score^2))
gamma.hat.0 <-  mte.reg$coefficients['X.i']
gamma.hat.1 <-  mte.reg$coefficients['I(X.i^2)']
beta.hat.1 <- mte.reg$coefficients['X.i:propensity.score']
beta.hat.2 <- mte.reg$coefficients['I(X.i^2):propensity.score']
pi.hat.1 <- mte.reg$coefficients['propensity.score']
pi.hat.2 <- mte.reg$coefficients['I(propensity.score^2)']

#	Terms of Xp:
#	beta.hat.1 & beta.hat.2

#	Terms of p:
#	pi.hat.1 & pi.hat.2

u <- seq(0, 1, 0.01)
matrix.Xp <- beta.hat.1 * X.i + beta.hat.2 * X.i^2
matrix.Kp <- pi.hat.1 + 2 * pi.hat.2 * u
matrix.mte <- outer(matrix.Xp, matrix.Kp, `+`)

ATE.p <- rowMeans(matrix.mte, na.rm = TRUE)
ATE <- mean(ATE.p)

weights.ATT <- outer(propensity.score, u, `>=`)
ATT.p <- rowSums(matrix.mte * weights.ATT, na.rm = TRUE) / rowSums(weights.ATT, na.rm = TRUE)
ATT <- mean(ATT.p[D.i == 1])

weights.ATU <- outer(propensity.score, u, `<`)
ATU.p <- rowSums(matrix.mte * weights.ATU, na.rm = TRUE) / rowSums(weights.ATU, na.rm = TRUE)
ATU <- mean(ATU.p[D.i == 0])

mte.curve <- c()
for (j in 1:length(seq(0, 1, 0.01))){
	mte.curve <- append(mte.curve, mean(matrix.Xp) + matrix.Kp[j])
}
plot(seq(0, 1, 0.01), mte.curve, type = 'l',
	main = bquote('MTE curve with squared term for'~K(p)), xlab = 'propensity score/unobserved gain from treatment',
	ylab = 'treatment effect')


#	Question 2.4

#	Regression.
mte.reg <- lm(Y.i ~ X.i + propensity.score * X.i + propensity.score + I(propensity.score^2) + I(propensity.score^3))
gamma.hat.0 <-  mte.reg$coefficients['X.i']
beta.hat.1 <- mte.reg$coefficients['X.i:propensity.score']
pi.hat.1 <- mte.reg$coefficients['propensity.score']
pi.hat.2 <- mte.reg$coefficients['I(propensity.score^2)']
pi.hat.3 <- mte.reg$coefficients['I(propensity.score^3)']

#	Terms of Xp:
#	beta.hat.1

#	Terms of p:
#	pi.hat.1, pi.hat.2 & pi.hat.3

u <- seq(0, 1, 0.01)
matrix.Xp <- beta.hat.1 * X.i
matrix.Kp <- pi.hat.1 + 2 * pi.hat.2 * u + 3 * pi.hat.3 * u^2
matrix.mte <- outer(matrix.Xp, matrix.Kp, `+`)

ATE.p <- rowMeans(matrix.mte, na.rm = TRUE)
ATE <- mean(ATE.p)

weights.ATT <- outer(propensity.score, u, `>=`)
ATT.p <- rowSums(matrix.mte * weights.ATT, na.rm = TRUE) / rowSums(weights.ATT, na.rm = TRUE)
ATT <- mean(ATT.p[D.i == 1])

weights.ATU <- outer(propensity.score, u, `<`)
ATU.p <- rowSums(matrix.mte * weights.ATU, na.rm = TRUE) / rowSums(weights.ATU, na.rm = TRUE)
ATU <- mean(ATU.p[D.i == 0])

mte.curve <- c()
for (j in 1:length(seq(0, 1, 0.01))){
	mte.curve <- append(mte.curve, mean(matrix.Xp) + matrix.Kp[j])
}
plot(seq(0, 1, 0.01), mte.curve, type = 'l',
	main = bquote('MTE curve without terms for'~X[i]^2), xlab = 'propensity score/unobserved gain from treatment',
	ylab = 'treatment effect')