#######################################
# Simple and Multiple Linear Regression
#######################################

library(HSAUR2)

##########
# Faithful

? faithful
plot(faithful$eruptions,faithful$waiting)

cor(faithful$eruptions,faithful$waiting) #min?l nagyobb a korrel?ci?, ann?l jobb a lin.reg.

f.lm = lm( waiting ~ eruptions, data = faithful) #ax+b
summary(f.lm) #coeff: intercept:b, eruptions: a, ?tlag: 0, multiple r-sq=korr^2
2*(1-pt(28.98,270))

plot(faithful$eruptions,faithful$waiting)
abline(f.lm,col=2)

p3 = predict(f.lm, data.frame(eruptions=3))  #y-koord az egyenesről
p3
pv = predict(f.lm, data.frame(eruptions=c(2,3,4)))
pv

wait.range = range(faithful$eruptions)
pred = predict(f.lm, data.frame(eruptions = wait.range), interval = "prediction")
pred
lines(wait.range, pred[,'lwr'], col = 4)
lines(wait.range, pred[,'upr'], col = 4)

####################################
# Estimating the Age of the Universe

library(gamair)
data("hubble")
colnames(hubble) = c("galaxy","velocity","distance")
?hubble

# velocity = β1 distance + ε   Hubble's Law

plot(velocity ~ distance, data = hubble)

sum(hubble$distance * hubble$velocity) / sum(hubble$distance^2) # using the formula

hmod <- lm(velocity ~ distance - 1, data = hubble)
coef(hmod)
predict(hmod,data.frame(distance = 10, random = 20))

layout(matrix(1:2, ncol = 2))
plot(velocity ~ distance, data = hubble)
abline(hmod)
plot(hmod, which = 1)

# Now we can use the estimated value of β 1 to find an approximate value
# for the age of the universe. The Hubble constant itself has units of km ×
# sec^−1 × Mpc^−1 . A mega-parsec (Mpc) is 3.09 × 10 19 km, so we need to divide
# the estimated value of β1 by this amount in order to obtain Hubble’s constant
# with units of sec^−1 . The approximate age of the universe in seconds will then
# be the inverse of this calculation.

Mpc <- 3.09 * 10^19
ysec <- 60^2 * 24 * 365.25
Mpcyear <- Mpc / ysec
1 / (coef(hmod) / Mpcyear) # roughly 12.8 billion years

###############
# Cloud Seeding

?clouds

layout(matrix(1:2, nrow = 1))
bxpseeding <- boxplot(rainfall ~ seeding, data = clouds,
                        ylab = "Rainfall", xlab = "Seeding")
bxpecho <- boxplot(rainfall ~ echomotion, data = clouds,
                     ylab = "Rainfall", xlab = "Echo Motion")

layout(matrix(1:4, nrow = 2))
plot(rainfall ~ time, data = clouds)
plot(rainfall ~ cloudcover, data = clouds)
plot(rainfall ~ sne, data = clouds, xlab="S-Ne criterion")
plot(rainfall ~ prewetness, data = clouds)

rownames(clouds)[clouds$rainfall %in% c(bxpseeding$out,
                                          bxpecho$out)]

clouds_formula <- rainfall ~ seeding +
  seeding:(sne + cloudcover + prewetness + echomotion) +
  time

clouds_lm <- lm(clouds_formula, data = clouds)
class(clouds_lm)

summary(clouds_lm)

betastar <- coef(clouds_lm)
betastar

psymb <- as.numeric(clouds$seeding)
plot(rainfall ~ sne, data = clouds, pch = psymb,
       xlab = "S-Ne criterion")
abline(lm(rainfall ~ sne, data = clouds,
            subset = seeding == "no"))
abline(lm(rainfall ~ sne, data = clouds,
            subset = seeding == "yes"), lty = 2)
legend("topright", legend = c("No seeding", "Seeding"),
         pch = 1:2, lty = 1:2, bty = "n")

clouds_resid <- residuals(clouds_lm)
clouds_fitted <- fitted(clouds_lm)

plot(clouds_fitted, clouds_resid, xlab = "Fitted values",
       ylab = "Residuals", type = "n",
       ylim = max(abs(clouds_resid)) * c(-1, 1))
abline(h = 0, lty = 2)
text(clouds_fitted, clouds_resid, labels = rownames(clouds))

qqnorm(clouds_resid, ylab = "Residuals")
qqline(clouds_resid)
