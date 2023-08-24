########################################
# Data Analysis Using Graphical Displays
########################################

library(HSAUR2)

####################
# Malignant Melanoma

head(USmelanoma) #els? hat megfigyel?s, head(USmelanoma,10) --> m?r 10-et ki?r

# USA map
plot(-USmelanoma$longitude, USmelanoma$latitude, col = "white")
#plot(-USmelanoma$longitude, USmelanoma$latitude, t = "n") ?res ?bra, minta col="white"-os
text(-USmelanoma$longitude,USmelanoma$latitude,rownames(USmelanoma)) # - longitude, mert balra vagyunk

# make sure that the x-axis is the same in both graphs
xr <- range(USmelanoma$mortality) * c(0.9, 1.1)
xr

layout(matrix(1:2, nrow = 2))
# set margin
par(mar = par("mar") * c(0.8, 1, 1, 1))
boxplot(USmelanoma$mortality, ylim = xr, horizontal = TRUE,
        xlab = "Mortality")
hist(USmelanoma$mortality, xlim = xr, xlab = "", main = "",
     axes = F, ylab = "")
axis(1)

# right skew
library(e1071)
skewness(USmelanoma$mortality)

# Navigate to 'Plots' and 'Remove plots' (broom icon) to get the original plot settings
# The mortality is increased in east or west coast states compared to the rest of the country
plot(mortality ~ ocean, data = USmelanoma,
       xlab = "Contiguity to an ocean", ylab = "Mortality")

# Density estimation (NOT NEEDED)
dyes <- with(USmelanoma, density(mortality[ocean == "yes"]))
dno <- with(USmelanoma, density(mortality[ocean == "no"]))
plot(dyes, lty = 1, xlim = xr, main = "", ylim = c(0, 0.018))
lines(dno, lty = 2)
legend("topleft", lty = 1:2, legend = c("Coastal State",
                                        "Land State"), bty = "n")

layout(matrix(1:2, ncol = 2))
# Uncorrelated
plot(mortality ~ longitude, data = USmelanoma)
# String negative correlation
plot(mortality ~ latitude, data = USmelanoma)

# On the same latitude mortality is higher for coast states
plot(mortality ~ latitude, data = USmelanoma,
       pch = as.integer(USmelanoma$ocean))
legend("topright", legend = c("Land state", "Coast state"),
            pch = 1:2, bty = "n")
# southernmost states
subset(USmelanoma, latitude < 32)

################################
# Chinese Health and Family Life

head(CHFLS)
summary(CHFLS)

# Barplot from crosstabs (contingency table) #xtabs:gyakoris?gi t?bl?zat
barplot(xtabs(~ R_happy, data = CHFLS))

# categorical ~ categorical   ->  Spineplot: kateg?ria f?ggv?ny?ben ?br?zolunk egy m?sik kateg?ri?t
plot(R_happy ~ R_health, data = CHFLS) 
# E.g. when health is better, the ratio of very unhappy women decreases

# Contingency table (as independent variables)
xtabs(~ R_happy + R_health, data = CHFLS)

layout(matrix(1:2, ncol = 2))
hist(CHFLS$R_income)
hist(log(CHFLS$R_income))
# categorical ~ numeric   ->   Spinogram
# check happiness conditional to log-income
plot(R_happy ~ log(R_income + 1), data = CHFLS) 
# income increases -> ratio of unhappy women decreases
cdplot(R_happy ~ log(R_income + 1), data = CHFLS) # conditional density plot

library(lattice)
# Scatter plot conditional to education of the woman (in plot() we can't give extra conditions)
xyplot(jitter(log(A_income + 0.5)) ~
         jitter(log(R_income + 0.5)) | R_edu, data = CHFLS) #jitter:zajt ad hozz?

xyplot(log(A_income + 0.5) ~
         log(R_income + 0.5) | R_edu, data = CHFLS)

############
# Exercise 1 - household
############
# Investigate how the division of household
# expenditure between the four commodity groups 
# depends on total expenditure and to find out whether
# this relationship differs for men and women.

household$total_income=apply(household[,1:4],1, sum)
plot(household) #f?ggetlen v?ltoz? sorban, f?gg? oszlop pl. total_income az utols? sorban  lesz

xyplot(food~total_income|gender,data=household)
xyplot(service~total_income|gender,data=household)
xyplot(housing~total_income|gender,data=household)
xyplot(goods~total_income|gender,data=household)
############
# Exercise 2 - suicides2
############
# Construct side-by-side box plots for the data
# from different age groups, and comment on
# what the graphic tells us about the data.
boxplot(suicides2)
text(1:5,suicides2['Hungary',],rep('Hungary',5),col=2)
