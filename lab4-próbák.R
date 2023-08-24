###########################
# One- and two-sample tests
###########################

# daily energy intake in kJ for 11 women ########
daily.intake <- c(5260,5470,5640,6180,6390,6515,
                  6805,7515,7515,8230,8770)
mean(daily.intake)
sd(daily.intake)

di_t = t.test(daily.intake,mu=7725)
di_t
di_t$conf.int #conf.level alap?rtelmezetten 0.95
t.test(daily.intake,mu=7725, 
       conf.level=0.99, alt = "l") # alternative = "less" / "g" -- "greater"

#?rt?kalap?
u_1=qt(0.025,10) #(n-1) a szabads?gi fok, itt: 11-1=10
u_2=qt(1-0.025,10) #quantile
t_ps=di_t$statistic
(u_1<t_ps) & (u_2>t_ps)

di_t2$p.value>0.01


qqplot(daily.intake)
qqnorm(daily.intake) # x-norm eloszl?s, y-minta, ha egyens  --> norm?lis
qqline(daily.intake) #el?z?re illeszked? egyenest rajzolja be

plot(qnorm(1:11/12),sort(daily.intake)) #DIY qqnorm()

wilcox.test(daily.intake, mu=7725) #medi?n,rang
w.ts = function(x, mu){
  r = rank(abs(x-mu))
  V = sum(r[x>mu])
  V
} #k?zi kisz?mol?s, pr?bastatot adja meg
w.ts(daily.intake, 7725)

# roomwidth ############  k?t mint?s t-pr?ba
# library(HSAUR2)
# ?roomwidth
data("roomwidth", package = "HSAUR2")
head(roomwidth,10)

convert <- ifelse(roomwidth$unit == "feet", 1, 3.28) # bool, T=1,F=3.28
tapply(roomwidth$width * convert, roomwidth$unit, summary)
tapply(roomwidth$width * convert, roomwidth$unit, sd)

layout(matrix(c(1,2,1,3), nrow = 2, ncol = 2, byrow = FALSE)) #?j v?ltoz? gen: pl. I(v?ltoz?*v?ltoz?)--> 1 db ?j

attach(roomwidth)
ujvalt=width * convert
boxplot(ujvalt ~ unit, data = roomwidth,
        ylab = "Estimated width (feet)",
        varwidth = TRUE, names = c("Estimates in feet",
                                   "Estimates in metres (converted to feet)"))


boxplot(I(width * convert) ~ unit, data = roomwidth,
             ylab = "Estimated width (feet)",
             varwidth = TRUE, names = c("Estimates in feet",
                                          "Estimates in metres (converted to feet)"))
feet <- roomwidth$unit == "feet"
qqnorm(roomwidth$width[feet],
            ylab = "Estimated width (feet)")
qqline(roomwidth$width[feet])
# x = roomwidth$width[feet]; n = length(x); plot(qnorm(1:n / (n+1)), sort(x)); abline(mean(x),sd(x))
qqnorm(roomwidth$width[!feet],
            ylab = "Estimated width (metres)")
qqline(roomwidth$width[!feet])

t.test(I(width * convert) ~ unit, data = roomwidth,
         var.equal = TRUE)

var.test(I(width * convert) ~ unit, data = roomwidth) #F-teszt k?t mint?ra
t.test(I(width * convert) ~ unit, data = roomwidth,
         var.equal = FALSE)
t.test(I(width * convert) ~ unit, data = roomwidth,
       var.equal = FALSE,conf.level=0.99)

t.test(c(2,4,3,5),c(1,2,7,6,4)) # t-teszt vagy formul?val vagy vektorral

wilcox.test(I(width * convert) ~ unit, data = roomwidth,
            conf.int = TRUE)

B = roomwidth$width[roomwidth$unit == "feet"]
A = roomwidth$width[roomwidth$unit == "metres"]*3.28
sum(outer(A, B, "<"))

# Wave Energy Device Mooring ##### --p?ros t-pr?ba van ?sszef?gg?s a v?ltoz?k k?z?tt -->2b?l 1
data("waves", package = "HSAUR2")

mooringdiff <- waves$method1 - waves$method2
layout(matrix(1:2, ncol = 2))
boxplot(mooringdiff, ylab = "Differences (Newton metres)",
        main = "Boxplot")
abline(h = 0, lty = 2)
qqnorm(mooringdiff, ylab = "Differences (Newton metres)")
qqline(mooringdiff)

t.test(waves$method1, waves$method2, paired = T) #easy megad?s, p?ros 
t.test(mooringdiff) #egy mint?s

# Piston-ring Failures ###### --f?ggetlens?g vizsg?lat, k?l?nb?z? oszt?lyok
data("pistonrings", package = "HSAUR2")
#?pistonrings
str(pistonrings)
chisq.test(pistonrings) #f?ggetlenek
chisq.test(pistonrings)$residuals


library("vcd")
assoc(pistonrings) #min?l nagyobb,ann?l elt?r?bb (C2,C3 j?l m?k?dik)

# Rearrests of Juveniles #######--azonos oszt?lyok(bels? szintek) --> nem j? a khi-n?gyzet -->mcnemar-pr?ba

data("rearrests", package = "HSAUR2")
rearrests
mcnemar.test(rearrests, correct = FALSE) #k?t v?ltoz? ?sszef?gg
binom.test(rearrests[2], n = sum(rearrests[c(2,3)]))

############
# Exercise 1
############
# After the students had made the estimates of the width of the lecture
# hall the room width was accurately measured and found to be 13.1 metres
# (43.0 feet). Use this additional information to determine which of the two
# types of estimates was more precise.
t1 = t.test(roomwidth$width[roomwidth$unit=="metres"], mu = 13.1)
t2 = t.test(roomwidth$width[roomwidth$unit=="feet"], mu = 43)
t1$p.value
t2$p.value

#outlierek kiv?tele
bb=boxplot(roomwidth$width[roomwidth$unit=="metres"])
bb$out
out.meter=bb$out
t3= t.test(roomwidth$width[roomwidth$unit=="metres"] [-out.meter], mu = 13.1)
t3$p.value

bp2=boxplot(roomwidth$width[roomwidth$unit=="metres"][-out.meter])
bp2$out


library(ISwR)
############
# Exercise 2 - data("react", package = "ISwR")
############
# Do the values of the react data set (notice that this is a single vector,
# not a data frame) look reasonably normally distributed? Does the mean
# differ significantly from zero according to a t test?
?react
qqnorm(react)
qqline(react)
t.test(react)

############
# Exercise 3
############
# The function shapiro.test computes a test of normality based on
# the degree of linearity of the Q–Q plot. Apply it to the react data. Does
# it help to remove the outliers?
shapiro.test(react)
b = boxplot(react)
bout = which(react %in% b$out)
shapiro.test(react[-bout])
