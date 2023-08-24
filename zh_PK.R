#1.feladat
#a)
attach(mtcars)
tapply(hp,cyl,mean)
#b)
bb1=boxplot(hp)
length(bb1$out) # 1db utlier lesz

ind_out=bb$out
ind_kell=which(hp==ind_out)
mtcars[ind_kell,] #Maserati Bora


#2.feladat
#a)
cr <- c(3, 4, 5, 9, 17, 31, 18, 26, 40, 46, 34, 31, 48, 35, 25, 26, 21, 31, 29, 16)
messi <- c(1, 6, 14, 10, 23, 34, 31, 50, 46, 28, 43, 26, 37, 34, 36, 25, 30, 3)
layout(matrix(1:2, ncol = 2))
hist(cr)
hist(messi)

#egyik sem normális eloszlású /(log-transzformációval sem)

#b)
length(cr)
length(messi)

t.test(cr,messi,alternative = "l") #igaz a nullhipotézis, a t-értéket tartalmazza  konf. intervallum


#3feladat
df=data.frame(magassag <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131),
              suly <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48))

#a)
lm_1=lm(suly ~ magassag)     
lm_1
#ax+b=0.674*x-38.45
summary(lm_1) #normális eloszlású, a p-érték túl kicsi, így a nullhipotézist elvetjük --> függő
cor(suly,magassag) #korreláltak, jó/pontos lesz a regresszió


#b)-szórásdiagram
library(lattice)
plot(suly ~ magassag) 
abline(lm(suly~magassag), col="red")


#c)
res1=residuals(lm_1)
qqnorm(res1, ylab = "Residuals")
qqline(res1,col=2)
#illeszkednek az egyenesre, így normális eloszlású

#d)
p186 = predict(lm_1, data.frame(magassag=186))
p186
