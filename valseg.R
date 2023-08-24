#####
# Normális eloszlás
# várható érték: mean
# szórás: sd

# random generálás
rnorm(100,mean = 0, sd = 1) 

# sűrűségfv.
dnorm(0, mean = 0, sd = 1)
dnorm(1/sqrt(2*pi)*exp(0))

# eloszlásfv.
pnorm(0.9, mean = 0, sd = 1)

# kvantilisek
qnorm(0.95, mean = 0, sd = 1)

#####
# Binomiális eloszlás
# n: size
# p: prob

# random generálás
rbinom(100, size = 10, prob = 0.5)

# valószínűség
dbinom(3, size = 10, prob = 0.5)
choose(10,3) #10 alat a 3
choose(10,3)*0.5^3*(1-0.5)^(10-3)


#####
# Egyéb eloszlások
# *exp: exponenciális
# *pois: poisson
# *unif: egyenletes
# *gamma: gamma

#####
# Minta vizsgálata ~ N(3, 2^2)

minta = rnorm(1000, 3, 2)
mean(minta)
sd(minta) #korrig�lt empirikus sz�r�s
var(minta) #sz�r�sn�gyzet
cumsum(1:10) #kumul�lt �sszeg

# Feladat: Ábrázoljuk a kumulált átlagot és a várható értéket!

plot(cumsum(minta)/(1:1000),type='l')
abline(3,0,col=2)

x=seq(-4,10,0.1)
lines(x,dnorm(x,3,2),col=2) #col=2 piros

hist(minta) 
hist(minta, freq=F) #s�r�s�gbecsl�s

# Feladat: Rajzoljuk az ábrára a megfelelő sűrűségfüggvényt!

plot(ecdf(minta), main = 'Empirikus eloszlásfüggvény')
lines(x,pnorm(x,3,2),col=2)

# Feladat: Rajzoljuk az ábrára a megfelelő eloszlásfüggvényt!

# Feladat: Legyen X~Binom(10,0.5). Készítsünk kísérletet a centrális határeloszláshoz!
atlag=c()
for(i in 1:2000){m=mean(rbinom(1000,10,0.5))
atlag=c(atlag,m)
#atlag[i]=m, el�tte egy 2000 hossz� vektor legener�l�sa
}
hist((atlag-mean(atlag))/sd(atlag),xlab='Standardiz�lt �tlag', main='Hisztogram',freq=F)
lines(x,dnorm(x),col=2)

hist((atlag-10*0.5)/sqrt(0.5^2*10/2000),xlab='Standardiz�lt �tlag', main='Hisztogram',freq=F)
lines(x,dnorm(x),col=2)
# Maximum Likelihood
# https://towardsdatascience.com/maximum-likelihood-estimation-in-r-b21f68f1eba4

# I don’t know about you but I’m feeling
set.seed(22)# Generate an outcome, ie number of heads obtained, assuming a fair coin was used for the 100 flips
heads <- rbinom(1,100,0.5)
heads

# To illustrate, let's find the likelihood of obtaining these results if p was 0.6—that is, if our coin was biased in such a way to show heads 60% of the time.
biased_prob <- 0.6# Explicit calculation
choose(100,52)*(biased_prob**52)*(1-biased_prob)**48# 0.0214877567069514# Using R's dbinom function (density function for a given binomial distribution)
dbinom(heads,100,biased_prob)# 0.0214877567069514

likelihood <- function(p){
  dbinom(heads, 100, p)
}# Test that our function gives the same result as in our earlier example
likelihood(biased_prob)# 0.0214877567069513

negative_likelihood <- function(p){
  dbinom(heads, 100, p)*-1
}# Test that our function is behaving as expected
negative_likelihood(biased_prob)# -0.0214877567069513

nlm(negative_likelihood,0.5,stepmax=0.5)

#Exp ML
minta=rexp(100,2)
neg_lh=function(lambda){
  prod(dexp(minta,lambda))
}
nlm(neg_lh,1)
