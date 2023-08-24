#Everitt k?nyv
# Forbes2000 (2004)

library("HSAUR2")

# Import data from HSAUR2 package
data("Forbes2000", package = "HSAUR2")

# Import data from .csv file
forbes = "/home/ati/Dokumentumok/Egyetem/Comp stat/codes/Forbes2000.csv"
csvForbes2000 <- read.table(forbes, header = TRUE, sep = ",", row.names = 1,
                            colClasses = c("character", "integer", "character",
                            "factor", "factor", "numeric", "numeric", "numeric",
                            "numeric"))
# or use: Environment -> Import Dataset -> From text...
# You can import Excel, SPSS...

# First 6 observations
head(Forbes2000)
# Structure of the object
str(Forbes2000)

# Class
class(Forbes2000)
# Dimensions
dim(Forbes2000)
# Number of rows
nrow(Forbes2000)
# Number of columns
ncol(Forbes2000)
# Variable names
names(Forbes2000)

# Class of the "rank" variable
class(Forbes2000[,"rank"])
# Length of the vector
length(Forbes2000[,"rank"])

# 1st value in the "name" variable
Forbes2000[,"name"][1]
class(Forbes2000[,"category"])
# Number of categories in a factor variable
nlevels(Forbes2000[,"category"])
# Possible categories 
levels(Forbes2000[,"category"])

# Frequency table
table(Forbes2000[,"category"])

# ? median
class(Forbes2000[,"sales"])
median(Forbes2000[,"sales"])
mean(Forbes2000[,"sales"])
range(Forbes2000[,"sales"])
summary(Forbes2000[,"sales"])

boxplot(log(Forbes2000$sales) ~ Forbes2000$country)

# Objects in the database can be accessed by simply giving their names
attach(Forbes2000)
boxplot(log(sales) ~ country)


# Remove it from the search path
detach(Forbes2000)
# name # -> error

companies <- Forbes2000[,"name"]
companies <- Forbes2000$name
companies[1:3]
# Name, sales, profits and assets of the first 3 companies
Forbes2000[1:3, c("name", "sales", "profits", "assets")]

# With basic options it gives ascending order (the INDICES of the values) --növekvő 
order_sales <- order(Forbes2000$sales) #indexeket rendez
order_sales
# The first 3 companies with the lowest sales
which(companies=="Custodia Holding")
companies[order_sales[1:3]]

# First 3 companies with largest sales
forder_sales <- rev(order(Forbes2000$sales))
companies[forder_sales[1:3]]
Forbes2000[forder_sales[1:3], c("name", "sales", "profits", "assets")]

length(order_sales)
Forbes2000[order_sales[c(2000, 1999, 1998)], c("name", "sales", "profits", "assets")]

# Companies with assets greater than 1000
Forbes2000[Forbes2000$assets > 1000, c("name", "sales", "profits", "assets")]

# Number of companies with assets > 1000
table(Forbes2000$assets > 1000)

# Number of missing values in profits
na_profits <- is.na(Forbes2000$profits)

companies[which(na_profits==T)]
Forbes2000[na_profits==T, c("name", "sales", "profits", "assets")]
Forbes2000[na_profits==T,1:8]
table(na_profits)
# The companies with missing values
Forbes2000[na_profits, c("name", "sales", "profits", "assets")]

table(complete.cases(Forbes2000))

# Only UK
UKcomp <- subset(Forbes2000, country == "United Kingdom")
UKcomp
dim(UKcomp) #137 cég van

summary(Forbes2000)
# Apply a function on each list elements
lapply(Forbes2000, summary)

# Median profits according to each categories (we have to remove the missing observations)
mprofits <- tapply(Forbes2000$profits, 
                   Forbes2000$category, median, na.rm = TRUE)
mprofits
median(Forbes2000$profits)
# Top 3 median profits
rev(sort(mprofits))[1:3]

median(Forbes2000$profits, na.rm = TRUE)

# Making a function for Interquartile Range
#iqr <- function(x, ...) {
   # q <- quantile(x, prob = c(0.25, 0.75), names = FALSE,...)
    #return(diff(q))
    #}
#iqr(Forbes2000$profits, na.rm = TRUE)

IQR(Forbes2000$profits, na.rm = TRUE) # q3-q1

# IQR profits for every category
iqr_profits <- tapply(Forbes2000$profits, Forbes2000$category, iqr, na.rm = TRUE)

# Minimum IQR profit category
levels(Forbes2000$category)[which.min(iqr_profits)]

which.min(iqr_profits)
levels(Forbes2000$category)[14]

# Maximum IQR profit category
levels(Forbes2000$category)[which.max(iqr_profits)]
levels(Forbes2000$category)

# Instead of "tapply", we use a for cycle to get the IQR profits
bcat <- Forbes2000$category
iqr_profits2 <- numeric(nlevels(bcat))
names(iqr_profits2) <- levels(bcat)
for (cat in levels(bcat)) {
  catprofit <- subset(Forbes2000, category == cat)$profit
  this_iqr <- iqr(catprofit, na.rm = TRUE)
  iqr_profits2[levels(bcat) == cat] <- this_iqr
}

layout(matrix(1:2, nrow = 2))
# The distribution is heavily right skewed -> we need log-transform to get a clearer view on the data
hist(Forbes2000$marketvalue)
hist(log(Forbes2000$marketvalue))

# Scatter plot(szórás grafikon,korrel?ci?t mutatja) -> dependent ~ independent 
# ?par
plot(log(marketvalue) ~ log(sales), data = Forbes2000, pch = ".")
# transparent color -> more information about the trend
plot(log(marketvalue) ~ log(sales), data = Forbes2000, col = rgb(0,0,0,0.1), pch = 16)
# https://chartio.com/learn/charts/what-is-a-scatter-plot/

with(Forbes2000,cor(log(marketvalue),log(sales))) #with(hol,milyen m?velet)

# Only the companies from the 4 countries
tmp <- subset(Forbes2000, country %in% c("United Kingdom",
                                       "Germany", "India", "Turkey"))

# Remove empty levels(drop=T)
tmp$country <- tmp$country[,drop = TRUE]
# If the independent variable is a factor -> boxplot
plot(log(marketvalue) ~ country, data = tmp, ylab = "log(marketvalue)", varwidth = TRUE)

hun=subset(Forbes2000, country == "Hungary")
hun

boxplot(log(Forbes2000$marketvalue))
#lines(rep(1,2),log(hun$marketvalue),col=2,t='p')
text(rep(1,2),log(hun$marketvalue),hun$name,col=2,t='p')
### Exercises

# 1 #
# Calculate the median profit for the companies in the US and the
# median profit for the companies in the UK, France and Germany.

USAp=subset(Forbes2000, country == "United States")
median(USAp$profits, na.rm = TRUE)
Kp=subset(Forbes2000, country %in% c("United Kingdom","France","Germany"))
median(Kp$profits, na.rm = TRUE)


# 2 #
# Find all German companies with negative profit.
germ=subset(Forbes2000, country == "Germany")
germ$name[which(germ$profits<0)]

# 3 # 
# To which business category do most of the Bermuda island companies belong?
levels(Forbes2000$country)
B=subset(Forbes2000, country == "Bermuda")
which.max(table(B$category))


# 4 # 
# Find the average value of sales for the companies in each country
# in the Forbes data set, and find the number of companies in each country
# with profits above 5 billion US dollars.
tapply(Forbes2000$sales,Forbes2000$country,mean)
tapply(Forbes2000$profits,Forbes2000$country,function(x)sum(x>5,na.rm=T))

?Forbes2000
?USmelanoma
