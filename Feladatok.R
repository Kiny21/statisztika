### Exercises --lab2

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


#lab3
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