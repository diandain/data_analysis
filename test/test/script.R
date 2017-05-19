#import dataset
table <- read.csv('Z:/gongxiang/ign.csv', sep = ',', stringsAsFactors = FALSE)
names(table)[1] <-c('plot.id')
attach(table)

#try some basic statisitic
#extract avialiable columns in dataset
data <- data.frame(score, platform, genre)
head(data)
head(table[data])
#summary this columns
summary(table[data])
#caculate the density of score and show the graph
d<-density(table$score)
plot(d)
y<-dnorm(score)
#from summary information, the results show mean score is 6.95
#add the score mean line in graph
abline(v = 6.95, col='red')
b<-aggregate(score,platform,table)

install.packages('Hmisc')
library(Hmisc)
#describe the numerical variable score
describe(table$score)

install.packages('pastecs')
library('pastecs')
#describe the range of score
#show the number of values, null values, missing values, 
#min,max,range and sum.
stat.desc(table$score, norm = FALSE)


install.packages('psych')
library(psych)
#list the mean of score by game platform and genre
aggregate(table['score'],by=list(platform=table$platform),mean)
aggregate(table['score'], by = list(genre = table$genre), mean)
#apply the function() to score column by column platform and genre.
#show the summary result
mystats <- function(x, na.omit = FALSE) {
    if (na.omit)
        x <- x[!is.na(x)]
    m <- mean(x)
    n <- length(x)
    s <- sd(x)
    skew <- sum((x - m) ^ 3 / s ^ 3) / n
    kurt <- sum((x - m) ^ 4 / s ^ 4) / n - 3
    return(c(n = n, mean = m, stdev = s, skew = skew, kurtosis = kurt))
}
ds<-function(x) sapply(x,mystats)
by(table['score'], table$platform, ds)
by(table['score'], table$genre, ds)
describeBy(table['score'],list(playform=platform))
describeBy(table['score'], list(genre = genre))

install.packages('doBy')
library(doBy)
#grouping variable and use function defined earlier
summaryBy(score ~ platform, data = table, FUN = mystats)
summaryBy(score ~ genre, data = table, FUN = mystats)
# the simple frequency count score level by score_phrase
mytable <- with(table, table(score_phrase))
mytable
#expressed as percentages
prop.table(mytable)*100
#show the frequency of platform/genre and the score level--score_phrase
mytable <- xtabs(~platform + score_phrase, data = table)
mytable1 <- xtabs(~genre + score_phrase, data = table)
mytable
mytable1


#correlations between score and platform,variance comparision
#statistic the mean by platform
#statistic the sd by platform
aggregate(score, by = list(platform), FUN = mean)
aggregate(score, by = list(platform), FUN = sd)
fit = aov(score ~ genre, data = table)
summary(fit)
#perform a multiple regression
levels(factor(platform))
levels(factor(score))
reg = lm(score ~ platform, data = table)
summary(reg) 
lm(formula = score ~ platform, data = table)
plot(reg)
#part of the coefficient of determination R^2=0.0508
#One interpretation of this is that our model explains 68% of variance in conversation duration. 
#Another interpretation of R^2 is that by square-rooting, we can find the multiple correlation coefficent R.
rsq<-summary(reg)$r.squared
rsq
#0.2255 isn't the correlation between game score and game platform
sqrt(rsq)
#the fitted values are the mean durations for each group
print(reg$fitted)
#check the Pearson correlation between observed and fitted value
cor(score, reg$fitted)
plot(x = reg$fitted, y = score, xlab = 'Fitted score', ylab = 'Observed score')
abline(lm(score ~ reg$fitted), col = 'red')

#Method 2---one-way ANOVA, ANOVA model fits the group means just as the regression did
install.packages('heplots')
library(heplots)
model.aov<-aov(score~platform,data=table)
summary(model.aov)
print(model.aov$fitted.values)
etasq(model.aov,partial=FALSE)
etasq(model.aov, partial = TRUE)



#score and genre
reg2 <- lm(score ~ genre, data = table)
reg2
summary(reg2)
lm(formula = score ~ genre, data = table)
plot(reg2)
reg2 = lm(score ~ genre, data = table)
summary(reg2)
lm(formula = score ~ genre, data = table)
plot(reg)
rsq2 <- summary(reg2)$r.squared
rsq2
sqrt(rsq2)
#the fitted values are the mean durations for each group
print(reg2$fitted)
#check the Pearson correlation between observed and fitted value
cor(score, reg2$fitted)
plot(x = reg2$fitted, y = score, xlab = 'Fitted score', ylab = 'Observed score')
abline(lm(score ~ reg2$fitted), col = 'red')
#do the same statistic to score and genre
aggregate(score, by = list(genre), FUN = mean)
aggregate(score, by = list(genre), FUN = sd)
fit2 = aov(score ~ genre, data = table)
summary(fit2)





#try ggplot()
boxplot(score~platform,data=table)
install.packages('ggplot2')
library('ggplot2')
ggplot(aver_score, aes(factor(aver_score$platform)), fill = aver_score$score) + geom_bar()
a <- table(genre)
b <- sort(subset(a, a > 1000))
array <- b[1:5]

ggplot(table, aes(release_year, fill = factor(genre))) + geom_bar(stat = 'count', colour = 'black')
+ labs(title = 'The Number of Game Genre in 20 Years', x = 'release_year', y = 'genre')
ggplot(table, aes(release_year, fill = factor(platform))) + geom_bar(stat = 'count') + labs(title = 'The Number of Game Platform in 20 Years', x = 'release_year', y = 'platform')






#normal distribution
x <- score
y <- dnorm(x, mean(score), sd(score))
ggplot(data = NULL, aes(x, y)) + geom_line(color = 'blue')
hist(x)
z<-table(score)
summary(z)


#multiple linear regression
library(car)
scatterplotMatrix(~score+factor(platform)+factor(genre),data=table,spread=FALSE,lty.smooth=2,main='scatter plot matrix')

#caculate a sample size
install.packages('pwr')
library('pwr')
esize <- seq(0.1, 0.5, 0.01)
esize
nes <- length(esize)
samplesize <- NULL
for (i in 1:nes) {
    result <- pwr.anova.test(k = 5, f = esize[i], sig.level = 0.05, power = 0.9)
    samplesize[i]<-ceiling(result$n)
}

plot(samplesize, esize, type = "l", lwd = 2, col = "black", ylab = 'effect size', xlab = 'sample size', maintainer = ' one way anova with power=0.90 and alpha= 0.05')

#calculate the effect size by putting n=200 into pwr.anova.test
pwr.anova.test(k = 5, n = 200, sig.level = 0.05, power = 0.9)
#add x=200,y=0.1244102 to the plot
abline(v=200,h=0.1244102,col='red')

#two-sample t test
pwr.t.test(n = 200,d = ,sig.level = 0.05,power = 0.9,type = 'two.sample',alternative = 'two.sided')

#extract random data n=200
data<-data.frame(score,platform,genre)
sampledata <- data[sample(nrow(data), 200),]
head(sampledata)

#test the correlation between each groups
install.packages('stats')
library(stats)
myda<-xtabs(sampledata$score~sampledata$platform,data = sampledata)
chisq.test(myda)

myda1 <- xtabs(sampledata$score ~ sampledata$genre, data = sampledata)
chisq.test(myda1)



#try t.test()
#can use for numerical variable
install.packages('car')
library(car)
g1 <- sampledata$score
g2 <- factor(sampledata$platform)
g3 <- sampledata$genre
t.test()
p_value = function(x, y, z) {
    x = data.frame(x)
    y = data.frame(y)
    c = nrow(x)
    d = nrow(y)
    colnames(x) = 'variable'
    colnames(y) = 'variable'
    a = data.frame(rep('A', c))
    b = data.frame(rep('B', d))
    colnames(a) = 'group'
    colnames(b) = 'group'
    data = data.frame(variable.names = rbind(x, y), group = rbind(a, b))
    if (leveneTest(variable ~ group, data)$Pr[1] == 'NaN') {
        print(NA)
    } else {
        if (leveneTest(variable ~ group, data)$Pr[1] > 0.05) {
            t.test(x, y, paired = z)$p.value
        } else {
            wilcox.test(data[1:c, 1], data[(c + 1):(c + d), 1], paired = z)$p.value
        }
    }
}
p_value(g1, g2, TRUE)





#time series by year
mean_score <- tapply(score, release_year, mean)
summary(mean_score)
plot(mean_score)
b <- as.data.frame(mean_score)
time <- ts(mean_score, frequency = 1, start = c(1970), end = (2016))
plot.ts(time)
install.packages('forecast')
library('forecast')


#prodict the futher score
scoreforecasts <- HoltWinters(mean_score, beta = FALSE, gamma = FALSE)
scoreforecasts
scoreforecasts$fitted
plot(scoreforecasts)

#create a new dataframe about the mean score, sd score, number of release game and the mean release day
b$sd_score <- tapply(score, release_year, sd)
b$num_game <- table(release_year)
b$day <- tapply(release_day, release_year, mean)
game_year <- b
plot(game_year)
summary(game_year)
plot(game_year)
#predict the game release day by time series
time_gameday <- ts(b$day, frequency = 1, start = c(1970), end = c(2016))
plot(time_gameday)





