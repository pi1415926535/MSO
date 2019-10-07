library(magrittr)
library(dplyr)

data <- read.csv2("../data/daneSoc.csv")

# head(data)
# nrow(data)
# ncol(data)
# colnames(data)
# summary(data)

str(data)
class(data[["wiek"]])

data %$% table(wyksztalcenie, praca)

table(data$wyksztalcenie, data$praca)

data %>%
  filter(wyksztalcenie == "srednie", plec == "mezczyzna") %$%
  summary(cisnienie.skurczowe)

data %>%
  filter(plec == "mezczyzna") %$%
  boxplot(cisnienie.skurczowe~praca)

data %>%
  filter(wyksztalcenie == "srednie", cisnienie.skurczowe>140, cisnienie.skurczowe<150)

data %>%
  filter(cisnienie.skurczowe == max(cisnienie.skurczowe))

data %>%
  filter(cisnienie.skurczowe > quantile(cisnienie.skurczowe, 0.8))


#zad2
library(car)

n<-10
qqnorm(rnorm(n))
abline(0,1)
qqnorm(rgamma(n,2,2))
abline(0,1)
qqnorm(rcauchy(n))
abline(0,1)
qqPlot(rnorm(n))
qqPlot(rgamma(n,2,2))
qqPlot(rcauchy(n))

n<-100
qqnorm(rnorm(n))
abline(0,1)
qqnorm(rgamma(n,2,2))
abline(0,1)
qqnorm(rcauchy(n))
abline(0,1)

n<-1000
qqnorm(rnorm(n))
abline(0,1)
qqnorm(rgamma(n,2,2))
abline(0,1)
qqnorm(rcauchy(n))
abline(0,1)


#zad3
n <- 100
betas <- c(0.5,1,0.5,0.75)
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
eps <- rnorm(n)
y <- betas[1] + betas[2]*x1 + betas[3]*x2 + betas[4]*x3 + eps

x <- cbind(1,x1,x2,x3)

QR <- qr(x)
Q <- qr.Q(QR)
R <- qr.R(QR)

solve(t(x)%*%x,t(x)%*%y)
solve(R,t(Q)%*%y)

m1 <- lm(y~x1+x2+x3)
m1$coef
summary(m1)
