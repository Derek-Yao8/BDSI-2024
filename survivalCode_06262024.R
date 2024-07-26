library(dplyr)
library(ggplot2)
library(forcats)
# library(DIMPLE)
# line for installing MItools; only need to run once
# devtools::install_github("junsoukchoi/MItools", force = TRUE)
library(spatstat.explore)
library(spatstat.geom)
library(MItools)

ovarianData <- readRDS(
  "/Users/derekyao/Documents/BDSI/Cancer Data Science/Data/ovarianData.RDS"
)
lungData <- readRDS(
  "/Users/derekyao/Documents/BDSI/Cancer Data Science/Data/lungData.RDS"
)

alternativeData <- data.frame(obs = 1:100, death = c(rep(1, 6), rep(0, 94)))
alternativeData$x = c(1, 10, 10, 15, 20, 20, 10, rep(12, 5), rep(24, 88))

alternativeData
# here i present alternative data. I will calculate KM estimate by hand,
# then using the "survival" package, and finally fit a Cox PH model separately

alternativeData_grouped <- alternativeData %>% 
  group_by(x) %>% 
  summarize(d = sum(death), c = sum(1-death)) %>% 
  mutate(n = nrow(alternativeData) - c(0 , head(cumsum(d+c) , 5)))
# put data in grouped form with n at risk
alternativeData_grouped

alternativeData_grouped$kmProd <- 1 - alternativeData_grouped$d/alternativeData_grouped$n
alternativeData_grouped$kmEst <- cumprod(alternativeData_grouped$kmProd)
alternativeData_grouped <- rbind(
  c(0,0,0,100,1,1), alternativeData_grouped
)

alternativeData_grouped
ggplot(alternativeData_grouped , aes(x = x, y = kmEst)) +
  geom_step() +
  geom_vline(
    data = alternativeData_grouped %>% filter(d > 0), aes(xintercept = x), lty = 2
  ) +
  theme_classic() +
  labs(x = "Time", y = "K-M Estimate of Survival", 
       title = "Kaplan-Meier Estimator for Survival", 
       caption = "Dotted lines represent death times")
library(survival)

kmObj <- survfit(
  Surv(time = x, event = death) ~ 1, data = alternativeData
)
plot(kmObj, ylim = c(0.9,1))
summary(kmObj)$surv
alternativeData_grouped$kmEst[alternativeData_grouped$d != 0]
# i can do math yay

n <- 1000
set.seed(1)
trtGroup <- sample(1:n, size = round(n/2), replace = F)
trt <- rep("A", n)
trt[trtGroup] <- "B"
table(trt)
t <- rexp(n = n, rate = ifelse(
  trt == "A", 1/10, 1/20
))
cens <- rexp(n = n, rate = 1/20)

d <- ifelse(t < cens, 1, 0)

x <- pmin(t,cens)

summary(coxph(Surv(x , d) ~ trt))
exp(confint(coxph(Surv(x , d) ~ trt)))
# treatment B has hazard of death 0.47 times the hazard of treatment A
# makes sense, because we generated survival times for treatment B from an exponential
# distribution with rate 1/2 the rate of treatment A

