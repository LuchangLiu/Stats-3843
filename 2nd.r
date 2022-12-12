---
title: "Second Component SS3843 Final Project"
author: "Luchang Liu 251107314"
date: "2021/12/12"
output:
  word_document: default
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Q1

```{r}
# b. 
y = c(8.04, 6.61, 11.99, 7.78, 8.40, 
      9.58, 6.58, 6.66, 5.34, 7.92, 
      7.98, 7.98, 8.98, 7.94, 11.32, 
      9.74, 9.46, 9.14, 12.00, 9.32, 
      9.66, 11.28, 8.04, 8.12, 6.72)
yogurts = factor(c('M', 'H', 'N', 'Y', 'C', 
                  'N', 'Y', 'C', 'M', 'H', 
                  'Y', 'M', 'H', 'C', 'N', 
                  'H', 'C', 'M', 'N', 'Y', 
                  'C', 'N', 'Y', 'H', 'M'))   
batches = factor(c(rep(1:5, each = 5)))
positions = factor(c(rep(1:5, times = 5)))

d1 = data.frame(batches, positions, yogurts, y)

q1 = lm(y ~ yogurts + batches + positions)
resi1 = resid(q1)

qqnorm(resi1, col = "lightgreen", pch = 20, cex = 2)
qqline(resi1, col = "darkgreen", lwd = 3)

```

## Comment:
From the QQ-plot, majority points are very close to the line, which means the residuals follow a normal distribution. Therefore, normality assumption holds. 

\newpage
# Q2
```{r}
# e.
cool.zone = rep(c(5,10,15,20,25), times=6)
preheat = rep(c(5,25), each=15)

y = c(6.45, 2.51, 4.71, 11.47, 10.69,
      2.71, 4.42, 4.91, 9.31, 9.25,
      3.08, 4.20, 8.19, 12.04, 10.00,
      2.86, 5.43, 5.35, 8.92, 13.13,
      5.51, 8.37, 4.20, 7.57, 12.01,
      5.66, 3.54, 6.49, 7.14, 11.71)

d2 = data.frame(cool.zone, preheat, y)
q2 = lm(y ~ factor(cool.zone) * factor(preheat))
resi2 = resid(q2)

qqnorm(resi2, col = "lightgreen", pch = 20, cex = 2)
qqline(resi2, col = "darkgreen", lwd = 3)
```

## Comment
From the QQ-plot, points in left and right parts of the plot are very far from the line, which means the residuals do not follow a normal distribution. Therefore, normal assumption is violated.

\newpage
```{r}
# f.
library(ggplot2)
library(dplyr)
means = d2 %>% group_by(cool.zone,preheat) %>% summarise(Mean=mean(y))
ggplot(means, aes(x = cool.zone, y = Mean, colour = factor(preheat))) +
  geom_line(size=1) + geom_point(size=2) + 
  labs(colour="Preheat", y="Interaction") + 
  theme_bw()


```


## Comment
From the plot, we can see that both levels of preheat show an increasing trend with different slopes, which means there is a significant difference between preheat level and cool-zone level. Furthermore, from the plot we also can see that the larger cool-zone level is, the larger interaction will be. 

\newpage
```{r}
# g.
lm.q2 = lm(y ~ factor(cool.zone) + factor(preheat))
anova(lm.q2)

```

\newpage


# Q4

4(a)
Yijkl = mu + Ti + Bj + Ak + Yl + (TB)ij + (TA)ik + (TY)il + (BA)jk + (BY)jl + (AY)kl
        + (TBA)ijk + (TBY)ijl + (BAY)ikl + (TBAY)ijkl + Eijkl

Yijkl is the observed hardness of the gold fillings on ith dentist (our blocking variable), ith condensation filling method, kth
sintered temperature and lth glod alloy.

Ti is the effect of the ith dentist, i = 1,...,5

Bj is the effect of the jth method, j = 1,...,3

Ak is the effect of kth temperature, k = 1,...,3

Yl is the effect of lth alloy, l = 1, 2

Eijkl is the random error.

(TB)ij is the interaction between the ith dentist and jth method

(TA)ik is the interaction between the ith dentist and kth temperature

(TY)il is the interaction between the ith dentist and lth alloy

(BA)jk is the interaction between the jth method and kth temperature

(BY)jl is the interaction between the jth method and lth alloy

(AY)kl is the interaction between the kth temperature and lth alloy

(TBA)ijk is the interaction between the ith dentist + jth method + kth temperature

(TBY)ijl is the interaction between the ith dentist + jth method + lth alloy

(BAY)ikll is the interaction between the jth method + kth temperature + lth alloy

(TBAY)ijkl is the interaction between the ith dentist + jth method, kth temperature, and lth alloy

sum(Ti) = 0

sum(Bj) = 0

sum(Ak) = 0

sum(Yl) = 0

sum((TB)ij) = 0

sum((TA)ik) = 0

sum((TY)il) = 0

sum((BA)jk) = 0 

sum((BY)jl) = 0

sum((AY)kl) = 0

sum((TBA)ijk) = 0

sum((TBY)ijl) = 0

sum((BAY)ikl) = 0

sum((TBAY)ijkl) = 0

\newpage

4(b)
dfdentist = 5-1 = 4

dfmethod = 3-1 = 2

dfalloy = 2-1 = 1

dftemperature = 3-1 = 2

dfdentist, method = 4x2 = 8

dfdentist, temperature = 4x2 = 8

dfdentist, alloy = 4x1 = 4

dfmethod, temperature = 2x2 = 4

dfmethod, alloy = 2x1 = 2

dftemperature, alloy = 2x1 = 2

dfdentist, method, temperature = 4x2x2 = 16

dfdentist, method, alloy = 4x2x1 = 8

dfdentist, temperature, alloy = 4x2x1 = 8

dfmethod, temperature, alloy = 2x2x1 = 4

dfdentist, method, temperature and alloy = 4x2x2x1 = 16
dftotal = N-1 = 90-1 = 89
dferror = 0

\newpage

# Q5

```{r, error = TRUE}
#a 
y = c(41.2, 41.2, 39.8, 41.5, 41.9, 45.5,
      42.6, 41.4, 40.3, 43.0, 42.7, 44.7,
      135.7, 143.0, 132.4, 134.4, 137.4, 141.1,
      136.8, 143.3, 130.3, 130.0, 135.2, 139.1,
      163.2, 181.4, 173.6, 174.9, 166.6, 175.0,
      163.3, 180.3, 173.9, 175.6, 165.5, 172.0)
con = factor(rep(c(1:3), each=12))
day = factor(rep(c(1:3), each=2, times=6))
run = factor(rep(c(1:6), times=6))

library(lme4)

model = lmer(y ~ con + (1|day) + (1|day:run) + (1|con:day) + (1|con:day:run))

summary(model)
```


```{r, error = TRUE}  
#b
#Test 1: concentration effect
fullmodel1 = lmer(y ~ con + (1|day) + (1|day:run) + (1|con:day) + (1|con:day:run), REML = FALSE)
reducedmodel1 = lmer(y ~ 1 + (1|day) + (1|day:run) + (1|con:day) + 
                        (1|con:day:run), REML = FALSE)
#LRT
D1 = as.numeric(2*(logLik(fullmodel1)-logLik(reducedmodel1)))
pvalue1 = pchisq(D1, df=(3-1), lower.tail = FALSE) 
pvalue1
```

## Comment 
Because the p-value = 1.467227e-12 is smaller than 0.1,0.05,0.01, there is strong evidence to reject H0, so the concentration effect is not 0.

```{r}
#Test 2: day effect
fullmodel2 = lmer(y ~ con + (1|day) + (1|day:run) + (1|con:day) + (1|con:day:run))
reducedmodel2 = lmer(y ~ con + (1|day:run) + (1|con:day) + (1|con:day:run))
#LRT
D2 = as.numeric(2*(logLik(fullmodel2)-logLik(reducedmodel2)))
pvalue2 = pchisq(D2, df=(3-1), lower.tail = FALSE) 
pvalue2
```

## Comment
Because p_value = 1 is greater than 0.001,0.05,0.1, the data have very weak evidence against H0, so the day effect is 0.

```{r}
#Test 3: run effect
fullmodel3 = lmer(y ~ con + (1|day) + (1|day:run) + (1|con:day) + (1|con:day:run))
reducedmodel3 = lmer(y ~ con + (1|day) + (1|con:day) + (1|con:day:run))
# LRT
D3 = as.numeric(2*(logLik(fullmodel3)-logLik(reducedmodel3)))
pvalue3 = pchisq(D3, df=(2-1), lower.tail = FALSE) 
pvalue3
```

## Comment
Because p_value = 0.5267768 is greater than 0.001,0.05,0.1, the data have very weak evidence against H0, so run effect is 0.

```{r}
#Test 4: interaction effect between concentration and day
fullmodel4 = lmer(y ~ con + (1|day) + (1|day:run) + (1|con:day) + (1|con:day:run))
reducedmodel4 = lmer(y ~ con + (1|day) + (1|day:run) + (1|con:day:run))
# LRT
D4 = as.numeric(2*(logLik(fullmodel4)-logLik(reducedmodel4)))
pvalue4 = pchisq(D4, df=(3-1)*(3-1), lower.tail = FALSE) 
pvalue4
```

## Comment
Because p_value = 1 is greater than 0.001,0.05,0.1, the data have very weak evidence against H0, so no interaction effect between concentration and day.

```{r}
#Test 5: interaction effect between concentration day run
fullmodel5 = lmer(y ~ con + (1|day) + (1|day:run) + (1|con:day) + (1|con:day:run))
reducedmodel5 = lmer(y ~ con + (1|day) + (1|day:run) + (1|con:day))
# LRT
D5 <- as.numeric(2*(logLik(fullmodel5)-logLik(reducedmodel5)))
pvalue5 <- pchisq(D5, df=(3-1)*(3-1)*(2-1), lower.tail = FALSE) 
pvalue5
```

## Comment
Because p_value = 4.586637e-05 is smaller than 0.1,0.05,0.01, there is strong evidence to reject H0, so the interaction effect is not 0.















