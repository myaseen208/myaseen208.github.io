##----reg.df1----
# Fertilizer (Kg/acre)
# Production (Bushels/acre)
Fertilizer <- c(100, 200, 400, 500)
Production <- c( 70,  70,  80, 100)
df1        <- data.frame(Fertilizer, Production)
df1


##----reg.p1----
library(ggplot2)
# if (!require("ggplot2")) install.packages("ggplot2")
p1 <- 
  ggplot(data = df1, mapping = aes(x = Fertilizer, y = Production)) + 
  geom_point() +
  labs(x = "Fertilizer", y = "Production") +
  theme_bw()
print(p1)


##----reg.fm1----
fm1 <- lm(formula = Production ~ Fertilizer, data = df1)
summary(fm1)

##----reg.fm1anova----
anova(fm1)


##----reg.df2----
Fert  <- c(0.3, 0.6, 0.9, 1.2, 1.5, 1.8, 2.1, 2.4)
Yield <- c(10, 15, 30, 35, 25, 30, 50, 45)
df2   <- data.frame(Fert, Yield)
df2

##----reg.p2----
library(ggplot2)
# if (!require("ggplot2")) install.packages("ggplot2")
p2 <- 
  ggplot(data = df2, mapping = aes(x = Fert, y = Yield)) + 
  geom_point() +
  labs(x = "Fertilizer", y = "Yield") +
  theme_bw()
print(p2)

##----reg.fm2----
fm2 <- lm(formula = Yield ~ Fert, data = df2)
summary(fm2)

##----reg.fm2anova----
anova(fm2)

##----reg.df3----
# Weekly Income ($) of a Family
# Weekly Expenditures ($) of a Family
Income       <- seq(from = 80, to = 260, by = 20)
Expenditures <- c(70, 65, 90, 95, 110, 115, 120, 140, 155, 150)
df3          <- data.frame(Income, Expenditures)
df3

##----reg.p3----
library(ggplot2)
# if (!require("ggplot2")) install.packages("ggplot2")
p3 <- 
  ggplot(data = df3, mapping = aes(x = Income, y = Expenditures)) + 
  geom_point() +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::dollar) +
  labs(x = "Weekly income, $", y = "Weekly consumption expenditures, $") +
  theme_bw()
print(p3)

##----reg.fm3----
fm3 <- lm(formula = Expenditures ~ Income, data = df3)
summary(fm3)

##----reg.fm3anova----
anova(fm3)

##----reg.df4----
# Fertilizer (Kg)
# Rainfall (mm)
# Yield (Kg)
Fertilizer <- seq(from = 100, to = 700, by = 100)
Rainfall   <- c(10, 20, 10, 30, 20, 20, 30)
Yield      <- c(40, 50, 50, 70, 65, 65, 80)
df4        <- data.frame(Fertilizer, Rainfall, Yield)
df4

##----reg.p4----
library(car)
# if (!require("car")) install.packages("car")
car::scatter3d(formula = Yield ~ Fertilizer + Rainfall, data = df4)

##----reg.fm4----
fm4 <- lm(formula = Yield ~ Fertilizer + Rainfall, data = df4)
summary(fm4)

##----reg.fm4anova----
anova(fm4)

##----reg.fm4sbeta----
library(lm.beta)
# if (!require("lm.beta")) install.packages("lm.beta")
lm.beta(fm4)

##----reg.df5----
Nitrogen <- c(0, 0, 10, 10, 20, 20)
Yield    <- c(5, 7, 15, 17,  9, 11)
df5      <- data.frame(Nitrogen, Yield)
df5


##----reg.p5----
library(ggplot2)
# if (!require("ggplot2")) install.packages("ggplot2")
p5 <- 
  ggplot(data = df5, mapping = aes(x = Nitrogen, y = Yield)) + 
  geom_point() +
  labs(x = "Nitrogen", y = "Yield") +
  theme_bw()
print(p5)


##----reg.fm5----
fm5 <- lm(formula = Yield ~ Nitrogen + I(Nitrogen^2), data = df5)
# fm5 <- lm(formula = Yield ~ poly(x = Nitrogen, degree = 2, raw = TRUE), data = df5)
summary(fm5)

##----reg.fm5anova----
anova(fm5)


##----anova.df6----
Yield   <- c(5, 6, 7, 15, 16, 17)
Variety <- gl(n = 2, k = 3, length = 2*3, labels = c("V1", "V2"))
df6     <- data.frame(Yield, Variety)
df6

##----anova.p6----
library(ggplot2)
# if (!require("ggplot2")) install.packages("ggplot2")
p6 <- 
  ggplot(data = df6, mapping = aes(x = Variety, y = Yield)) + 
  geom_point() +
  geom_boxplot() +
  labs(x = "Variety", y = "Yield") +
  theme_bw()
print(p6)

##----anova.fm6----
fm6 <- lm(formula = Yield ~ Variety, data = df6)
summary(fm6)

##----anova.fm6anova----
anova(fm6)



##----anova.df7----
Yield   <- c(5, 7, 15, 17, 17, 19)
Variety <- gl(n = 3, k = 2, length = 3*2, labels = c("V1", "V2", "V3"))
df7     <- data.frame(Yield, Variety)
df7

##----anova.p7----
library(ggplot2)
# if (!require("ggplot2")) install.packages("ggplot2")
p7 <- 
  ggplot(data = df7, mapping = aes(x = Variety, y = Yield)) + 
  geom_point() +
  geom_boxplot() +
  labs(x = "Variety", y = "Yield") +
  theme_bw()
print(p7)

##----anova.fm7----
fm7 <- lm(formula = Yield ~ Variety, data = df7)
summary(fm7)

##----anova.fm7anova----
anova(fm7)


##----ancova.df8----
Yield    <- c(51, 52, 53, 54, 56, 57, 55, 56, 58, 59, 62, 63)
Fert     <- rep(x = c(80, 90, 100), each = 2)
Variety  <- gl(n = 2, k = 6, length = 2*6, labels = c("V1", "V2"), ordered = FALSE)
df8         <- data.frame(Yield, Fert, Variety)
df8


##----ancova.fm8----
fm8 <- lm(formula = Yield ~ Fert + Variety, data = df8)
summary(fm8)

##----ancova.fm8anova----
anova(fm8)


##----ancova.p8----
library(ggplot2)
# if (!require("ggplot2")) install.packages("ggplot2")
p8 <- 
  ggplot(data = df8, mapping = aes(x = Fert, y = Yield, color = Variety)) + 
  geom_point() +
  geom_line(mapping = aes(y = predict(fm8))) +
  labs(x = "Fert", y = "Yield") +
  theme_bw()
print(p8)


##----ancova.fm9----
fm9 <- lm(formula = Yield ~ Fert * Variety, data = df8)
summary(fm9)

##----ancova.fm9anova----
anova(fm9)


##----ancova.p9----
library(ggplot2)
# if (!require("ggplot2")) install.packages("ggplot2")
p9 <- 
  ggplot(data = df8, mapping = aes(x = Fert, y = Yield, color = Variety)) + 
  geom_point() +
  geom_line(mapping = aes(y = predict(fm9))) +
  labs(x = "Fert", y = "Yield") +
  theme_bw()
print(p9)


##----corr.df10----
WingLength  <- c(10.4, 10.8, 11.1, 10.2, 10.3, 10.2, 10.7, 10.5, 10.8, 11.2, 10.6, 11.4)
TailLength  <- c(7.4, 7.6, 7.9, 7.2, 7.4, 7.1, 7.4, 7.2, 7.8, 7.7, 7.8, 8.3)
df10        <- data.frame(WingLength, TailLength)
df10

##----corr.p10----
library(ggplot2)
# if (!require("ggplot2")) install.packages("ggplot2")
p10 <- 
  ggplot(data = df10, mapping = aes(x= WingLength, y = TailLength)) + 
  geom_point() +
  labs(x = "Sparrow Wing Length (cm)", y = "Sparrow Tail Length (cm)") +
  theme_bw()
print(p10)

##----corr.fm10----
cor(df10$WingLength, df10$TailLength)
cor.test(df10$WingLength, df10$TailLength)


##----corr.df11----
LeafArea    <- c(72, 174, 116, 78, 134, 95, 113, 98, 148, 42)
LeafMoist   <- c(75, 81, 83, 79, 81, 80, 81, 74, 78, 58)
TotShLength <- c(307, 529, 632, 527, 442, 525, 481, 710, 422, 345)
df11        <- data.frame(LeafArea, LeafMoist, TotShLength)
df11


##----corr.fm11----
library(ppcor)
# if (!require("ppcor")) install.packages("ppcor")
pcor(df11)



##----corr.fm12----
fm12 <- lm(formula = TotShLength ~ LeafArea + LeafMoist, data = df11)
sqrt(summary(fm12)$r.squared)

##----crd.df13----
ScabIndex <-
      c(
        12, 10,  24, 29, 
        30, 18,  32, 26, 
         9,  9,  16,  4,
        30,  7,  21,  9,
        16, 10,  18, 18,
        18, 24,  12, 19,
        10,  4,   4,  5,
        17,  7,  16, 17
      )
Trt  <- factor(
          x = rep(x = 1:7, times = c(8, 4, 4, 4, 4, 4, 4))
        , labels = c("O", "F3", "S3", "F6", "S6", "F9", "S9")
        )

df13 <- data.frame(Trt, ScabIndex)
df13

##----crd.p13----
library(ggplot2)
# if (!require("ggplot2")) install.packages("ggplot2")
p13 <- 
  ggplot(data = df13, mapping = aes(x = Trt, y = ScabIndex)) + 
  geom_point() +
  geom_boxplot() +
  labs(x = "Treatment", y = "Scab Index") +
  theme_bw()
print(p13)

##----crd.fm13----
fm13 <- lm(formula = ScabIndex ~ Trt, data = df13)
summary(fm13)

##----crd.fm13anova----
anova(fm13)


##----rcbd.df14----
Yield <- c(
             32.1,	35.6,	41.9,	35.4
          ,  30.1,	31.5,	37.1,	30.8
          ,  25.4,	27.4,	33.8,	31.1
          ,  24.1,	33.0,	35.6,	31.4
          ,  26.1,	31.0,	33.8,	31.9
          ,  23.2,	24.8,	26.7,	26.7
          )
SoilType <- gl(n = 4, k = 6, length = 4*6
               , labels = c("I", "II", "III", "IV"), ordered = FALSE)
Trt      <- gl(n = 6, k = 1, length = 4*6
               , labels = c("(NH4)2SO4", "NH4NO3", "CO(NH2)2", "Ca(NO3)2", "NaNO3", "Control"), ordered = FALSE)

df14 <- data.frame(Yield, SoilType, Trt)
df14

##----rcbd.p14----
library(ggplot2)
# if (!require("ggplot2")) install.packages("ggplot2")
p14 <- 
  ggplot(data = df14, mapping = aes(x = Trt, y = Yield)) + 
  geom_point() +
  geom_boxplot() +
  labs(x = "Treatment", y = "Yield") +
  theme_bw()
print(p14)

##----rcbd.fm14----
fm14 <- lm(formula = Yield ~ SoilType + Trt, data = df14)
summary(fm14)

##----rcbd.fm14anova----
anova(fm14)



##----latin.df15----
Yield <- c(
           257, 230, 279, 287, 202,
           245, 283, 245, 280, 260,
           182, 252, 280, 246, 250,
           203, 204, 227, 193, 259,
           231, 271, 266, 334, 338
           )
Row <- factor(x = rep(x = 1:5, each = 5))
Col <- factor(x = rep(x = 1:5, times = 5))
Trt <- c(
          "B", "E", "A", "C", "D"
        , "D", "A", "E", "B", "C"
        , "E", "B", "C", "D", "A"
        , "A", "C", "D", "E", "B"
        , "C", "D", "B", "A", "E"
         )

df15 <- data.frame(Yield, Row, Col, Trt)
df15

##----latin.p15----
library(ggplot2)
# if (!require("ggplot2")) install.packages("ggplot2")
p15 <- 
  ggplot(data = df15, mapping = aes(x = Trt, y = Yield)) + 
  geom_point() +
  geom_boxplot() +
  labs(x = "Treatment", y = "Yield") +
  theme_bw()
print(p15)

##----latin.fm15----
fm15 <- lm(formula = Yield ~ Row + Col + Trt, data = df15)
summary(fm15)

##----latin.fm15anova----
anova(fm15)

