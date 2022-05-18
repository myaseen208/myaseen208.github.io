##----Data----
library(readxl)
library(tidyverse)
Data <-
  read_excel(path = "NHANES1.xlsx") |>
  mutate(
    Gender  = fct_relevel(Gender, "Male", "Female")
  , BMI.Cat = fct_relevel(BMI.Cat, "Under Weight", "Normal Weight", "Over Weight", "Obese")
    )
Data


##----Out1----
library(janitor)
Out1 <-
  Data |>
  tabyl(Gender) |>
  adorn_totals(c("row")) |>
  adorn_pct_formatting(digits = 2)

Out1

##----Plot1----
library(scales)
Plot1 <-
  ggplot(data = Data, mapping = aes(x = Gender)) +
  geom_bar(stat = "count", width = 0.4) +
  geom_text(stat = "count", mapping = aes(label = comma(stat(count))), vjust = -0.2) +
  scale_y_continuous(
      expand = c(0, NA)
    , limits = c(0, 4700)
    , labels = scales::comma
    , breaks = pretty_breaks(12)
    ) +
  labs(
    x = "Gender"
  , y = "Frequency"
  ) +
  theme_classic() +
  theme(aspect.ratio = 2/1)

Plot1

##----Out2----
Out2 <-
  Data |>
  tabyl(Gender)
Out2

##----Plot2----
library(ggchicklet)
Plot2 <-
  ggplot(data = Out2, mapping = aes(x = Gender, y = n)) +
  geom_chicklet(radius = grid::unit(3, "mm"), width = 0.5) +
  geom_text(mapping = aes(label = scales::comma(n)), vjust = -0.2) +
  scale_y_continuous(
      expand = c(0, NA)
    , limits = c(0, 4700)
    , labels = scales::comma
    , breaks = pretty_breaks(12)
    ) +
  labs(
    x = "Gender"
  , y = "Frequency"
  ) +
  theme_classic() +
  theme(aspect.ratio = 2/1)

Plot2

##----Out3----
Out3 <-
  Data |>
  tabyl(Race) |>
  adorn_totals(c("row")) |>
  adorn_pct_formatting(digits = 2)
Out3

##----Plot3----
Plot3 <-
  ggplot(data = Data, mapping = aes(x = Race)) +
  geom_bar(stat = "count") +
  geom_text(stat = "count", mapping = aes(label = comma(stat(count))), vjust = -0.2) +
  scale_y_continuous(
      expand = c(0, NA)
    , limits = c(0, 3500)
    , labels = scales::comma
    , breaks = pretty_breaks(12)
    ) +
  labs(
    x = "Race"
  , y = "Frequency"
  ) +
  theme_classic()

Plot3

##----Out4----
Out4 <-
  Data |>
  tabyl(BMI.Cat) |>
  adorn_totals(c("row")) |>
  adorn_pct_formatting(digits = 2)

Out4

##----Plot4----
Plot4 <-
  ggplot(data = Data, mapping = aes(x = BMI.Cat)) +
  geom_bar(stat = "count") +
  geom_text(stat = "count", mapping = aes(label = comma(stat(count))), vjust = -0.2) +
  scale_y_continuous(
      expand = c(0, NA)
    , limits = c(0, 3000)
    , labels = scales::comma
    , breaks = pretty_breaks(12)
    ) +
  labs(
    x = "BMI"
  , y = "Frequency"
  ) +
  theme_classic()

Plot4

##----Out5----
Out5 <-
  Data |>
  tabyl(Gender, BMI.Cat) |>
  adorn_totals(c("row", "col")) |>
  adorn_percentages("row") |>
  adorn_pct_formatting(digits = 2) |>
  adorn_ns(position = "front") |>
  adorn_title("combined")

Out5

##----Plot5----
Plot5 <-
  ggplot(data = Data, mapping = aes(x = BMI.Cat, fill = Gender)) +
  geom_bar(stat = "count") +
  geom_text(stat = "count", mapping = aes(label = comma(stat(count))), vjust = -0.2) +
  facet_grid(cols = vars(Gender)) +
  scale_y_continuous(
      expand = c(0, NA)
    , limits = c(0, 1600)
    , labels = scales::comma
    , breaks = pretty_breaks(12)
    ) +
  labs(
    x = "BMI"
  , y = "Frequency"
  ) +
  theme_classic()

Plot5

##----Plot6----
Plot6 <-
  ggplot(data = Data, mapping = aes(y = BMI)) +
  geom_boxplot(stat = "boxplot", outlier.colour = "red") +
  scale_x_discrete() +
  scale_y_continuous(
      expand = c(0, NA)
    , limits = c(0, NA)
    , labels = scales::comma
    , breaks = pretty_breaks(12)
    ) +
  labs(
    x = ""
  , y = "BMI"
  ) +
  theme_classic()

Plot6

##----Plot7----
Plot7 <-
  ggplot(data = Data, mapping = aes(y = BMI)) +
  geom_boxplot(stat = "boxplot", outlier.colour = "red") +
  facet_grid(cols = vars(Gender)) +
  scale_x_discrete() +
  scale_y_continuous(
      expand = c(0, NA)
    , limits = c(0, NA)
    , labels = scales::comma
    , breaks = pretty_breaks(12)
    ) +
  labs(
    x = ""
  , y = "BMI"
  ) +
  theme_classic()

Plot7

##----Out6----
library(moments)
Out6 <-
  Data |>
  summarise(
    n        = length(BMI)
  , Mean     = mean(BMI)
  , Variance = var(BMI)
  , SD       = sd(BMI)
  , SK       = skewness(BMI)
  , K        = kurtosis(BMI)
  )

Out6

##----Out7----
Out7 <-
  Data |>
  filter(BMI < 70) |>
  summarise(
    n        = length(BMI)
  , Mean     = mean(BMI)
  , Variance = var(BMI)
  , SD       = sd(BMI)
  , SK       = skewness(BMI)
  , K        = kurtosis(BMI)
  )
Out7

##----Out8----
Out8 <-
  Data |>
  group_by(Gender) |>
    summarise(
    n        = length(BMI)
  , Mean     = mean(BMI)
  , Variance = var(BMI)
  , SD       = sd(BMI)
  , SK       = skewness(BMI)
  , K        = kurtosis(BMI)
  )
Out8

##----Out9----
Out9 <-
  Data |>
  filter(BMI < 70) |>
  group_by(Gender) |>
    summarise(
    n        = length(BMI)
  , Mean     = mean(BMI)
  , Variance = var(BMI)
  , SD       = sd(BMI)
  , SK       = skewness(BMI)
  , K        = kurtosis(BMI)
  )

Out9



##----fm1Plot1----
fm1Plot1 <-
  ggplot(data = Data, mapping = aes(x = Age, y = BMI)) +
  geom_point() +
  theme_classic()

fm1Plot1

##----fm1----
fm1 <- lm(formula = BMI ~ Age, data = Data)
summary(fm1)$coef
anova(fm1)

##----fm1Plot2----
library(emmeans)
fm1Plot2 <-
  emmip(
      object     = fm1
    , formula    =  ~ Age
    , cov.reduce = range
    ) +
  scale_x_continuous(breaks = pretty_breaks(12)) +
  scale_y_continuous(breaks = pretty_breaks(12)) +
  labs(x = "Age", y = "BMI") +
  theme_classic()
fm1Plot2

##----fm1Plot3----
library(ggpmisc)
fm1Plot3 <-
  ggplot(data = Data, mapping = aes(x = Age, y = BMI)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black", formula = y ~ x) +
  stat_poly_eq(
      formula = y ~ x
    , eq.with.lhs = FALSE
    , mapping = aes(
                   label = paste("widehat(italic(BMI))", "~`=`~", gsub("italic(x)","Age",..eq.label..,fixed = TRUE),  "~~~", ..rr.label..,  "~~~",  ..adj.rr.label.., "~~~",  ..p.value.label.. ,  sep = "")
                  )
    , size  = 4.5
    , parse = TRUE
    ) +
  theme_classic()

fm1Plot3

##----fm2----
fm2 <- lm(formula = BMI ~ Gender, data = Data)
anova(fm2)

##----fm2Plot1----
fm2Plot1 <-
  emmip(
      object     = fm2
    , formula    =  ~ Gender
    ) +
  scale_y_continuous(breaks = pretty_breaks(12)) +
  labs(x = "Gender", y = "BMI") +
  theme_classic()
fm2Plot1

##----fm3----
fm3 <- lm(formula = BMI ~ Age*Gender, data = Data)
anova(fm3)
summary(fm3)$coef

##----fm3Plot1----
fm3Plot1 <-
  emmip(
      object     = fm3
    , formula    = Gender ~ Age
    , cov.reduce = range
    ) +
  scale_x_continuous(breaks = pretty_breaks(12)) +
  scale_y_continuous(breaks = pretty_breaks(12)) +
  labs(x = "Age", y = "BMI") +
  theme_classic()
fm3Plot1

##----Out10----
Out10 <-
  Data |>
  select(BMI, Age) |>
  var()

Out10

##----Out11----
Out11 <-
  Data |>
  select(BMI, Age) |>
  cor()
Out11

##----Out12----
Out12 <-
  Data |>
  filter(BMI < 70) |>
  select(BMI, Age) |>
  var()
Out12

##----Out13----
Out13 <-
  Data |>
  filter(BMI < 70) |>
  select(BMI, Age) |>
  cor()
Out13


##----Out14----
Data |>
  filter(Gender == "Male") |>
  select(BMI, Age) |>
  var()

Data |>
  filter(Gender == "Male") |>
  select(BMI, Age) |>
  cor()

Data |>
  filter(Gender == "Female") |>
  select(BMI, Age) |>
  var()


Data |>
  filter(Gender == "Female") |>
  select(BMI, Age) |>
  cor()
