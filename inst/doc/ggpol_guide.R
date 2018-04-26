## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(ggplot2)
library(ggpol)

## ------------------------------------------------------------------------
bt <- data.frame(
  parties = factor(c("CDU", "CSU", "AfD", "FDP", "SPD",
                     "Linke", "Gruene", "Fraktionslos"),
                   levels = c("CDU", "CSU", "AfD", "FDP", "SPD",
                              "Linke", "Gruene", "Fraktionslos")),
  seats   = c(200, 46, 92, 80, 153, 69, 67, 2),
  colors  = c("black", "blue", "lightblue", "yellow", "red",
              "purple", "green", "grey"),
  stringsAsFactors = FALSE)

ggplot(bt) + 
  geom_arcbar(aes(shares = seats, r0 = 5, r1 = 10, fill = parties), 
               sep = 0.1) + 
  scale_fill_manual(values = bt$colors) +
  coord_fixed() +
  theme_void()

## ------------------------------------------------------------------------
bt <- data.frame(
 parties = factor(c("CDU", "CSU", "AfD", "FDP", "SPD", 
                    "Linke", "Gruene", "Fraktionslos"),
                  levels = c("CDU", "CSU", "AfD", "FDP", "SPD", 
                             "Linke", "Gruene", "Fraktionslos")),
 seats   = c(200, 46, 92, 80, 153, 69, 67, 2),
 colors  = c("black", "blue", "lightblue", "yellow", 
             "red","purple", "green", "grey"),
 stringsAsFactors = FALSE)

ggplot(bt) + 
  geom_parliament(aes(seats = seats, fill = parties), color = "black") + 
  scale_fill_manual(values = bt$colors, labels = bt$parties) +
  coord_fixed() + 
  theme_void()

## ------------------------------------------------------------------------
df <- data.frame(x = sample(1:10, 3), y = sample(1:10, 3),
                 r = sample(3:4, 3, replace = TRUE))
 
ggplot(df) + geom_circle(aes(x = x, y = y, r = r, fill = gl(3, 1))) +
  coord_fixed()

## ------------------------------------------------------------------------
ggplot(economics, aes(x = date, y = unemploy)) +
  geom_line() +
  geom_tshighlight(aes(xmin = as.Date("01/01/1990", format = "%d/%m/%Y"), 
                       xmax = as.Date("01/01/2000", format = "%d/%m/%Y")),
                   alpha = 0.01)

## ------------------------------------------------------------------------
df <- data.frame(score = rgamma(150, 4, 1), 
                 gender = sample(c("M", "F"), 150, replace = TRUE), 
                 genotype = factor(sample(1:3, 150, replace = TRUE)))

ggplot(df) + 
  geom_boxjitter(aes(x = genotype, y = score, fill = gender),
                 jitter.shape = 21, jitter.color = NA, 
                 jitter.height = 0, jitter.width = 0.04,
                 outlier.color = NA, errorbar.draw = TRUE) +
  scale_fill_manual(values = c("#ecb21e", "#812e91")) +
  theme_minimal()

## ------------------------------------------------------------------------
ggplot(df) + 
  geom_boxjitter(aes(x = genotype, y = score, fill = gender),
                 jitter.shape = 21, jitter.color = NA, 
                 jitter.height = 0, jitter.width = 0.04,
                 outlier.color = "black", errorbar.draw = TRUE,
                 outlier.intersect = TRUE, outlier.shape = 24,
                 outlier.size = 1.5) +
  scale_fill_manual(values = c("#ecb21e", "#812e91")) +
  theme_minimal()

## ------------------------------------------------------------------------
ggplot(df) + 
  geom_boxjitter(aes(x = genotype, y = score, fill = gender),
                 errorbar.draw = TRUE, boxplot.expand = TRUE,
                 errorbar.length = 0.4) +
  scale_fill_manual(values = c("#ecb21e", "#812e91")) +
  theme_minimal()

## ------------------------------------------------------------------------
df <- data.frame(sex = sample(c("M", "F"), 1000, replace = TRUE),
                 age = rnorm(1000, 45, 12))

df$age_bins <- cut(df$age, 15)
df$count <- 1
df <- aggregate(count ~ sex + age_bins, data = df, length)


df_h <- df
df_h$count <- ifelse(df_h$sex == "F", df_h$count * -1, df_h$count)

ggplot(df_h, aes(x = age_bins, y = count, fill = sex)) + 
  geom_bar(stat = "identity") +
  facet_share(~sex, dir = "h", scales = "free", reverse_num = TRUE) + 
  coord_flip()

## ------------------------------------------------------------------------
# When setting direction to vertical, and if we want to mirror the second panel,
# we must multiply the second factor by -1.
# And levels(factor(gender))[2] is M. 
df_v <- df
df_v$count <- ifelse(df_v$sex == "M", df_v$count * -1, df_v$count)
ggplot(df_v, aes(x = age_bins, y = count, fill = sex)) + 
  geom_bar(stat = "identity") +
  facet_share(~sex, dir = "v", scales = "free", reverse_num = TRUE)

