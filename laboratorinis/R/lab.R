Sys.setenv(LANG = "lt_LT.UTF-8")
duom = read.csv("../data/lab_sodra.csv")
duom
library(tidyverse)
library(dplyr)
#1
data = duom %>%
  filter(ecoActCode == 682000)
summary(data)
data %>%
  ggplot(aes(x=avgWage)) +
  theme_minimal() +
  geom_histogram(fill = "yellow", col = "black", bins = 150) +
  labs(title = "Vidutine darbotoju alga", x="Darbuotoju kiekis", y="Algos dydis")

#2
top5 = data %>%
  group_by(name) %>%
  summarise(wage = max(avgWage)) %>%
  arrange(desc(wage)) %>%
  head(5)

top5full = data %>% filter(name %in% top5$name)

top5full %>%
  ggplot(aes(x = month, y = avgWage, group = name)) +
  geom_line(aes(colour = name))+
  labs(title = "Top 5 kompanijos su didziausiu vidutiniu atlyginimu", x = "Menuo", y = "Vidutine alga")

#3

top5full %>%
  group_by(name) %>%
  slice_max(numInsured, with_ties = FALSE) %>%
  head(10) %>%
  ggplot(aes(x = reorder(name, -numInsured), y = numInsured)) +
  geom_col(aes(fill = name)) +
  theme(axis.text.x = element_blank()) +
  theme_minimal() +
  labs(title = "Apdrausti darboutojai", x = "Imone", y = "Apdraustuju skaicius")


