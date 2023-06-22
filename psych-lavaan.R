library(psych)
library(lavaan)
library(corrplot)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)

df <- read_delim("survey_results.csv", delim =";") %>% 
  mutate(Program = str_extract(`Academic Program`, "(Data Science|Business Informatics|Statistic|Statistik und Wirtschaftsmathematik)"),
         Degree = str_extract(`Academic Program`, "[M/B][S/s]c"),
         University = str_extract(`Academic Program`, "(TU W[i\\I]en|University of Zagreb|Erasmus student)")) %>% 
  select(-`Academic Program`) %>% 
  select(Gender, Age, `Academic Program` = Program, Degree, University,
         `SocialMedia` = `Antwort 1`, `Time` = `Antwort 2`, `Activity` = `Antwort 3`)

df$Degree <- ifelse(is.na(df$Degree) & df$`Academic Program` == "Data Science", "MSc", df$Degree)
df$University <- ifelse(is.na(df$University) & df$`Academic Program` == "Data Science", "TU Wien", df$University)
  `
test <- df %>%
  mutate_all(~as.numeric(factor(.)))
corrplot(cor(test))