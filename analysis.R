library(dplyr)
library(tidyr)
library(corrplot)
library(ggplot2)
library(readr)
library(stringr)
library(corrplot)

res <- read_delim("survey_results.csv", delim =";") %>% 
  mutate(Program = str_extract(`Academic Program`, "(Data Science|Business Informatics|Statistic|Statistik und Wirtschaftsmathematik)"),
         Degree = str_extract(`Academic Program`, "[M/B][S/s]c"),
         University = str_extract(`Academic Program`, "(TU W[i\\I]en|University of Zagreb|Erasmus student)")) %>% 
  select(-`Academic Program`) %>% 
  select(Gender, Age, `Academic Program` = Program, Degree, University,
         `Social Media` = `Antwort 1`, `Time` = `Antwort 2`, `Activity` = `Antwort 3`) %>% 
  mutate(Age = as.numeric(Age))

res$Degree <- ifelse(is.na(res$Degree) & res$`Academic Program` == "Data Science", "MSc", res$Degree)
res$University <- ifelse(is.na(res$University) & res$`Academic Program` == "Data Science", "TU Wien", res$University)

H1 <- res %>% 
  filter(`Activity` == "Watching entertainment content") %>% 
  group_by(`Social Media`) %>% 
  summarize(`Average Time ` = mean(`Time`))

ggplot(H1, aes(x = `Social Media`, y = `Average Time `, fill = `Social Media`)) +
  geom_bar(stat = "identity") +
  labs(x = "Social Media Platform", y = "Average Time") +
  ggtitle("Average Time Spent on watching entertainment content") +
  theme_minimal()


H2 <- res %>% 
  filter(Age > 24) %>% 
  group_by(`Activity`) %>% 
  summarize(`Average Time ` = mean(`Time`))

print(H2)

ggplot(H2, aes(x = `Activity`, y = `Average Time `)) +
  geom_bar(stat = "identity") +
  labs(y = "Activity", x = "Average Time") +
  ggtitle("Average Time Spent spent by 24-year older boomers")

H2_A <- res %>%
  group_by(`Age Group` = ifelse(Age <= 24, "Younger 24", "Older 24"), `Activity`) %>%
  summarize(`Average Time ` = mean(`Time`))

print(H2_A)

ggplot(data = H2_A, aes(fill = `Age Group`, y = `Average Time `, x = `Activity`)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Activity", y = "Average Time", title = "Average Time by Age Group and Activity") +
  theme_minimal() 


H3 <- res %>% 
  group_by(`Academic Program`) %>%
  replace_na(list(`Academic Program` = "Others")) %>%
  summarize(`Average Time ` = mean(`Time`))

print(H3)

ggplot(data = H3, aes(x = `Academic Program`, y = `Average Time `, fill = `Academic Program`)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Academic Program", y = "Average Time", title = "Average Time by Academic Program") +
  theme_minimal() +
  theme(axis.text.x = element_blank())


H4 <- res %>% 
  group_by(Gender, Activity) %>%
  filter(Activity == "Chatting") %>% 
  summarize(`Average Time ` = mean(`Time`))

print(H4)


ggplot(data = H4, aes(x = `Gender`, y = `Average Time `)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Academic Program", y = "Average Time", title = "Average Time on Chatting by Gender ") +
  theme_minimal()



ggplot(res, aes(x = `Social Media`, y = Time, fill = Activity)) +
  geom_bar(stat = "identity", position="dodge") +
  # labs(x = "Answer 1", y = "Answer 2", fill = "Answer 3") +
  ggtitle("Activity Time per Social Media") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()

plot3 <- ggplot(res, aes(x = Time, fill = as.factor(`Social Media`))) +
  geom_density(alpha = 0.5) +
  ggtitle("Density of Time and Social Media") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Activities") +
  theme_minimal()

plot3

plot <- ggplot(res, aes(x = Age, fill = as.factor(Activity))) +
  geom_density(alpha = 0.5) +
  ggtitle("Density of activities and age") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Activities") +
  theme_minimal()

plot
