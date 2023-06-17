library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)



res <- read_delim("survey_results.csv", delim =";") 

H1 <- res %>% 
  filter(`Antwort 3` == "Watching entertainment content") %>% 
  group_by(`Antwort 1`) %>% 
  summarize(avg_time = mean(`Antwort 2`))

ggplot(H1, aes(x = `Antwort 1`, y = avg_time)) +
  geom_bar(stat = "identity", fill = "darkgrey") +
  labs(x = "Social Media Platform", y = "Average Time") +
  ggtitle("Average Time Spent on watching entertainmnet content")

H2 <- res %>% 
  filter(Age > 24) %>% 
  group_by(`Antwort 3`) %>% 
  summarize(avg_time = mean(`Antwort 2`))

print(H2)

ggplot(H2, aes(x = `Antwort 3`, y = avg_time)) +
  geom_bar(stat = "identity", fill = "darkgrey") +
  labs(y = "Activity", x = "Average Time") +
  ggtitle("Average Time Spent spent by 24-year older boomers")

H2_A <- res %>%
  group_by(`Age Group` = ifelse(Age <= 24, "Younger 24", "Older 24"), `Antwort 3`) %>%
  summarize(avg_time = mean(`Antwort 2`))

print(H2_A)

ggplot(data = H2_A, aes(fill = `Age Group`, y = avg_time, x = `Antwort 3`)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Age Group", y = "Average Time", title = "Average Time by Age Group and Activity") +
  theme_minimal()