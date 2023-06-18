library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)
library(hrbrthemes)
library(viridis)


res <- read_delim("survey_results.csv", delim =";") %>% 
  mutate(Program = str_extract(`Academic Program`, "(Data Science|Business Informatics|Statistic|Statistik und Wirtschaftsmathematik)"),
         Degree = str_extract(`Academic Program`, "[M/B][S/s]c"),
         University = str_extract(`Academic Program`, "(TU W[i\\I]en|University of Zagreb|Erasmus student)")) %>% 
  select(-`Academic Program`) %>% 
  select(Gender, Age, `Academic Program` = Program, Degree, University,
         `Social Media` = `Antwort 1`, `Time` = `Antwort 2`, `Activity` = `Antwort 3`)# %>%
  
res$Degree <- ifelse(is.na(res$Degree) & res$`Academic Program` == "Data Science", "MSc", res$Degree)
res$University <- ifelse(is.na(res$University) & res$`Academic Program` == "Data Science", "TU Wien", res$University)

res <- res %>%
  group_by(`Social Media`) %>%
  mutate(Count_Social_Media_Ppl = n())

str(res)

H1 <- res %>% 
  filter(`Activity` == "Watching entertainment content") %>% 
  group_by(`Social Media`) %>% 
  summarize(`Average Time ` = mean(`Time`))

ggplot(H1, aes(x = `Social Media`, y = `Average Time `)) +
  geom_bar(stat = "identity", fill = "darkgrey") +
  labs(x = "Social Media Platform", y = "Average Time") +
  ggtitle("Average Time Spent on watching entertainmnet content")

H2 <- res %>% 
  filter(Age > 24) %>% 
  group_by(`Activity`) %>% 
  summarize(`Average Time ` = mean(`Time`))

print(H2)

ggplot(H2, aes(x = `Activity`, y = `Average Time `)) +
  geom_bar(stat = "identity", fill = "darkgrey") +
  labs(y = "Activity", x = "Average Time") +
  ggtitle("Average Time Spent spent by 24-year older boomers")

H2_A <- res %>%
  group_by(`Age Group` = ifelse(Age <= 24, "Younger 24", "Older 24"), `Activity`) %>%
  summarize(`Average Time ` = mean(`Time`))

print(H2_A)

ggplot(data = H2_A, aes(fill = `Age Group`, y = `Average Time `, x = `Activity`)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Age Group", y = "Average Time", title = "Average Time by Age Group and Activity") +
  theme_minimal()


H3 <- res %>% 
  group_by(`Academic Program`) %>%
  replace_na(list(`Academic Program` = "Others")) %>%
  summarize(`Average Time ` = mean(`Time`))

print(H3)

ggplot(data = H3, aes(x = `Academic Program`, y = `Average Time `)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Academic Program", y = "Average Time", title = "Average Time by Academic Program") +
  theme_minimal()

#bubblechart
ggplot(res, aes(x = Time, y = Age, size = Count_Social_Media_ppl, fill = `Social Media`)) +
  geom_point(alpha=0.7)

# Most basic bubble plot
res %>%
  #arrange(desc(Time)) %>%
  #mutate(`Social Media` = factor("Social Media")) %>%
  ggplot(aes(x=Time, y=Age, size=Count_Social_Media_Ppl, fill=`Social Media`)) +
  geom_point(alpha=0.5, shape=21, color="black") +
  scale_size(range = c(.1, 24), name="Social Media Analysis") +
  #scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  theme_ipsum() +
  theme(legend.position="bottom") +
  ylab("Age") +
  xlab("Time") +
  theme(legend.position = "right")