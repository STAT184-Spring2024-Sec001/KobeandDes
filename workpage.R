data <- read.csv("/Users/ray/Downloads/Sleep_health_and_lifestyle_dataset.csv")
library(janitor)
library(dplyr)
library(kableExtra)
library(ggplot2)

#esquisse
library(esquisse)
#esquisse::esquisser(data = [insert dataset], viewer = "browser")

#Table of Dataset Column (Attribute) names
col_names <- colnames(data)
kable(col_names, row.names = TRUE, col.names= 'ATTRIBUTES')%>%
  kable_styling(full_width = F)

#Average Sleep Duration per Occupation Table 
avgSleepDuration <- data %>%
  group_by(Occupation)%>%
  summarize(avgSleepDuration = mean(Sleep.Duration))

#Average Sleep Duration for Each Occupation
ggplot(avgSleepDuration) +
  aes(x = Occupation, y = avgSleepDuration, fill = Occupation) +
  geom_col() +
  scale_fill_manual(
    values = c(Accountant = "#440154",
               Doctor = "#CEA2FD",
               Engineer = "#960019",
               Lawyer = "#0075EF",
               Manager = "#027091",
               Nurse = "#5DCFCA",
               `Sales Representative` = "#27A833",
               Salesperson = "#097A30",
               Scientist = "#FF9913",
               `Software Engineer` = "#3F4D03",
               Teacher = "#8A3324")
  ) +
  labs(x = "Occupation",
       y = "Average Sleep Duration (hours)",
       title = "Average Sleep Duration for Each Occupation",
       fill = "Occupation"
       )+
  coord_flip() +
  theme_classic()+
  theme_bw(base_size = 16)

#Average Stress Levels per Occupation Table
avgStressLevels <- data %>%
  group_by(Occupation)%>%
  summarize(avgStressLevels = mean(Stress.Level))

#Avg Stress Levels for Each Occupation
ggplot(avgStressLevels) +
  aes(x = Occupation, y = avgStressLevels, fill = Occupation) +
  geom_col() +
  scale_fill_manual(
    values = c(Accountant = "#440154",
               Doctor = "#CEA2FD",
               Engineer = "#960019",
               Lawyer = "#0075EF",
               Manager = "#027091",
               Nurse = "#5DCFCA",
               `Sales Representative` = "#27A833",
               Salesperson = "#097A30",
               Scientist = "#FF9913",
               `Software Engineer` = "#3F4D03",
               Teacher = "#8A3324")
  ) +
  labs(x = "Occupation",
       y = "Average Stress Levels (scale 1-10)",
       title = "Average Stress Levels for Each Occupation",
       fill = "Occupation",
  )+
  coord_flip() +
  theme_classic()+
  theme_bw(base_size = 16)

#Sleep Duration Descriptive Statistics
sleepDurStats <- data%>%
  select(Sleep.Duration) %>%
  summarize(
    Min = min(data$Sleep.Duration, na.rm = TRUE),
    Q1 = quantile(data$Sleep.Duration, probs = 0.25, na.rm = TRUE),
    Median = median(data$Sleep.Duration, na.rm = TRUE),
    Q3 = quantile(data$Sleep.Duration, probs = 0.75, na.rm = TRUE),
    Q4 = quantile(data$Sleep.Duration, probs = 1, na.rm = TRUE),
    Max = max(data$Sleep.Duration, na.rm = TRUE),
    Mean = mean(data$Sleep.Duration, na.rm = TRUE),
    SD = sd(data$Sleep.Duration, na.rm = TRUE)
  )%>%
  round(., 2)

#Polished Sleep Duration Descr. Stats Table
sleepDurKable <- sleepDurStats %>%
  kable(
    caption = "Sleep Duration Descriptive Statistics",
    booktabs = TRUE,
    align = c("c",rep("c",6))
  ) %>%
  kable_styling(
    bootstrap_options = c("striped"),
    font_size = 16
  )

#Display Polished Sleep Duration Descr. Stats Table
sleepDurKable

#Stress Level Descriptive Stats
stressStats <- data%>%
  select(Stress.Level) %>%
  summarize(
    Min = min(data$Stress.Level, na.rm = TRUE),
    Q1 = quantile(data$Stress.Level, probs = 0.25, na.rm = TRUE),
    Median = median(data$Stress.Level, na.rm = TRUE),
    Q3 = quantile(data$Stress.Level, probs = 0.75, na.rm = TRUE),
    Q4 = quantile(data$Stress.Level, probs = 1, na.rm = TRUE),
    Max = max(data$Stress.Level, na.rm = TRUE),
    Mean = mean(data$Stress.Level, na.rm = TRUE),
    SD = sd(data$Stress.Level, na.rm = TRUE)
    )%>%
  round(., 2)

#Polished Stress Level Descr. Stats Table
stressKable <- stressStats %>%
  kable(
    caption = "Stress Level Descriptive Statistics",
    booktabs = TRUE,
    align = c("c",rep("c",6))
  ) %>%
  kable_styling(
    bootstrap_options = c("striped"),
    font_size = 16
  )

#Display Polished Stress Level Descr. Stats Table
stressKable

#genderCountTable
genderCount <- data %>%
  group_by(Gender)%>%
  summarise(Count=n())

#polishedGenderCount
polishedGenderCount <- kable(genderCount, caption = 'Gender', position = "center") %>%
  kable_styling(full_width = FALSE, font_size = 12)

#Display Polished Gender Count
polishedGenderCount


#gender distribution
ggplot(genderCount) +
  aes(x = Gender, fill = Gender, weight = Count.Freq) +
  geom_bar() +
  labs(title="Gender Distribution", x = "Gender", y= "Count")
  coord_flip() +
  theme_minimal()

#occupationGender
occAndGender <- data %>%
  group_by(Occupation,Gender) %>%
  summarise(Count = n())


ggplot(occAndGender, aes(x = Occupation, y = count, fill = Gender)) +
  geom_col(position = position_dodge()) +  # Stacked bars for gender comparison
  coord_flip() +
  labs(x = "Occupation",
       y = "Number of Individuals",
       title = "Gender Distribution of Occupation",
       fill = "Gender") +
  theme_bw()


View(occAndGender)
kable(occAndGender, row.names = FALSE) %>%
  kable_styling(full_width = F)
