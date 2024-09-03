library(ggplot2)
library(dplyr)
library(reshape2)
install.packages("corrplot")
library(corrplot)

View(Student_performance_data_)
data <- (Student_performance_data_)
head(data)

correlation_data <- data %>%
  select(StudyTimeWeekly, Absences, Tutoring, GPA)

correlation_matrix <- cor(correlation_data)

corrplot(correlation_matrix, method = "color", addCoef.col = "black", tl.cex = 0.8, number.cex = 0.8)

gender_analysis <- data %>%
  group_by(Gender) %>%
  summarise(
    StudyTimeWeekly = mean(StudyTimeWeekly),
    Absences = mean(Absences),
    Tutoring = mean(Tutoring),
    GPA = mean(GPA)
  )
View(gender_analysis)

ethnicity_analysis <- data %>%
  group_by(Ethnicity) %>%
  summarise(
    StudyTimeWeekly = mean(StudyTimeWeekly),
    Absences = mean(Absences),
    Tutoring = mean(Tutoring),
    GPA = mean(GPA)
  )
View(ethnicity_analysis)

grade_level_analysis <- data %>%
  group_by(GradeClass) %>%
  summarise(
    StudyTimeWeekly = mean(StudyTimeWeekly),
    Absences = mean(Absences),
    Tutoring = mean(Tutoring),
    GPA = mean(GPA)
  )
View(grade_level_analysis)

print(gender_analysis)
print(ethnicity_analysis)
print(grade_level_analysis)

melted_correlation <- melt(correlation_matrix)

ggplot(data = melted_correlation, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name="Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  coord_fixed() +
  geom_text(aes(Var1, Var2, label = round(value, 2)), color = "black", size = 4) +
  labs(title = "Correlation Matrix for Student Performance Data", x = "", y = "")

ggplot(data = grade_level_analysis, aes(x = as.factor(GradeClass), y = GPA)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Average GPA by Grade Level", x = "Grade Level", y = "Average GPA") +
  theme_minimal()

ggplot(data = gender_analysis, aes(x = as.factor(Gender), y = GPA)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Average GPA by Gender", x = "Gender", y = "Average GPA") +
  theme_minimal()

ggplot(data = ethnicity_analysis, aes(x = as.factor(Ethnicity), y = GPA)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Average GPA by Ethnicity", x = "Ethnicity", y = "Average GPA") +
  theme_minimal()

tutoring_grade_analysis <- data %>%
  group_by(GradeClass, Tutoring) %>%
  summarise(
    AverageGPA = mean(GPA)
  )

ggplot(tutoring_grade_analysis, aes(x = as.factor(GradeClass), y = AverageGPA, fill = as.factor(Tutoring))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Impact of Tutoring on GPA Across Grade Levels", x = "Grade Level", y = "Average GPA") +
  scale_fill_manual(values = c("0" = "red", "1" = "green"), labels = c("No Tutoring", "Tutoring")) +
  theme_minimal()

absences_ethnicity_analysis <- data %>%
  group_by(Ethnicity) %>%
  summarise(
    AverageAbsences = mean(Absences),
    AverageGPA = mean(GPA)
  )

ggplot(absences_ethnicity_analysis, aes(x = AverageAbsences, y = AverageGPA, color = as.factor(Ethnicity))) +
  geom_point(size = 4) +
  labs(title = "Impact of Absences on GPA by Ethnicity", x = "Average Absences", y = "Average GPA") +
  theme_minimal()

studytime_gender_analysis <- data %>%
  group_by(Gender) %>%
  summarise(
    AverageStudyTime = mean(StudyTimeWeekly),
    AverageGPA = mean(GPA)
  )

ggplot(studytime_gender_analysis, aes(x = AverageStudyTime, y = AverageGPA, color = as.factor(Gender))) +
  geom_point(size = 4) +
  labs(title = "Effect of Study Time on GPA by Gender", x = "Average Study Time (Weekly)", y = "Average GPA") +
  theme_minimal()
