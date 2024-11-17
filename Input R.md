# 2b. Read in your data
setwd("D:/3. KULIAH/5th SEMESTER/Analisis Data")
getwd()
data <- read.csv2("depression_age.csv", sep = ";", header = TRUE)
data
names(data) <- make.names(names(data))
names(data)

# 2c. Check the packaging
nrow(data)
ncol(data)
str(data)

# 2d. Look at the top and the bottom of your data
head(data)
tail(data)

# 2e. Check your “n”s
library(dplyr)
summary(data)
sum(is.na(data$age))
sum(is.na(data$depression_diagnosis))
dim(data)
datafix <- data %>% filter(!is.na(data$depression_diagnosis)) 
dim(datafix)
summary(datafix)

# 2f. Validate with at least one external data source
summary(datafix$age)
quantile(datafix$age, seq(0, 1, 0.1))

# 2g. Make a plot 
library(ggplot2)
ggplot(datafix, aes(x = age, y = depression_diagnosis)) +
  geom_boxplot(color = "violet") +
  labs(title = "Boxplot Diagnosis Depresi Berdasarkan Usia",
       x = "Usia",
       y = "Diagnosis Depresi") +
  theme_minimal()

ggplot(datafix, aes(x = age, fill = depression_diagnosis)) +
  geom_histogram(binwidth = 5, position = "dodge") +
  labs(title = "Distribusi Diagnosis Depresi Berdasarkan Usia",
       x = "Usia",
       y = "Frekuensi",
       fill = "Diagnosis Depresi") +
  theme_minimal()

# 2h. Try the easy solution first
cor.test(as.numeric(datafix$age), as.numeric(datafix$depression_diagnosis))

# 2i. Follow up
# Mengelompokkan umur
datafix <- datafix %>%
  mutate(age_group = case_when(
    age >= 13 & age <= 19 ~ "Teenager",
    age >= 20 & age <= 35 ~ "Young Adult",
    age > 35 ~ "Adult"))
table(datafix$age_group, datafix$depression_diagnosis)
# Konversi variabel respons menjadi faktor
datafix$depression_diagnosis <- as.factor(datafix$depression_diagnosis)
# Model regresi logistik
logistic_model <- glm(depression_diagnosis ~ age, data = datafix, family = binomial)
summary(logistic_model)
# Odds ratio
exp(coef(logistic_model))

# 3a. Models as Expectations
# Model regresi logistik sebagai ekspektasi awal
logistic_model <- glm(depression_diagnosis ~ age, data = datafix, family = binomial)
summary(logistic_model)

# 3b. Comparing Model Expectations to Reality
# Histogram variabel dependen
ggplot(datafix, aes(x = as.numeric(depression_diagnosis))) +
  geom_histogram(binwidth = 0.1, color = "violet", fill = "lightblue", alpha = 0.7) +
  labs(title = "Distribusi Diagnosis Depresi", x = "Diagnosis Depresi", y = "Frekuensi") +
  theme_minimal()
# Histogram distribusi normal
set.seed(123)
normal_data <- rnorm(nrow(datafix), 
                     mean = mean(as.numeric(datafix$depression_diagnosis)),
                     sd = sd(as.numeric(datafix$depression_diagnosis)))
ggplot() +
  geom_histogram(aes(x = normal_data, y = ..density..), binwidth = 0.1,
                 color = "violet", fill = "lightblue", alpha = 0.7) +
  geom_density(aes(x = normal_data), color = "violet", size = 1.2) +
  labs(title = "Distribusi Normal", 
       x = "Simulasi Nilai Normal", 
       y = "Density") +
  theme_minimal()
  
