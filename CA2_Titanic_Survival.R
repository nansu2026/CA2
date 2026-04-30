# Install packages
install.packages("psych")
install.packages("dplyr")
install.packages("rstatix")
install.packages("ggplot2")

# Load libraries
library(psych)    # required for skew()
library(dplyr)    # required for data cleaning
library(rstatix)  # useful for statistical testing
library(ggplot2)  # required for charts



# Step 1: Load the dataset
# ------------------------------------------------------------

Titanic_Dataset <- read.csv("C:/Users/nancy/Downloads/archive/Titanic-Dataset.csv")
# View the dataset
View(Titanic_Dataset)

# View the first rows
head(Titanic_Dataset)

# Check the structure of the dataset
str(Titanic_Dataset)

# Check number of rows and columns
dim(Titanic_Dataset)

# Check column names
names(Titanic_Dataset)


# Step 2: Check missing values
# ------------------------------------------------------------

colSums(is.na(Titanic_Dataset))


# Step 3: Convert variables to correct data types
# ------------------------------------------------------------

Titanic_Dataset <- Titanic_Dataset %>%
  mutate(
    Survived = factor(Survived, levels = c(0, 1), labels = c("No", "Yes")),
    Pclass = factor(Pclass, levels = c(1, 2, 3), labels = c("First", "Second", "Third")),
    Sex = factor(Sex),
    Embarked = factor(Embarked)
  )

# Check structure again after conversion
str(Titanic_Dataset)

# Step 4: Descriptive statistics
# ------------------------------------------------------------
# Survival count and percentage
table(Titanic_Dataset$Survived)
prop.table(table(Titanic_Dataset$Survived)) * 100

# Sex and survival table
table(Titanic_Dataset$Sex, Titanic_Dataset$Survived)
prop.table(table(Titanic_Dataset$Sex, Titanic_Dataset$Survived), margin = 1) * 100

# Passenger class and survival table
table(Titanic_Dataset$Pclass, Titanic_Dataset$Survived)
prop.table(table(Titanic_Dataset$Pclass, Titanic_Dataset$Survived), margin = 1) * 100

# Embarkation port and survival table
table(Titanic_Dataset$Embarked, Titanic_Dataset$Survived)
prop.table(table(Titanic_Dataset$Embarked, Titanic_Dataset$Survived), margin = 1) * 100

# Age descriptive statistics
summary(Titanic_Dataset$Age)
mean(Titanic_Dataset$Age, na.rm = TRUE)
median(Titanic_Dataset$Age, na.rm = TRUE)
sd(Titanic_Dataset$Age, na.rm = TRUE)
skew(Titanic_Dataset$Age, na.rm = TRUE)

# Fare descriptive statistics
summary(Titanic_Dataset$Fare)
mean(Titanic_Dataset$Fare, na.rm = TRUE)
median(Titanic_Dataset$Fare, na.rm = TRUE)
sd(Titanic_Dataset$Fare, na.rm = TRUE)
skew(Titanic_Dataset$Fare, na.rm = TRUE)


# Step 5: Descriptive charts
# ------------------------------------------------------------

# Survival count chart
windows(16, 10)
ggplot(Titanic_Dataset, aes(x = Survived)) +
  geom_bar() +
  labs(
    title = "Survival Count of Titanic Passengers",
    x = "Survived",
    y = "Number of passengers"
  )

# Survival by sex chart
windows(16, 10)
ggplot(Titanic_Dataset, aes(x = Sex, fill = Survived)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Survival by Sex",
    x = "Sex",
    y = "Number of passengers"
  )

# Survival by class chart
windows(16, 10)
ggplot(Titanic_Dataset, aes(x = Pclass, fill = Survived)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Survival by Passenger Class",
    x = "Passenger Class",
    y = "Number of passengers"
  )

# Survival by embarkation chart
ggplot(Titanic_Dataset, aes(x = Embarked, fill = Survived)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Survival by Embarkation Port",
    x = "Embarkation Port",
    y = "Number of passengers"
  )

# Age histogram
windows(16, 10)
ggplot(Titanic_Dataset, aes(x = Age)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribution of Passenger Age",
    x = "Age",
    y = "Frequency"
  )

# Fare histogram
ggplot(Titanic_Dataset, aes(x = Fare)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribution of Passenger Fare",
    x = "Fare",
    y = "Frequency"
  )


# Step 6: Research Question 1
# Is survival associated with passenger sex?
# ------------------------------------------------------------

# H0: There is no association between passenger sex and survival.
# H1: There is an association between passenger sex and survival.

sex_survival_table <- table(Titanic_Dataset$Sex, Titanic_Dataset$Survived)

sex_survival_table

chisq.test(sex_survival_table)

# Step 7: Research Question 2
# Is survival associated with passenger class?
# ------------------------------------------------------------

# H0: There is no association between passenger class and survival.
# H1: There is an association between passenger class and survival.

class_survival_table <- table(Titanic_Dataset$Pclass, Titanic_Dataset$Survived)

class_survival_table

chisq.test(class_survival_table)



# Step 8: Research Question 3
# Is survival associated with embarkation port?
# ------------------------------------------------------------

# H0: There is no association between embarkation port and survival.
# H1: There is an association between embarkation port and survival.

# Remove missing and blank Embarked values, then drop unused factor levels
embarked_data <- Titanic_Dataset %>%
  filter(!is.na(Embarked), Embarked != "") %>%
  droplevels()

# Create table
embarked_survival_table <- table(embarked_data$Embarked, embarked_data$Survived)

# View table
embarked_survival_table

# Run chi-squared test
chisq.test(embarked_survival_table)


# Step 9: Research Question 4
# Is age different between survivors and non-survivors?
# ------------------------------------------------------------

# H0: There is no difference in age between passengers who survived and passengers who did not survive.
# H1: There is a difference in age between passengers who survived and passengers who did not survive.

age_data <- Titanic_Dataset %>%
  filter(!is.na(Age))

# Visual normality check: histogram
windows(16, 10)
ggplot(age_data, aes(x = Age)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Age Distribution",
    x = "Age",
    y = "Frequency"
  )

# Visual normality check: Q-Q plot
ggplot(age_data, aes(sample = Age)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Q-Q Plot for Age")

# Statistical normality test by survival group
shapiro.test(age_data$Age[age_data$Survived == "No"])
shapiro.test(age_data$Age[age_data$Survived == "Yes"])

# Because Age is usually not normally distributed, use Mann-Whitney test
wilcox.test(Age ~ Survived, data = age_data)



# Step 10: Research Question 5
# Is fare different across passenger classes?
# ------------------------------------------------------------

# H0: There is no difference in fare across passenger classes.
# H1: There is a difference in fare across passenger classes.

# Visual normality check: histogram
windows(16, 10)
ggplot(Titanic_Dataset, aes(x = Fare)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Fare Distribution",
    x = "Fare",
    y = "Frequency"
  )

# Visual normality check: Q-Q plot
ggplot(Titanic_Dataset, aes(sample = Fare)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Q-Q Plot for Fare")

# Statistical normality test by passenger class
shapiro.test(Titanic_Dataset$Fare[Titanic_Dataset$Pclass == "First"])
shapiro.test(Titanic_Dataset$Fare[Titanic_Dataset$Pclass == "Second"])
shapiro.test(Titanic_Dataset$Fare[Titanic_Dataset$Pclass == "Third"])

# Because Fare is usually not normally distributed, use Kruskal-Wallis test
kruskal.test(Fare ~ Pclass, data = Titanic_Dataset)

#rstatix version
Titanic_Dataset %>%
  kruskal_test(Fare ~ Pclass)



