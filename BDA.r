# Read the CSV files
tel_1 <- read.csv("D:/churn-bigml-80.csv")
tel_2 <- read.csv("D:/churn-bigml-20.csv")

# Concatenate the datasets
telcom <- rbind(tel_1, tel_2)

# Display the dimensions of the concatenated dataset
print(dim(telcom))

#2
# Get the number of rows and columns of the telcom data frame
telcom_shape <- dim(telcom)
#3
# Display the number of rows and columns
print(telcom_shape)
# Display the first few rows of the telcom data frame
head(telcom)
#4
# Display information about the structure of the telcom data frame
str(telcom)

# Summary statistics of the telcom data frame
summary(telcom)

# Access the 'Churn' column and display the first 10 rows
telcom_churn <- telcom$Churn[1:10]
print(telcom_churn)
# Obtain summary statistics for each numeric column in the telcom data frame
summary(telcom)


# Count the number of data points in each category

y <- table(telcom$Churn)
print(y)

#pie chart
# Get the count of unique values in the 'Churn' column
y <- table(telcom$Churn)

# Create a pie chart
pie(y, labels = names(y), main = "Distribution of Churn", col = rainbow(length(y)))

# Add percentage labels
percent_labels <- round(100 * y / sum(y), 1)
label_text <- paste(names(y), "\n", percent_labels, "%", sep="")
legend("bottomright", legend = label_text, cex = 0.8, fill = rainbow(length(y)))

# Add legend
legend("topright", legend = names(y), fill = rainbow(length(y)))

###bar graph
# Get the count of unique values in the 'Churn' column
y <- table(telcom$Churn)

# Create a bar plot
barplot(y, names.arg = names(y), main = "Distribution of Churn", xlab = "Churn", ylab = "Count")

############################ Statistics for both the classes
# Group telcom by 'Churn' and compute the mean
means <- aggregate(. ~ Churn, data = telcom, mean)
print(means)
# Group telcom by 'Churn' and compute the standard deviation
std_devs <- aggregate(. ~ Churn, data = telcom, sd)
print(std_devs)
########### Exploring feature distributions
# Load the ggplot2 library
library(ggplot2)

# Visualize the distribution of 'Account length'
ggplot(telcom, aes(x = `Account.length`)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Account length", x = "Account length", y = "Frequency")

# Visualize the distribution of 'Total day minutes'
ggplot(telcom, aes(x = `Total.day.minutes`)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Total day minutes", x = "Total day minutes", y = "Frequency")

# Visualize the distribution of 'Total eve minutes'
ggplot(telcom, aes(x = `Total.eve.minutes`)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Total eve minutes", x = "Total eve minutes", y = "Frequency")

# Visualize the distribution of 'Total intl minutes'
ggplot(telcom, aes(x = `Total.intl.minutes`)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Total intl minutes", x = "Total intl minutes", y = "Frequency")
###############3# Check for missing values
# Check for missing values in each column of the telcom data frame
has_missing <- colSums(is.na(telcom)) > 0
print(has_missing)
####################### check for duplicate rows 
# Identify duplicate rows in the telcom data frame
duplicate_rows <- telcom[duplicated(telcom), ]

# Print the duplicate rows
print(duplicate_rows)
# Display the first few rows of the telcom data frame
head(telcom)
###################data frame

# Get the data types of each column in the telcom data frame
column_types <- sapply(telcom, class)

# Print the data types
print(column_types)








# Get the names of boolean columns
bool_columns <- names(Filter(is.logical, telcom))

# Convert boolean values to integers (0 for FALSE, 1 for TRUE)
telcom[bool_columns] <- lapply(telcom[bool_columns], as.integer)

# Print the names of boolean columns
print(bool_columns)

# Get the names of object type columns
object_columns <- names(Filter(is.factor, telcom))

# Print the names of object type columns
print(object_columns)


# Load required libraries----doubt
library(dplyr)


# Convert boolean values to integers (0 for FALSE, 1 for TRUE)
telcom[bool_columns] <- lapply(telcom[bool_columns], as.integer)

# Replace 'No' with 0 and 'Yes' with 1 in 'International plan' and 'Voice mail plan'
telcom$International.plan <- ifelse(telcom$International.plan == 'No', 0, 1)
telcom$Voice.mail.plan <- ifelse(telcom$Voice.mail.plan == 'No', 0, 1)

# See the results
head(telcom[c('International.plan', 'Voice.mail.plan', 'Churn')])


################ Feature selection and engineering---doubt
# Drop 'State' feature
telcom <- telcom[, !(names(telcom) %in% c('State'))]

# Calculate the correlation matrix
corr_matrix <- cor(telcom)

# Select upper triangle of correlation matrix
upper <- corr_matrix
upper[lower.tri(corr_matrix)] <- NA

# Find index of feature columns with correlation greater than 0.95
to_drop <- colnames(upper)[apply(upper, 2, function(x) any(abs(x) > 0.95, na.rm = TRUE))]

# Drop the correlated features from the dataset
telcom <- telcom[, !(names(telcom) %in% to_drop)]

# Print the table of the first few rows of the modified dataset
print(head(telcom))

# Describe the 'Total intl calls' column
summary(telcom$`Total intl calls`)































