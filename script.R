library(dplyr)
library(tidytext)
library(janeaustenr)
library(stringr)
library(ggplot2)
library(tidyr)
library(readr)
library(lubridate)

# set up environment
rm(list = ls())
setwd('~/Documents/BMO')

# read file into tibble
complaints_tibble <- read.csv('complaints-2026-01-05_16_58.csv')

# I. data cleaning
# a. convert data type
complaints_tibble$Date.received <- as.Date(complaints_tibble$Date.receive, format = '%m/%d/%Y')
complaints_tibble$Date.sent.to.company <- as.Date(complaints_tibble$Date.sent.to.company, format = '%m/%d/%Y')

# b. tag col include multiple variable -> split those with multiple selections into separated rows
complaints_tibble <- complaints_tibble %>%
  separate_rows(Tags, sep = ", ")

# c. convert all empty cells into N/A instead of ""
col_w_empty_cells <- names(complaints_tibble[colSums(complaints_tibble == "", na.rm = TRUE) > 0])

complaints_tibble <- complaints_tibble %>%
  mutate(across(col_w_empty_cells, ~na_if(., "")))

# d. convert N/A to na
na_cols_logical <- sapply(complaints_tibble, function(x) {
  if(is.character(x)) {
    return(any(x == "N/A", na.rm = TRUE))
  } else {
    return(FALSE)
  }
})

cols_with_NA_text <- names(na_cols_logical)[na_cols_logical]

complaints_tibble <- complaints_tibble %>%
  mutate(across(cols_with_NA_text, ~na_if(., "N/A")))

# II. Data Summary
# Data Summary Code

# Count unique values in each column
unique_counts <- sapply(complaints_tibble, function(x) length(unique(x)))
print(unique_counts)

# III. Sentiment Analysis on BMO's consumer complaint narrative
# a. High level overview
# Use word cloud and sentiment bing
get_sentiments("bing")

# tokenize words
complaints_tibble <- complaints_tibble %>%
  unnest_tokens(word, Consumer.complaint.narrative)

# get negative words from bing
bing_negative <- get_sentiments("bing") %>%
  filter(sentiment == "negative")

# count negative word to prep for word cloud
negative_word_count <- complaints_tibble %>%
  inner_join(bing_negative) %>%
  count(word, sort = TRUE)

# word cloud for high-level overview
library(wordcloud2)

wordcloud2(data = negative_word_count,
           size = 1.5,
           color = "random-dark",
           backgroundColor = "black",
           shape = "circle")

# Comparative Analysis using nrc sentiment
# Method: Compare the emotional content of disputed vs. non-disputed complaints
# Goal: identify emotional patterns that might predict complaint resolution difficulty

nrc <- get_sentiments("nrc")

# join with nrc
complaints_tibble <- complaints_tibble %>%
  anti_join(stop_words) %>% # remove common stop word
  inner_join(nrc, relationship = "many-to-many")

# calculate emotion scores for each complaint
emotion_scores <- complaints_tibble %>%
  count(Complaint.ID, Consumer.disputed., sentiment) %>%
  group_by(Complaint.ID) %>%
  mutate(emotion_proportion = n / sum(n)) %>%
  ungroup()

# aggregate emotions by dispute status
emotion_by_dispute <- emotion_scores %>%
  group_by(Consumer.disputed., sentiment) %>%
  summarise(
    avg_score = mean(n, na.rm = TRUE),
    avg_proportion = mean(emotion_proportion, na.rm = TRUE),
    .groups = "drop"
  )

# Create stacked bar chart visualization
ggplot(emotion_by_dispute, aes(x = sentiment, y = avg_proportion, fill = Consumer.disputed.)) +
  geom_bar(position = "Stack", stat = "identity") +
  scale_fill_manual(values = c("No" = "#E74C3C", "Yes" = "#3498DB"),
                    labels = c("No" = "Disputed", "Yes" = "Not Disputed")) +
  labs(title = "Emotional Content in Disputed vs. Non-Disputed Complaints",
       subtitle = "Comparison of normalized emotion scores",
       x = "Emotion",
       y = "Average Proportion of Words",
       fill = "Complaint Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Line graph year over year trend
# 1. Prepare the data - extract year and quarter from Date.received
complaints_over_time <- complaints_tibble %>%
  # Convert Date.received to Date format
  mutate(
    date_received = as.Date(Date.received, format = "%m/%d/%Y"),
    # Extract year and quarter
    year = year(date_received),
    quarter = quarter(date_received),
    # Create year-quarter label
    year_quarter = paste0(year, " Q", quarter)
  ) %>%
  # Remove rows with missing dates
  filter(!is.na(date_received))

# 2. Count complaints by year-quarter
complaints_summary <- complaints_over_time %>%
  group_by(year, quarter, year_quarter) %>%
  summarise(
    complaint_count = n(),
    .groups = "drop"
  ) %>%
  # Create a proper date for plotting (first day of each quarter)
  mutate(
    quarter_date = ymd(paste(year, (quarter - 1) * 3 + 1, "01", sep = "-"))
  ) %>%
  arrange(quarter_date)

# 3. Create the line graph
# Alternative: Show fewer x-axis labels for better readability
ggplot(complaints_summary, aes(x = quarter_date, y = complaint_count)) +
  geom_line(color = "#2E86AB", size = 1.2) +
  geom_point(color = "#2E86AB", size = 3) +
  labs(
    title = "Number of Complaints Over Time",
    subtitle = "Quarterly trend of consumer complaints",
    x = "Year-Quarter",
    y = "Number of Complaints"
  ) +
  scale_x_date(
    date_breaks = "1 year",  # Show every year instead of every quarter
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    labels = scales::comma
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90"),
    panel.grid.major.y = element_line(color = "gray90")
  )

# 4. Summary statistics
cat("Average complaints per quarter:", 
    round(mean(complaints_summary$complaint_count), 0), "\n")
cat("Peak quarter:", 
    complaints_summary$year_quarter[which.max(complaints_summary$complaint_count)],
    "with", max(complaints_summary$complaint_count), "complaints\n")
