library(dplyr)
library(tidytext)
library(janeaustenr)
library(stringr)
library(ggplot2)
library(tidyr)
library(readr)

# set up environment
rm(list = ls())
setwd('~/Documents/r_projects/text_analysis/data')

# read file into tibble
complaints_tibble <- read.csv('Consumer_Complaints.csv')

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

# Find which company has the most records
top_5_companies <- complaints_tibble %>%
  count(Company, sort = TRUE) %>%  # Count records by country and sort
  head(5)


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
  filter(Company == "BMO Harris") %>%
  inner_join(bing_negative) %>%
  count(word, sort = TRUE)

# word cloud for high-level overview
library(wordcloud2)

wordcloud2(data = negative_word_count,
           size = 1.5,
           color = "random-dark",
           backgroundColor = "white",
           shape = "circle")

 # Create chart to analyze emotion based on each product
complaint_product_sentiment <- complaints_tibble %>%
  filter(Company == "BMO Harris") %>%
  inner_join(get_sentiments("bing"),relationship = "many-to-many") %>%
  count(Product, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

ggplot(complaint_product_sentiment, aes(x = Product, y = sentiment, fill = Product)) +
  geom_col() +
  labs(title = "Net Sentiment by Product",
       x = "Product",
       y = "Net Sentiment (Positive - Negative)") +
  theme_minimal()

# Comparative Analysis using nrc sentiment
# Method: Compare the emotional content of disputed vs. non-disputed complaints
# Goal: identify emotional patterns that might predict complaint resolution difficulty

nrc <- get_sentiments("nrc")

# join with nrc
complaints_tibble <- complaints_tibble %>%
  filter(Company == "BMO Harris") %>%
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

# Statistical analysis - logistic regression
# Reshape data for logistic regression
complaints_tibble <- complaints_tibble %>%
  mutate(binary_dispute = case_when(
    Consumer.disputed. == "Yes" ~ 1,
    Consumer.disputed. == "No" ~ 0,
    TRUE ~ NA_real_
  ))

emotion_wide <- complaints_tibble %>%
  count(Complaint.ID,binary_dispute, sentiment) %>%
  pivot_wider(
    id_cols = c(Complaint.ID, binary_dispute),
    names_from = sentiment,
    values_from = n,
    values_fill = 0
  )

# Run logistic regression
dispute_model <- glm(binary_dispute ~ anger + fear + joy + sadness + trust + surprise + anticipation + disgust,
                     data = emotion_wide, family = "binomial")

# View model summary
summary(dispute_model)

# validate model with Chi-Squared Test 
chisq_test <- anova(dispute_model, test = "Chisq")
print(chisq_test)
