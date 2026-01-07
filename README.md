# BMO N.A. Consumer Complaint Analysis 🏦

## By Robin Nguyen ☀️

![BMO Banner](https://github.com/minhnbnguyen/BMO/blob/main/pics/1636124069353.jpeg)

## Introduction
This project analyzes the customer narrative complaint from BMO N.A. to find relationship between customer's emotion with complaint dispute rate. In this project, I compare the emotional content of disputed vs. non-disputed complaints to identify emotional patterns that might predict complaint resolution difficulty.

## Data Dictionary 📖
Our dataset includes the following columns:

- **Date.received**: Date that the complaint was received
- **Product**: Product that was complaint about (i.e Debt Collection, Mortgage,..)
- **Sub.product**: Sub category of the product (i.e Credit Card, Debit Card,...)
- **Issue**: Issue category
- **Sub.issue**: Sub issue category
- **Consumer.complaint.narrative**: Consumer explanation on the issue
- **Company.public.response**: Company public response via website or social media
- **Company**: Name of bank. Note: Our dataset includes all US banks, but for the range of this project I will filter out only JP Morgan Chase Co.
- **State**: State where the headquarter is located
- **ZIP.code**: ZIP code where the headquarter is located
- **Tags**: The department/office that the issue will be directed to 
- **Consumer.consent.provided.**: Consumer consent to public issue
- **Submitted.via**: Platform the issue was submitted
- **Date.sent.to.company**: Date that the issue was sent to the bank
- **Company.response.to.consumer**: Company resolution to the issue
- **Timely.response**: Whether if this is a timely response or not
- **Consumer.disputed.**: Whether if the consumer dispute the resolution or not
- **Complaint.ID**: The unique identifier of each complaint

## Data Cleaning Methodology To Ensure Tidy Data 🧹

### Tag column include multiple variables
- Split tag columns into seperate rows if there are more than 2 tags

```r
complaints_tibble <- complaints_tibble %>%
  separate_rows(Tags, sep = ", ")
```

### Standardize empty cells to na format
- Unified country name formats
- Standardized variations of USA/U.S.A to "United States"
- Determined missing countries from locality information

```r
# convert all empty cells into N/A instead of ""
col_w_empty_cells <- names(complaints_tibble[colSums(complaints_tibble == "", na.rm = TRUE) > 0])

complaints_tibble <- complaints_tibble %>%
  mutate(across(col_w_empty_cells, ~na_if(., "")))

# convert "N/A" to na
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

```

### Date Formatting 📆
- Converted all date column from varchar to date format

```r
complaints_tibble$Date.received <- as.Date(complaints_tibble$Date.receive, format = '%m/%d/%Y')
complaints_tibble$Date.sent.to.company <- as.Date(complaints_tibble$Date.sent.to.company, format = '%m/%d/%Y')
```

## Key Findings

### High-level view of the customer complaint
![Word Cloud](https://github.com/minhnbnguyen/BMO/blob/main/pics/Rplot_wordcloud.png)
- The most common problem are likely related to wrong information, lost, or failed issue

### Comparative Analysis using nrc sentiment
![Emotional content](https://github.com/minhnbnguyen/BMO/blob/main/pics/Rplot1.png)
- Method: Compare the emotional content of disputed vs. non-disputed complaints
- Goal: Identify emotional patterns that might predict complaint resolution difficulty
- Result: Largest dispute ratio falls within negative and trust emotions

  
## Perform statistical analysis to find correlation between emotion and dispute rate

### Run logistic regression
![Logistic](https://github.com/minhnbnguyen/BMO/blob/main/pics/Screen%20Shot%202026-01-06%20at%2020.28.09.png)

Significant predictors are
- anger (p = 0.002522): positive relationship 💢
- joy (p = 0.01): negative relationship 😊
- trust (p = 0.000456): positive relationship 🙏🏻
- anticipation (p = 0.09): positive relationship 😌


### Validate Model with Chi-Square Test
![Validate](https://github.com/minhnbnguyen/BMO/blob/main/pics/Screen%20Shot%202026-01-06%20at%2020.28.51.png)

Significant predictors are
- Anger (p = 9.61e)
- Trust (p = 3.79e)
- Sadness (p = 0.01)

- Joy is significant in the coefficient test but not in the sequential test, suggesting it may share explanatory power with variables added earlier
- Sadness is significant in the sequential test but not in the coefficient test

## Final Suggestions
- Focus on anger and trust as your primary findings since they are significant in both tests
- Acknowledge joy as potentially important since it's significant when controlling for all variables
- Consider whether to include sadness based on your research question and theoretical framework -> work backwards
