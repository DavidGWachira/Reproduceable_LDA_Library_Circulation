## A Reproducible LDA Topic Modeling Methodology to Analyze Library Circulation

#-----------Load the necessary Libraries------------
library("tm") #text mining package
library("tidyverse") #data manipulation and visualization
library("tidytext") #tidy and tokenize data
library("topicmodels") #topic modeling
library("lubridate") #date manipulation
library("ggplot2") #visualizations
library("dplyr") #data manipulations
library("stringr") #string manipulation
library("textstem") #lemmatization
#install.packages("ldatuning", type = "binary")
library("ldatuning") #LDA tuning
library("slam") #Sparse matrix operation


# Clear the environment
rm(list = ls())

# Set the working directory
setwd("C:/Users/") # file location
dir()

# Read the dataset
df <- read.csv("BookBorrowingLogs.csv")
head(df) 
dim(df) #66087 entries


#----------------Text PreProcessing------------

# Add a Unique Document ID
df <- df %>%
  mutate(doc_id = row_number())

# Combine Text and Subject
df <- df %>%
  mutate(text = paste(TitleNormalized, Subjects, sep = " "))

# Tokenize and clean
cleaned <- df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!str_detect(word, "\\d")) %>%
  filter(str_length(word) > 2) %>%
  mutate(word = lemmatize_words(word))

# Create a DTM
dtm <- cleaned %>%
  count(doc_id, word) %>%
  cast_dtm(document = doc_id, term = word, value = n)
dtm
#DocumentTermMatrix (documents: 66048, terms: 32835)>>
#Non-/sparse entries: 722460/2167963620 - nonzero entries
#Sparsity           : 100% - zero entries
#Maximal term length: 29 - longest word

# Remove sparse terms
dtm_filtered <- removeSparseTerms(dtm, 0.99) #keep terms that appear in at least 1% of documents
dtm_filtered
#sparsity at 97%

# Remove empty rows (documents with no terms left)
row_totals <- slam::row_sums(dtm_filtered)
dtm_filtered <- dtm_filtered[row_totals > 0, ] #keep documents where TF>0
dtm_filtered

#---------- LDA Topic Modeling------------------

# Optimal number of topics (the k value)
result <- FindTopicsNumber(
  dtm_filtered,
  topics = seq(2, 15, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 1234),
  mc.cores = 1L,
  verbose = TRUE
)

FindTopicsNumber_plot(result)
#Griffiths2004 and Deveaud2014: Higher is better.
#CaoJuan2009 and Arun2010: Lower is better.
#k value = 10

# Set number of topics 
num_topics <- 10

# Fit the LDA model
lda_model <- LDA(dtm_filtered, k = num_topics, control = list(seed = 1234))
lda_model
topics <- tidy(lda_model, matrix = "beta")
topics
#beta-probability of word given topic - p(word|topic)

# Top Terms per Topic
top_terms <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>% #top 10
  ungroup() %>%
  arrange(topic, -beta)

top_terms

# Visualize the Top Terms per Topic
top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 5, nrow = 2) +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top Terms per Topic", x = "Terms", y = "Beta")


#---------Beta Variance (how concentrated or diffuse each topic is)--------
# Calculate variance of beta values per topic
beta_variance <- topics %>%
  group_by(topic) %>%
  summarise(beta_variance = var(beta))

beta_variance

# Visualize the variance
ggplot(beta_variance, aes(x = factor(topic), y = beta_variance)) +
  geom_col(fill = "steelblue") +
  labs(title = "Beta Variance per Topic",
       x = "Topic",
       y = "Variance of Beta") +
  theme_minimal()

#high variance - few words dominate - concentrated topic
#low variance - many words have similar probabilities - diffuse topic


#-----------Topic Distribution per Document (Gamma Matrix)------------
# Extract gamma matrix - Probability of each topic in each document P(topic|document)
gamma <- tidy(lda_model, matrix = "gamma")
gamma

# Which topics dominate each documet
top_topic_per_doc <- gamma %>%
  group_by(document) %>%
  slice_max(gamma, n = 1) %>%
  ungroup()

top_topic_per_doc

# Plot distribution
ggplot(top_topic_per_doc, aes(x = factor(topic))) +
  geom_bar(fill = "darkorange") +
  labs(title = "Topic Prevalence",
       x = "Topic",
       y = "Number of Documents") +
  theme_minimal()
#how many documents are primarily associated with each topic - 
#useful for understanding which topics are most prevalent

#--------------Topic Trends over Time----------------------------

# Join gamma with LoanYearMonth
gamma_with_time <- gamma %>%
  left_join(df %>% mutate(doc_id = as.character(doc_id)) %>% select(doc_id, LoanYearMonth),
            by = c("document" = "doc_id"))

# Calculate average topic proportion per MONTH
avg_topic_by_month <- gamma_with_time %>%
  group_by(LoanYearMonth, topic) %>%
  summarise(avg_gamma = mean(gamma), .groups = "drop")

# Plot
ggplot(avg_topic_by_month, aes(x = LoanYearMonth, y = avg_gamma, color = factor(topic), group = topic)) +
  geom_line(size = 1) +
  labs(title = "Topic Trends Over Time",
       x = "Loan Year-Month",
       y = "Average Topic Trend",
       color = "Topic") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Smoothed Topic Trends Over Time
ggplot(avg_topic_by_month, aes(x = LoanYearMonth, y = avg_gamma, color = factor(topic), group = topic)) +
  geom_line(alpha = 0.3) +  # Faint original lines
  geom_smooth(se = FALSE, method = "loess", span = 0.3, size = 1.2) +  # Smoothed lines
  labs(title = "Smoothed Topic Trends Over Time",
       x = "Loan Year-Month",
       y = "Average Topic Trend",
       color = "Topic") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calculate average topic Trend per YEAR

# Extract the Year
avg_topic_by_month$Year <- substr(avg_topic_by_month$LoanYearMonth, 1, 4)

# Aggregate Year and Topic
avg_topic_by_year <- avg_topic_by_month %>%
  group_by(Year, topic) %>%
  summarise(avg_gamma = mean(avg_gamma), .groups = "drop")

# Plot Trends by Year
ggplot(avg_topic_by_year, aes(x = Year, y = avg_gamma, color = factor(topic), group = topic)) +
  geom_line(size = 1.2) +
  labs(title = "Topic Trends by Year",
       x = "Year",
       y = "Average Topic Trend",
       color = "Topic") +
  theme_minimal()

# Smoothed Topic Trends by Year
ggplot(avg_topic_by_year, aes(x = as.numeric(Year), y = avg_gamma, color = factor(topic), group = topic)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = FALSE, method = "loess", span = 0.75, size = 1.2) +
  labs(title = "Smoothed Topic Trends by Year",
       x = "Year",
       y = "Average Topic Trend",
       color = "Topic") +
  theme_minimal()


# Topic Trends coefficients - Most Dominant Topics Overall
# Average topic proportion across all years
overall_topic_strength <- avg_topic_by_year %>%
  group_by(topic) %>%
  summarise(overall_avg = mean(avg_gamma), .groups = "drop") %>%
  arrange(desc(overall_avg))

overall_topic_strength
#topics by their importance average

# Find Topics with Strongest Trends
# Fit a linear model for each topic to estimate trend
library("broom")

topic_trends <- avg_topic_by_year %>%
  mutate(Year = as.numeric(Year)) %>%
  group_by(topic) %>%
  do(tidy(lm(avg_gamma ~ Year, data = .))) %>%
  filter(term == "Year") %>%
  arrange(desc(estimate))  

topic_trends

# Plot
library(gridExtra)
grid.table(topic_trends)








