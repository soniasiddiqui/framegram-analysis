# Framegram Analysis: Womanhood in relation to romance 
# Student Name: Sonia Siddiqui 

# --- 1. SETUP: Load Packages and Define Stop Words ---

library(pacman)

p_load(
  tidyverse,
  tidytext,
  stopwords,
  rio
  )

# --- Define Custom Stop Word List ---
# Load a standard stop word list (Snowball English)
my_stopwords <- data_stopwords_snowball$en

# --- CUSTOMIZE YOUR STOP WORD LIST HERE ---
# Modify the list to include or exclude words relevant to your specific research focus.
# Example: removing 'not' is important for 'fear not' framegrams... you don't want 'not' in
# your stopwords list!
my_stopwords <- my_stopwords[!(my_stopwords %in% c("your", "into", "by", "and", "much"))]
# my_stopwords <- c(my_stopwords, "archaicword1", "archaicword2", "archaicword3") # Add archaic words if needed

# Convert the final list into a regular expression pattern for counting
my_stopwords_regex <- paste0("\\b(", paste(my_stopwords, collapse = "|"), ")\\b")

# --- 1.2. Define Your Keywords ---
# Choose 3 keywords relevant to your comparative topic
# Topic: Womanhood in relation to romance
keyword_list <- c("virtue", "marriage", "love")
keyword_regex <- paste(keyword_list, collapse = "|") # Creates a regex like: "word1|word2|word3"

# --- 2. DATA IMPORT (Assume files are in a 'data' subfolder) ---

# Novel 1: Pamela, PG ID: 6124
novel1_df <- tibble(
  text = readLines("data/Pamela.txt", warn = FALSE)) %>%
  filter(text != "") %>%
  mutate(novel = "Novel 1: Pamela")

# Novel 2: Anti-Pamela, PG ID: used different source
novel2_df <- tibble(
  text = readLines("data/Anti-Pamela.txt", warn = FALSE)) %>%
  filter(text != "") %>%
  mutate(novel = "Novel 2: Anti-Pamela")

# Combine the two novels for easier tokenization, keeping source information
combined_novels <- bind_rows(novel1_df, novel2_df) %>%
  # Ensure the text is lowercased for consistent tokenization
  mutate(text = tolower(text))

# --- 3. TOKENIZATION AND FEATURE EXTRACTION ---

# 3.1. Tokenize into Trigrams
trigrams_df <- combined_novels %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  # Remove rows with NAs resulting from tokenization (e.g., end of document)
  filter(!is.na(trigram))

# 3.2. Feature Engineering (Applying Graham's Constraints)
framegrams_analysis_df <- trigrams_df %>%
  mutate(
    # Check if the trigram contains any of the focus keywords
    contains_keyword = str_detect(trigram, keyword_regex),

    # Count the number of stop words in the trigram
    n_stop_words = str_count(string = trigram, pattern = my_stopwords_regex),

    # Define the final frame gram: must contain keyword AND have < 2 stop words
    is_frame_gram = contains_keyword & (n_stop_words < 2)
  )

# --- 4. ANALYSIS: COUNTING AND DISPLAYING TOP FRAME GRAMS ---

# Filter for only true frame grams and count their frequency
final_results <- framegrams_analysis_df %>%
  filter(is_frame_gram) %>%
  count(novel, trigram, sort = TRUE) %>%
  group_by(novel)

# --- Output for Novel 1 ---
cat("\n--- Top 10 Frame Grams in Pamela: ---\n")
final_results %>%
  filter(novel == "Novel 1: Pamela") %>%
  slice_max(n, n = 10) %>%
  print(n = 10)

# --- Output for Novel 2 ---
cat("\n--- Top 10 Frame Grams in Anti-Pamela: ---\n")
final_results %>%
  filter(novel == "Novel 2: Anti-Pamela") %>%
  slice_max(n, n = 10) %>%
  print(n = 10)

# Optional: Visualize results (requires ggplot2)
library(ggplot2)
final_results %>%
   slice_max(n, n = 10) %>%
   ungroup() %>%
   mutate(trigram = reorder_within(trigram, n, novel)) %>%
   ggplot(aes(x = trigram, y = n, fill = novel)) +
   geom_col(show.legend = FALSE) +
   coord_flip() +
   facet_wrap(~novel, scales = "free_y") +
   scale_x_reordered() +
   labs(title = "Top 10 Frame Grams by Novel")
