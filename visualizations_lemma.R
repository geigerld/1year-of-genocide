library(tidyverse)
library(plotly)
library(patchwork)

data <- read_csv("frequency_data/en_DW_frequency.csv") %>%
  mutate(publish_date = as.Date(as.character(publish_date), format = "%Y%m%d"))


data %>%
  mutate(
    Lemma_Israel = if_else(Lemma_Israel > 100, 100, Lemma_Israel),
    Lemma_Hamas = if_else(Lemma_Hamas > 100, 100, Lemma_Hamas)
  ) %>%
  ggplot() +
  geom_line(aes(x = publish_date, y = Lemma_Israel), color = "red", alpha = 0.5) +
  geom_line(aes(x = publish_date, y = Lemma_Hamas), color = "blue", alpha = 0.5) +
  geom_smooth(aes(x = publish_date, y = Lemma_Israel), se = F, color = "red") +
  geom_smooth(aes(x = publish_date, y = Lemma_Hamas), se = F, color = "blue")
geom_line(aes(x = publish_date, y = Lemma_Israel), color = "black", alpha = 0.5) +

  max_total_word <- data$total_word_count %>% max()
max_lemma_israel <- data$Lemma_Israel %>% max()

# Data structure as long table
data_long <- data %>%
  pivot_longer(cols = starts_with("Lemma_"), names_to = "word_lemma", values_to = "count_lemma") %>%
  pivot_longer(cols = starts_with("Token_"), names_to = "word_token", values_to = "count_token") %>%
  mutate(
    word_lemma = str_remove(word_lemma, "Lemma_"),
    word_token = str_remove(word_token, "Token_")
  ) %>%
  group_by(word_lemma) %>%
  arrange(publish_date, .by_group = TRUE) %>%
  mutate(count_lemma_acum = cumsum(count_lemma)) %>%
  ungroup() %>%
  mutate(log_count_lemma_acum = log(count_lemma_acum + 1))

# Plotting
ggplotly(data_long %>%
  filter(word_lemma == "Iran") %>%
  ggplot(aes(x = publish_date, y = log_count_lemma_acum, color = word_lemma)) +
  geom_line() +
  theme(panel.grid.major = element_blank()) +
  scale_color_viridis_d())

# Other datasets

AJ <- read_csv("frequency_data/en_AJ_frequency.csv") %>%
  mutate(publish_date = as.Date(as.character(publish_date), format = "%Y%m%d")) %>%
  pivot_longer(cols = starts_with("Lemma_"), names_to = "word_lemma", values_to = "count_lemma") %>%
  pivot_longer(cols = starts_with("Token_"), names_to = "word_token", values_to = "count_token") %>%
  mutate(
    word_lemma = str_remove(word_lemma, "Lemma_"),
    word_token = str_remove(word_token, "Token_")
  ) %>%
  group_by(word_lemma) %>%
  arrange(publish_date, .by_group = TRUE) %>%
  mutate(count_lemma_acum = cumsum(count_lemma)) %>%
  ungroup() %>%
  mutate(log_count_lemma_acum = log(count_lemma_acum + 1)) %>%
  mutate(media = "AJ")

BBC <- read_csv("frequency_data/en_BBCNews_frequency.csv") %>%
  mutate(publish_date = as.Date(as.character(publish_date), format = "%Y%m%d")) %>%
  pivot_longer(cols = starts_with("Lemma_"), names_to = "word_lemma", values_to = "count_lemma") %>%
  pivot_longer(cols = starts_with("Token_"), names_to = "word_token", values_to = "count_token") %>%
  mutate(
    word_lemma = str_remove(word_lemma, "Lemma_"),
    word_token = str_remove(word_token, "Token_")
  ) %>%
  group_by(word_lemma) %>%
  arrange(publish_date, .by_group = TRUE) %>%
  mutate(count_lemma_acum = cumsum(count_lemma)) %>%
  ungroup() %>%
  mutate(log_count_lemma_acum = log(count_lemma_acum + 1)) %>%
  mutate(media = "BBC")

CNN <- read_csv("frequency_data/en_CNN_frequency.csv") %>%
  mutate(publish_date = as.Date(as.character(publish_date), format = "%Y%m%d")) %>%
  pivot_longer(cols = starts_with("Lemma_"), names_to = "word_lemma", values_to = "count_lemma") %>%
  pivot_longer(cols = starts_with("Token_"), names_to = "word_token", values_to = "count_token") %>%
  mutate(
    word_lemma = str_remove(word_lemma, "Lemma_"),
    word_token = str_remove(word_token, "Token_")
  ) %>%
  group_by(word_lemma) %>%
  arrange(publish_date, .by_group = TRUE) %>%
  mutate(count_lemma_acum = cumsum(count_lemma)) %>%
  ungroup() %>%
  mutate(log_count_lemma_acum = log(count_lemma_acum + 1)) %>%
  mutate(media = "CNN")

DW <- read_csv("frequency_data/en_DW_frequency.csv") %>%
  mutate(publish_date = as.Date(as.character(publish_date), format = "%Y%m%d")) %>%
  pivot_longer(cols = starts_with("Lemma_"), names_to = "word_lemma", values_to = "count_lemma") %>%
  pivot_longer(cols = starts_with("Token_"), names_to = "word_token", values_to = "count_token") %>%
  mutate(
    word_lemma = str_remove(word_lemma, "Lemma_"),
    word_token = str_remove(word_token, "Token_")
  ) %>%
  group_by(word_lemma) %>%
  arrange(publish_date, .by_group = TRUE) %>%
  mutate(count_lemma_acum = cumsum(count_lemma)) %>%
  ungroup() %>%
  mutate(log_count_lemma_acum = log(count_lemma_acum + 1)) %>%
  mutate(media = "DW")

combined_channels <- rbind(AJ, BBC, CNN, DW)

lemma <- "Palestine"

# Color palette

media_colors <- c("#d69d10", "#000000", "#cc0000", "#05b2fc")

plot_lemma <- function(lemma, date_to_plot) {
  # Plotting
  filtered_data <- combined_channels %>%
    filter(word_lemma == lemma) %>%
    filter(as.Date(publish_date, format = "%Y-%m-%d") > as.Date("2023-10-07"))

  plot1 <- filtered_data %>%
    ggplot(aes(x = publish_date, y = count_lemma_acum, color = media)) +
    geom_line() +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.text.x = element_blank()) +
    scale_y_log10() +
    scale_color_manual(values = media_colors) +
    xlab("") +
    ylab("Accumulated frequency") +
    geom_vline(xintercept = as.Date(date_to_plot), linetype = "dashed") +
    labs(title = paste0("Mentions of ", lemma," in media"))

  plot2 <- filtered_data %>%
    ggplot(aes(x = publish_date, y = count_lemma, fill = media)) +
    geom_vline(xintercept = as.Date(date_to_plot), linetype = "dashed") +
    geom_col(alpha = 0.7) +
    theme_minimal() +
    theme(panel.grid = element_blank()) +
    scale_fill_manual(values = media_colors) +
    xlab("") +
    ylab("Mentions")

  # Align the plots by a common X axis
  aligned_plots <- plot1 / plot2

  # Display the aligned plots
  print(aligned_plots)
}

plot_lemma(lemma = "Palestine", 
           date_to_plot = c("2024-04-25", "2024-06-07"))

plot_lemma(lemma = "Israel", 
           date_to_plot = c("2024-04-25", "2024-06-07"))
