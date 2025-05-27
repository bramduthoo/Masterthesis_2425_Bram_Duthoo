# 1. Inladen van packages
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)

# 2. Inlezen van de Excel
df <- read_excel("data/smaakexperiment/20250509141750+Bram_Bouillon_BEEF.xlsx", sheet = 1)
df2 <- read_excel("data/smaakexperiment/20250509140726+Bram_Bouillon_PORK.xlsx", sheet = 1)

# 3. Selecteer relevante kolommen en filter enkel samples 1 t.e.m. 6
df_clean <- df2 %>%
  filter(product %in% 1:6) %>%
  select(user, product_id = product, accept = Q5, class = Q6) %>%
  mutate(
    product_id = as.integer(product_id),
    accept = as.numeric(accept),
    class = factor(class, levels = 1:5,
                   labels = c("Beef", "Pork", "Chicken", "None", "Uncertain"))
  )

# 4. Bereken acceptatie-statistieken per sample
accept_stats <- df_clean %>%
  group_by(product_id) %>%
  summarise(
    mean_accept = mean(accept, na.rm = TRUE),
    sd = sd(accept, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = se * 1.96
  )

# 5. Bereken classificatieverdeling
class_counts <- df_clean %>%
  group_by(product_id, class) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(product_id) %>%
  mutate(prop = n / sum(n))

# 6. Plot 1: Acceptatie per sample

p <- ggplot(accept_stats, aes(x = factor(product_id), y = mean_accept)) +
  geom_point(size = 3.5, color = "#377EB8") +
  geom_errorbar(aes(ymin = mean_accept - ci95, ymax = mean_accept + ci95), width = 0.15, linewidth = 0.9) +
  geom_line(group = 1, color = "red", linewidth = 0.5) +
  scale_x_discrete(name = "Sample (addition)") +
  scale_y_continuous(name = "Mean acceptance", limits = c(1, 9), breaks = 1:9, expand = c(0, 0)) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    plot.title = element_blank()
  )
p
# Opslaan
ggsave("C:/Users/bramd_finhsgu/OneDrive - UGent/Thesis/afbeeldingen/pork_acceptance.png",
       plot = p, width = 8, height = 6, dpi = 300)

# 7. Plot 2: Classificatieverdeling per sample

# Defineer vaste kleuren
classification_colors <- c(
  "Chicken" = "#377EB8",
  "Pork" = "#FF7F00",
  "Beef" = "#E41A1C",
  "None" = "grey40",
  "Uncertain" = "black"
)

q <- ggplot(class_counts, aes(x = factor(product_id), y = prop, fill = class)) +
  geom_bar(stat = "identity", position = "stack", width = 0.85, color = "black", linewidth = 0.3) +
  scale_fill_manual(values = classification_colors, name = "Classification") +
  scale_y_continuous(name = "Proportion of responses", expand = c(0, 0)) +
  scale_x_discrete(name = "Sample (addition)") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    plot.title = element_blank()
  )
q
ggsave("C:/Users/bramd_finhsgu/OneDrive - UGent/Thesis/afbeeldingen/pork_classification.png",
       plot = q, width = 8, height = 6, dpi = 300)
