library(dplyr)
library(ggplot2)
library(readxl)
library(broom)
library(plotly)
library(RColorBrewer)

# 1. Inlezen en mapping
df <- read_excel("data/smaakexperiment/20250509141841+Bram_Bouillon_CHKN.xlsx", sheet = 1)

product_map <- tibble::tibble(
  product_id = 1:7,
  cysteine = c(1.0, 0.11, 0.11, 1.12, 0.6, 0.11, 1.12),
  dextrose = c(50, 6, 60, 6, 33, 33, 33),
  combi_label = c("high-high", "low-low", "low-high", "high-low", "med-med", "low-med", "high-med")
)

# 2. Data opschonen en verrijken
df_clean <- df %>%
  transmute(
    user,
    product_id = as.numeric(product),
    accept_score = as.numeric(Q5),
    Q6 = as.numeric(Q6),
    Q7 = as.numeric(Q7),
    chicken = case_when(
      Q6 == 1 ~ "Yes",
      Q6 == 2 ~ "No",
      Q6 == 3 ~ "Uncertain",
      TRUE ~ NA_character_
    ),
    diet = case_when(
      Q7 == 1 ~ "Omnivore",
      Q7 == 2 ~ "Flexitarian",
      Q7 == 3 ~ "Pescatarian",
      Q7 == 4 ~ "Vegetarian",
      Q7 == 5 ~ "Vegan",
      TRUE ~ "Unknown"
    )
  ) %>%
  filter(product_id %in% 1:7, !is.na(accept_score), !is.na(Q6)) %>%
  left_join(product_map, by = "product_id")

# 3. Statistieken per sample
stats_per_sample <- df_clean %>%
  group_by(product_id, cysteine, dextrose) %>%
  summarise(
    mean_accept = mean(accept_score),
    sd = sd(accept_score),
    n = n(),
    se = sd / sqrt(n),
    ci95 = se * qt(0.975, df = n - 1),
    min = min(accept_score),
    max = max(accept_score),
    n_yes = sum(chicken == "Yes"),
    n_no = sum(chicken == "No"),
    n_uncertain = sum(chicken == "Uncertain"),
    prop_yes = n_yes / n,
    .groups = "drop"
  )

print(stats_per_sample)
stats_per_sample$product_id <- as.factor(stats_per_sample$product_id)

# 4. Model A: alleen cysteine
model_cys <- lm(mean_accept ~ cysteine, data = stats_per_sample)
summary(model_cys)$r.squared

g <- ggplot(stats_per_sample, aes(x = cysteine, y = mean_accept, color = as.factor(product_id))) +
  geom_point(size = 3.5) +
  geom_errorbar(aes(ymin = mean_accept - ci95, ymax = mean_accept + ci95), 
                width = 0.03, linewidth = 0.9) +
  geom_text(aes(label = product_id),
            hjust = -0.2, vjust = -0.8, size = 5, fontface = "bold", color = "black", show.legend = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", linewidth = 1) +
  scale_y_continuous(name = "Mean acceptance", limits = c(1, 9), breaks = 1:9, expand = c(0, 0)) +
  scale_x_continuous(name = "Cysteine (g)", expand = c(0.01, 0)) +
  scale_color_brewer(palette = "Accent", name = "Sample") +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)) +  # Legenda op één rij
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),# Zet de assen aan
    axis.ticks = element_line(color = "black"),# Zet de tick marks aan
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    legend.spacing.x = unit(0.8, 'cm')  # Extra ruimte tussen legenda-items
  )
g
# Opslaan
ggsave("C:/Users/bramd_finhsgu/OneDrive - UGent/Thesis/afbeeldingen/cys.png",
       plot = g, width = 8, height = 6, dpi = 300)

# 5. Model B: alleen dextrose
model_dex <- lm(mean_accept ~ dextrose, data = stats_per_sample)
summary(model_dex)$r.squared

p <- ggplot(stats_per_sample, aes(x = dextrose, y = mean_accept, color = as.factor(product_id))) +
  geom_point(size = 3.5) +
  geom_errorbar(aes(ymin = mean_accept - ci95, ymax = mean_accept + ci95), 
                width = 1, linewidth = 0.9) +
  geom_text(aes(label = product_id, hjust = -0.35, vjust = ifelse(product_id %in% c(4), 0.8, ifelse(product_id %in% c(6), 1.2, -0.8))),
            size = 5, fontface = "bold", color = "black", show.legend = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", linewidth = 1) +
  scale_y_continuous(name = "Mean acceptance", limits = c(1, 9), breaks = 1:9, expand = c(0, 0)) +
  scale_x_continuous(name = "Dextrose (g)", expand = c(0.01, 0)) +
  scale_color_brewer(palette = "Accent", name = "Sample") +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)) +  # Legenda op één rij
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),# Zet de assen aan
    axis.ticks = element_line(color = "black"),# Zet de tick marks aan
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    legend.spacing.x = unit(0.8, 'cm')  # Extra ruimte tussen legenda-items
  )
p
# Opslaan
ggsave("C:/Users/bramd_finhsgu/OneDrive - UGent/Thesis/afbeeldingen/dex.png",
       plot = p, width = 8, height = 6, dpi = 300)


# 6. Model C: beide
model_both <- lm(mean_accept ~ cysteine + dextrose, data = stats_per_sample)
summary(model_both)$r.squared

#
q <- plot_ly()

# Per sample een balk toevoegen
for (i in 1:nrow(stats_per_sample)) {
  s <- stats_per_sample[i, ]
  
  q <- q %>%
    add_trace(
      type = "scatter3d",
      mode = "lines",
      x = c(s$cysteine, s$cysteine),
      y = c(s$dextrose, s$dextrose),
      z = c(0, s$mean_accept),
      line = list(
        width = 10,
        color = s$mean_accept,
        colorscale = "Viridis"
      ),
      name = paste("Sample", s$product_id),
      showlegend = FALSE
    )
}

# Layout toevoegen
q <- q %>%
  layout(
    scene = list(
      xaxis = list(title = "Cysteïne (g)"),
      yaxis = list(title = "Dextrose (g)"),
      zaxis = list(title = "Mean acceptance", range = c(0, 9), tickvals = 1:9)
    )
  )

q <- q %>%
  add_trace(
    type = "scatter3d",
    mode = "text",
    x = stats_per_sample$cysteine,
    y = stats_per_sample$dextrose,
    z = stats_per_sample$mean_accept + 0.3,  # Klein beetje erboven
    text = paste("Sample", stats_per_sample$product_id),
    textposition = "top center",
    showlegend = FALSE,
    textfont = list(size = 12, color = "black")
  )

q


#

q <- plot_ly(
  stats_per_sample,
  x = ~cysteine,
  y = ~dextrose,
  z = ~mean_accept,
  text = ~paste("Sample:", product_id),
  type = "scatter3d",
  mode = "markers+text",
  marker = list(size = 5, color = ~mean_accept, colorscale = 'Viridis', showscale = TRUE),
  textposition = "top center"
) %>%
  layout(
    scene = list(
      xaxis = list(title = "Cysteïne (g)"),
      yaxis = list(title = "Dextrose (g)"),
      zaxis = list(title = "Mean acceptance", range = c(1, 9), tickvals = 1:9, tickfont = list(size = 10))
    )
  )

q

# 7. Binomiale toets
binomial_results <- stats_per_sample %>%
  rowwise() %>%
  mutate(
    p_value = binom.test(n_yes, n, p = 1/3, alternative = "greater")$p.value,
    significant = p_value < 0.05
  )

print(binomial_results %>%
        select(product_id, cysteine, dextrose, n_yes, n_no, n_uncertain, prop_yes, p_value, significant))

# 8. Gedetailleerde antwoorden + dieet voor sample 1
ref_all_answers <- df_clean %>%
  filter(product_id == 1) %>%
  select(user, chicken, diet)

print(ref_all_answers)

# 9. Aantallen per dieetcategorie
diet_counts <- df_clean %>%
  distinct(user, diet) %>%
  count(diet, name = "n_participants") %>%
  arrange(desc(n_participants))

print(diet_counts)
