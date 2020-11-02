#' ---
#' title: "Determinants of left-right political identity"
#' author: "Sam Gardiner"
#' date: "18 October 2020"
#' output: github_document
#' ---


library(tidyverse)
library(ggstatsplot)
library(partykit)
library(ggparty)
library(table1)
library(tidygraph)
library(ggraph)
library(here)
library(ggExtra)
library(patchwork)
library(e1071)
library(corrplot)

theme_set(theme_bw())

#' # Data

wvs_q <- readRDS(here("data", "nzl_coded.RDS")) %>%
  select(matches(paste0("Q", c(1:260, 262, 269, 270, 274:278, 287, 288), "$")))


#' # Exploratory

q240_summary <- wvs_q %>%
  summarise(n = n(),
            n_missing = sum(is.na(Q240)),
            n_complete = n - n_missing,
            mean = mean(Q240, na.rm = TRUE),
            sd = sd(Q240, na.rm = TRUE),
            median = median(Q240, na.rm = TRUE),
            min = min(Q240, na.rm = TRUE),
            max = max(Q240, na.rm = TRUE),
            iqr = IQR(Q240, na.rm = TRUE))

q240_summary_sex <- wvs_q %>%
  group_by(Q260) %>%
  summarise(n = n(),
            n_missing = sum(is.na(Q240)),
            n_complete = n - n_missing,
            mean = mean(Q240, na.rm = TRUE),
            sd = sd(Q240, na.rm = TRUE),
            median = median(Q240, na.rm = TRUE),
            min = min(Q240, na.rm = TRUE),
            max = max(Q240, na.rm = TRUE),
            iqr = IQR(Q240, na.rm = TRUE))

# Summary statistics

table1(~ Q240 | Q260, data = wvs_q )

# Other moments
skewness(wvs_q$Q240, na.rm = TRUE)
kurtosis(wvs_q$Q240, na.rm = TRUE)


# Histogram and QQ plot

q240_hist <- ggplot(wvs_q, aes(Q240)) +
  geom_histogram(binwidth = 1, colour = "grey") +
  scale_x_continuous(breaks = 1:10) +
  stat_function(fun = ~dnorm(.x,
                             mean = q240_summary$mean,
                             sd = q240_summary$sd) * q240_summary$n_complete,
                size = 1) +
  labs(x = expression("Left " ~ phantom(m) %<->% phantom (m) ~ " right"),
       y = "Responses",
       subtitle = "Histogram of responses") +
  theme(panel.grid.minor = element_blank())

q240_qq <- ggplot(wvs_q, aes(sample = Q240)) +
  geom_qq(alpha = 1/10) +
  geom_qq_line() +
  labs(x = "Theoretical normal quantiles",
       y = "Sample quantiles",
       subtitle = "Normal quantile-quantile plot of responses")

q240_hist + q240_qq +
  plot_annotation(title = "Distribution of left-right political identity")


# By sex

# Comparative violin-box-dot plots
ggbetweenstats(wvs_q,
               x = Q260,
               y = Q240,
               type = "parametric",
               bf.message = FALSE,
               var.equal = TRUE,
               results.subtitle = FALSE) +
  scale_y_continuous(limits = c(1, 10), breaks = 1:10) +
  labs(x = "Sex",
       y = expression("Left " ~ phantom(m) %<->% phantom (m) ~ " right"),
       title = "Political identity by sex") +
  theme(panel.grid.minor = element_blank())

# The following figure is redundant since the density curves are shown in the
# violin plots
wvs_q %>%
  filter(!is.na(Q260)) %>%
  ggplot(aes(x = Q240, fill = Q260)) +
  geom_density(alpha = 1/2) +
  scale_x_continuous(breaks = 1:10) + 
  xlab(expression("Q240 response" ~ ("More left" %<->% " more right")))

# By age

q240_age_lm <- wvs_q %>%
  mutate(age = Q262 / 10) %>%
  lm(Q240 ~ age, data = .)
summary(q240_age_lm)
confint(q240_age_lm)

age_overall <- ggplot(wvs_q, aes(Q262, Q240)) +
  geom_jitter(width = 0, height = 1/4, alpha = 1/4) +
  geom_smooth(method = "lm") +
  scale_y_continuous(breaks = 1:10) + 
  labs(x = "Age",
       y = expression("Left " ~ phantom(m) %<->% phantom (m) ~ " right"),
       subtitle = "Overall") +
  theme(panel.grid.minor.y  = element_blank())



# By age and sex

q240_age_sex_lm <- wvs_q %>%
  mutate(age = Q262 / 10) %>%
  lm(Q240 ~ age * Q260, data = .)
summary(q240_age_sex_lm)
confint(q240_age_sex_lm)

q240_sex <- wvs_q %>%
  filter(!is.na(Q260)) %>%
  ggplot(aes(x = Q262, y = Q240, colour = Q260)) +
  geom_jitter(width = 0, height = 1/4, alpha = 1/4) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(breaks = 1:10) + 
  scale_color_discrete("Sex") +
  labs(x = "Age",
       y = NULL,
       subtitle = "Interaction with sex") +
  theme(panel.grid.minor.y  = element_blank(),
        legend.position = "bottom")


age_overall + q240_sex +
  theme(legend.position = "right") +
  plot_annotation("Association between age and political identity")

# By self-reported income

ggplot(wvs_q, aes(Q288, Q240)) +
  geom_jitter(alpha = 1/5, width = 0.1, height = 0.1) +
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(breaks = 1:10) +
  theme(panel.grid.minor = element_blank()) +
  labs(title = "Political identity by income",
       x = expression("Self-reported income decile (low"  %<->%  "high)"),
       y = expression("Left" ~ phantom(m) %<->% phantom(m) ~ "right"))

q240_income_lm <- lm(Q240 ~ Q288, data = wvs_q)
summary(q240_income_lm)
confint(q240_income_lm)

# By level of education

wvs_educ <- wvs_q %>%
  mutate(across(matches(paste0("Q", 275:278)),
                function(x) {
                  x_int <- as.integer(x)
                  factor(if_else(x_int <= 4,
                                 "High school",
                                 "Post high school"))
                }))

ggbetweenstats(wvs_educ,
               x = Q275,
               y = Q240,
               type = "parametric",
               bf.message = FALSE,
               var.equal = FALSE,
               results.subtitle = FALSE) +
  scale_y_continuous(limits = c(1, 10), breaks = 1:10) +
  labs(x = "Education",
       y = expression("Left " ~ phantom(m) %<->% phantom (m) ~ " right"),
       title = "Political identity by education") +
  theme(panel.grid.minor = element_blank())

t.test(Q240 ~ Q275, data = wvs_educ, var.equal = FALSE)


# Graphical exploration of correlation

wvs_q_cor <- readRDS(here("data", "cor_ext.RDS"))$correlations %>%
  as_tibble(rownames = "from") %>%
  pivot_longer(cols = -from, names_to = "to")

cor_subset <- wvs_q_cor %>%
  filter(to == "Q240") %>%
  slice_max(abs(value), n = 20) %>%
  mutate(qnum = as.numeric(gsub("Q", "", from))) %>%
  arrange(qnum) %>%
  pull(from)

cor_subset_labels = if_else(cor_subset == "Q240",
                           expression(bold(cor_subset)),
                           expression(plain(cor_subset)))

cor_subset_labels <- map(cor_subset,
                         function(x) {
                           if (x == "Q240") {
                             bquote(bolditalic(.(x)))
                           } else {
                             bquote(plain(.(x)))
                           }
                         })

# ggplot heatmap
wvs_q_cor %>%
  filter(from %in% cor_subset,
         to %in% cor_subset) %>%
  mutate(across(.cols = c(from, to),
                ~factor(.x,
                        levels = rev(cor_subset),
                        labels = rev(cor_subset)))) %>%
  ggplot(aes(fct_rev(from), to, fill = value)) +
  geom_raster() +
  coord_equal() +
  scale_fill_distiller(palette = "RdBu",
                       limits = c(-1, 1),
                       name = "Generalised\ncorrelation") +
  scale_x_discrete(position = "top", labels = cor_subset_labels) +
  scale_y_discrete(labels = rev(cor_subset_labels)) +
  theme(axis.text.x = element_text(angle = 90, vjust = -1),
        legend.key.height = unit(10, "mm")) +
  labs(x = NULL, y = NULL)

# corrplot heatmap
wvs_cor_mat <- wvs_q_cor %>%
  filter(from %in% cor_subset,
         to %in% cor_subset) %>%
  pivot_wider(names_from = "to") %>%
  column_to_rownames("from") %>%
  as.matrix()

corrplot(wvs_cor_mat, method = "color", type = "full", tl.col = "black")

# Variable selection
# Very few complete cases.
wvs_q %>%
  na.omit() %>%
  nrow()


