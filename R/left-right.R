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

theme_set(theme_bw())

#' # Data

wvs_q <- readRDS(here("data", "nzl_coded.RDS")) %>%
  select(matches(paste0("Q", c(1:260, 262, 269, 270, 274, 287, 288), "$")))


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

# Table 1

table1(~ Q240 | Q260, data = wvs_q )

# Histogram

ggplot(wvs_q, aes(Q240)) +
  geom_histogram(binwidth = 1, colour = "black") +
  scale_x_continuous(breaks = 1:10) +
  geom_vline(xintercept = q240_summary$mean,
             linetype = "dashed",
             size = 2,
             colour = "firebrick") +
  geom_vline(xintercept = q240_summary$median,
             linetype = "dotted",
             size = 2,
             colour = "cornflowerblue") +
  annotate(geom = "label",
           x = q240_summary$mean,
           y = 75,
           label = str_glue("Mean = {round(q240_summary$mean, 2)}"),
           colour = "firebrick") +
  annotate(geom = "label",
           x = q240_summary$median,
           y = 50,
           label = str_glue("Median = {q240_summary$median}"),
           colour = "cornflowerblue") +
  stat_function(fun = ~dnorm(.x,
                             mean = q240_summary$mean,
                             sd = q240_summary$sd) * q240_summary$n_complete,
                size = 2) +
  xlab(expression("Q240 response" ~ ("More left " %<->% " More right")))

# By sex

ggbetweenstats(wvs_q,
               x = Q260,
               y = Q240,
               type = "parametric",
               bf.message = FALSE,
               var.equal = TRUE) +
  scale_y_continuous(limits = c(1, 10), breaks = 1:10) +
  labs(x = "Sex", y = expression(bold("Left " %<->% " right")))

wvs_q %>%
  filter(!is.na(Q260)) %>%
  ggplot(aes(x = Q240, fill = Q260)) +
  geom_density(alpha = 1/2) +
  scale_x_continuous(breaks = 1:10) + 
  xlab(expression("Q240 response" ~ ("More left" %<->% " more right")))

# By age

age_stats <- ggscatterstats(wvs_q,
                            x = Q262,
                            y = Q240,
                            bf.message = FALSE,
                            xlab = "Age",
                            marginal = NULL,
                            ylab = expression(bold("Left " %<->% " right")),
                            output = "subtitle")

ggplot(wvs_q, aes(Q262, Q240)) +
  geom_point(alpha = 1/4) +
  geom_smooth(method = "lm") +
  scale_y_continuous(breaks = 1:10) + 
  labs(x = "Age",
       y = expression("Left " %<->% " right"),
       subtitle = age_stats,
       title = "Age and political identity") +
  theme(panel.grid.minor.y  = element_blank())
  


# By age and sex

age_sex_stats <- grouped_ggscatterstats(wvs_q,
                       x = Q262,
                       y = Q240,
                       grouping.var = Q260,
                       marginal = NULL,
                       bf.message = FALSE,
                       xlab = "Age",
                       ylab = expression(bold("Left " %<->% ~ " right")),
                       output = "subtitle")


stats_subtitle <- (age_sex_stats)

str(age_sex_stats)

wvs_q %>%
  filter(!is.na(Q260)) %>%
  ggplot(aes(x = Q262, y = Q240, colour = Q260)) +
  geom_point(alpha = 1/4) +
  geom_smooth(method = "lm", se = FALSE) +
  annotate("text",
           x = 25, y = 8.5,
           label = age_sex_stats$Male,
           hjust = 0,
           colour = "#f8766d") +
  annotate("text",
           x = 25, y = 2.5,
           label = age_sex_stats$Female,
           hjust = 0,
           colour = "#00bfc4") +
  scale_y_continuous(breaks = 1:10) + 
  scale_color_discrete("Sex") +
  labs(x = "Age",
       y = expression("Left " %<->% " right"),
       title = "Age and political identity by sex") +
  theme(panel.grid.minor.y  = element_blank(),
        legend.position = "bottom")



# By self-reported income

ggscatterstats(wvs_q,
               x = Q288, 
               y = Q240,
               bf.message = FALSE,
               xlab = expression("Q288: Self-reported income decile (low " %<->% " high)"),
               ylab = expression(bold("Q240: " ~ "Left " %<->% " right")))


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

# Variable selection
# Very few complete cases.
wvs_q %>%
  na.omit() %>%
  nrow()


