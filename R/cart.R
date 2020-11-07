#' ---
#' title: "CART"
#' author: "Sam Gardiner"
#' date: "10 October 2020"
#' output: github_document
#' ---

# packages
library(tidyverse)
library(here)
library(partykit)
library(ggparty)
library(ggfortify)
library(gtsummary)

theme_set(theme_bw())

#' # Helper functions:

# Make ordinal variables nominal, with optional vector of exceptions.
disorder <- function(data, except = NULL) {
  data %>%
    mutate(across(c(where(is.ordered),
                    -{{except}}),
                  ~factor(.x, ordered = FALSE)))
}

# Reverse the order of given ordinal variables.
reverse_order <- function(data, factors) {
  data %>%
    mutate(across(.cols = {{factors}},
                  ~fct_rev(.x)))
}

# Prepare dataset and response for modelling, depending on response type:
# (ordinal)
prep.ordered <- function(data, response, subset) {
  data %>%
    disorder(except = {{response}}) %>%
    reverse_order({{response}}) %>%
    subset(subset) %>%
    filter(across(.cols = {{response}},
                  ~!is.na(.x)))
}

# (factors and numeric)
prep.default <- function(data, response, subset) {
  data %>%
    disorder() %>%
    subset(subset) %>%
    filter(across(.cols = {{response}},
                  ~!is.na(.x)))
}

# the generic:
prep <- function(data, response, subset) {
  UseMethod(generic = "prep",
            object = pull(data, response))
}

# Root-mean-square-error:
rmse <- function(observed, predicted){
  sqrt(mean((observed - predicted)^2,
            na.rm = TRUE))
}

#' # Data
wvs_q <- readRDS(here("data", "nzl_coded.RDS")) %>%
  select(matches(paste0("Q", c(1:222, 224:260, 262:265, 269:280,
                               284:290), "$"))) %>%
  mutate(across(matches(paste0("Q", 275:278)),
                function(x) {
                  x_int <- as.integer(x)
                  factor(if_else(x_int <= 4,
                                 "High school",
                                 "Post high school"))
                }),
         across(c(Q279, Q280),
                ~fct_collapse(.x,
                              "Employed" = c("Full time (30 hours a week or more)",
                                             "Part time (less than 30 hours a week)",
                                             "Self employed"),
                              "Student" = "Student",
                              other_level = "Unemployed")))

#' Split
set.seed(20201010)
training <- sample(c(TRUE, FALSE),
                   size = nrow(wvs_q),
                   prob = c(0.7, 0.3),
                   replace = TRUE)

#' # Build trees

# Inference tree control
ctrl <- ctree_control(alpha = 0.05,
                      MIA = FALSE,
                      minbucket = 10)

# Fit a conditional inference tree for _every_ variable.

responses <- names(wvs_q) %>%
  set_names()

trees <- map(responses,
             function(response, data, subset, control) {
               f <- reformulate(".", response) 
               prep(data, {{response}}, subset) %>%
                 ctree(f, ., control = ctrl)
             },
             data = wvs_q,
             subset = training,
             control = ctrl)

# Extract important splits from each tree

trees_info <- map(trees,
                  safely(function(tree) {
                    ggparty:::get_plot_data(tree) %>%
                      select(-starts_with("nodedata"))
                  }))

trees_errors <- keep(trees_info, ~!is.null(.x$error))

trees_table <- map_dfr(trees_info, "result", .id = "model")

#' # Query our models of interest

#' Which trees make use of the news source variables?

trees_table %>%
  filter(splitvar %in% paste0("Q", 201:208))

#' What does social media consumption predict?

trees_table %>%
  filter(splitvar == "Q207")

#' TV?

trees_table %>%
  filter(splitvar == "Q202")

plot(trees$Q179)


# ==============================================================================


# What predicts left-right political identity?

# Fit a single model for LR identity

lr_ctrl <- ctree_control(alpha = 0.01,
                         MIA = FALSE,
                         minbucket = 10)

lr_tree <- prep(wvs_q, "Q240", training) %>%
  ctree(Q240 ~ ., data = ., control = lr_ctrl)

plot(lr_tree)

lr_data <- ggparty:::get_plot_data(lr_tree)

ggparty(lr_tree) +
  geom_edge() +
  geom_edge_label(
  ) +
  geom_node_label(
    # Inner nodes
    line_list = list(
      aes(label = splitvar),
      aes(label = scales::pvalue(p.value, add_p = TRUE))
    ),
    line_gpar = list(
      list(size = 13),
      list(size = 8)
    ),
    ids = "inner"
  ) +
  geom_node_plot(
    # Terminal plots
    gglist = list(
      geom_boxplot(aes(y = Q240),
                   fill = "grey"),
      scale_y_continuous(breaks = 1:10),
      theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank()),
      labs(y = expression("Left " ~ phantom(m) %<->% phantom (m) ~ " right"))
    ),
    shared_axis_labels = TRUE
  ) +
  geom_node_label(
    # Terminal nodes
    aes(label = paste0("n = ", nodesize)),
    ids = "terminal",
    nudge_x = 0.02,
    nudge_y = -0.01
  )



# Extract partitions
Q240_train <- prep(wvs_q, "Q240", training)
Q240_test <- prep(wvs_q, "Q240", !training)

# Fit ctrees for varying alpha and minbucket:
alphas <- c(0.1, 0.05, 0.01, 0.005, 0.001) %>% set_names()

vary_alpha <- map(alphas,
                  function(alpha, formula, data){
                    alpha_ctrl <- ctree_control(alpha = alpha,
                                                MIA = FALSE,
                                                minbucket = 10)
                    ctree(formula, data = data, control = alpha_ctrl)
                  },
                  formula = Q240 ~ .,
                  data = Q240_train)

# Tabulate inner nodes, terminal nodes, training RMSE
train_table <- imap_dfr(vary_alpha,
                        function(model, alpha, observed, newdata){
                          list(
                            alpha = alpha,
                            nodes = length(model),
                            terminal = width(model),
                            depth = depth(model),
                            RMSE = rmse(observed = observed$Q240, 
                                        predicted = predict(model, newdata = newdata))
                          )},
                        observed = Q240_train,
                        newdata = Q240_train)

knitr::kable(train_table, digits = 3) %>%
  kableExtra::kable_classic()

# ctree Testing RMSE:
rmse(observed = Q240_test$Q240,
     predicted = predict(lr_tree, newdata = Q240_test))

# ctree training observations fit
562 #sum of node sizes
nrow(Q240_train)

# ctree testing observations fit
test_obs <- sum(!is.na(predict(lr_tree, newdata = Q240_test)))
test_obs_extant <- sum(!is.na(Q240_test$Q240))


# Variable importance
varimp(lr_tree, nperm = 1000)

# Does a linear regression do as well with the same variables?
Q240_lm <- lm(Q240 ~ Q106 + relevel(Q211, ref = 2) + Q252 + fct_rev(Q68) + relevel(Q36, ref =3),
              data = Q240_train)

summary(Q240_lm)

# Training RMSE:
rmse(observed = Q240_train$Q240,
     predicted = predict(Q240_lm, newdata = Q240_train))

# Testing RMSE:
rmse(observed = Q240_test$Q240,
     predicted = predict(Q240_lm, newdata = Q240_test))

lm_obs <- sum(!is.na(predict(Q240_lm, newdata = Q240_train)))
lm_obs
lm_obs / nrow(Q240_train)

lm_test_obs <- sum(!is.na(predict(Q240_lm, newdata = Q240_test)))
lm_test_obs
lm_test_obs / nrow(Q240_test)

# Linear model diagnostic plot
autoplot(Q240_lm, which = c(1:3, 5), label = FALSE, alpha = 1/4)

# Linear model summary table

tbl_regression(Q240_lm)
