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
library(tidygraph)
library(ggraph)

theme_set(theme_bw())

#' # Helper functions:

# Make ordinal variables nominal, with options vector of exceptions.
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

#' # Data
wvs_q <- readRDS(here("data", "nzl_coded.RDS")) %>%
  select(matches(paste0("Q", c(1:260, 262, 269, 270, 274, 287, 288), "$")))

#' Split
set.seed(20201010)
training <- sample(c(TRUE, FALSE),
                      size = nrow(wvs_q),
                      prob = c(0.7, 0.3),
                      replace = TRUE)

#' # Build trees

# Inference tree control
ctrl <- ctree_control(alpha = 0.01,
                      MIA = FALSE,
                      minbucket = 10)

# Fit a conditional inference tree for _every_ variable.
responses <- names(wvs_q) %>%
  set_names()

tictoc::tic("Sequential map")
trees <- map(responses,
             function(response, data, subset, control) {
               f <- reformulate(".", response) 
               prep(data, {{response}}, subset) %>%
                 ctree(f, ., control = ctrl)
             },
             data = wvs_q,
             subset = training,
             control = ctrl)
tictoc::toc()

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

# What predicts political interest?

# What predicts revolutionaries?

#' ggparty plot
#' 
#' # ggparty(wvs_ctree_207) +
#   geom_edge() +
#   geom_edge_label(size = 2.5) +
#   geom_node_splitvar(size = 2.5) +
#   geom_node_plot(gglist = list(geom_bar(aes(x = Q207)),
#                                labs(y = NULL)),
#                  scales = "free_y")

# Produce a variable importance network.

trees_varimp <- map(trees, varimp, nperm = 1)

trees_nodes <- tibble(name = names(trees)) %>%
  mutate(id = row_number())

trees_nodes[trees_nodes$name %in% c("Q1", "Q2"), "id"]

trees_edges <- imap(trees_varimp,
                    safely(function(predictors, response, lookup) {
                      from <- lookup[lookup$name %in% names(predictors), "id"]
                      to <- lookup[lookup$name == response, "id"]
                      tibble(from = from$id, to = to$id, weight = predictors)
                    }),
                    lookup = trees_nodes) %>%
  map_dfr("result")


trees_graph <- tbl_graph(nodes = trees_nodes,
                         edges = trees_edges)

ggraph(trees_graph,) +
  geom_edge_link(arrow = arrow(), aes(edge_width = weight, alpha = weight)) +
  geom_node_label(aes(label = name))

