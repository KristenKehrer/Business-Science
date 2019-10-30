# LEARNING LAB 19 ----
# NETWORK DETECTION ----
# CUSTOMER CREDIT CARD HISTORY
# Kaggle Data: https://www.kaggle.com/arjunbhasin2013/ccdata


# LIBRARIES ----
library(tidyverse)
library(tidyquant)

library(DataExplorer)
library(correlationfunnel)

library(recipes)

library(tidygraph)
library(ggraph)


# 1.0 DATA ----
credit_card_tbl <- read_csv("data/CC GENERAL.csv")

credit_card_tbl %>% glimpse()


# 2.0 EXPLORATORY DATA ANALYSIS ----

plot_missing(credit_card_tbl)

# 2.1 Minimum Payments has NA (missing data) 

credit_card_tbl %>%
    pull(MINIMUM_PAYMENTS) %>%
    quantile(na.rm = TRUE)

credit_card_no_missing_tbl <- credit_card_tbl %>%
    select_if(is.numeric) %>%
    filter(!is.na(MINIMUM_PAYMENTS)) %>%
    filter(!is.na(CREDIT_LIMIT)) 

credit_card_no_missing_tbl %>%
    binarize() %>%
    correlate(target = MINIMUM_PAYMENTS__825.49646275_Inf) %>%
    plot_correlation_funnel()

credit_card_tbl %>%
    ggplot(aes(BALANCE, MINIMUM_PAYMENTS)) + 
    geom_point(alpha = 0.25) +
    geom_smooth(method = "lm")

# 2.2 Credit Limit has NA (missing data) 

credit_card_tbl %>%
    pull(CREDIT_LIMIT) %>%
    quantile(na.rm = TRUE)

credit_card_no_missing_tbl %>%
    binarize() %>%
    correlate(target = CREDIT_LIMIT__6500_Inf) %>%
    plot_correlation_funnel()

credit_card_tbl %>%
  ggplot(aes(CREDIT_LIMIT, BALANCE)) + 
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm") 


# 3.0 PREPROCESSING -----
rec_obj <- recipe(~ ., data = credit_card_tbl) %>%
    # Random Forest Imputation
    step_bagimpute(MINIMUM_PAYMENTS, CREDIT_LIMIT) %>%
    # Scale & Center for relationship analysis
    step_center(all_numeric()) %>%
    step_scale(all_numeric()) %>%
    prep()

train_tbl <- bake(rec_obj, new_data = credit_card_tbl)

train_tbl %>% glimpse()


# 5.0 ADJACENCY MATRIX ----

customer_correlation_matrix <- train_tbl %>%
    
    # Transpose data for customer similarity
    gather(key = "FEATURE", value = "value", -CUST_ID, factor_key = TRUE) %>%
    spread(key = CUST_ID, value = value) %>%
    select(-FEATURE) %>%
    
    # Adjacency Matrix - Perform Similarity using Correlation
    as.matrix() %>%
    cor() 

customer_correlation_matrix %>% class()
customer_correlation_matrix %>% as_tibble(rownames = "CUST_ID")

# 6.0 PRUNING THE ADJACENCY MATRIX ----

# 6.1 Remove customer relationships to themselves
diag(customer_correlation_matrix) <- 0
customer_correlation_matrix %>% as_tibble(rownames = "CUST_ID")

# 6.2 Remove duplicate relationships 
customer_correlation_matrix[upper.tri(customer_correlation_matrix)] <- 0
customer_correlation_matrix %>% as_tibble(rownames = "CUST_ID")

# 6.3 Prune relationships
edge_limit <- 0.99
customer_correlation_matrix[customer_correlation_matrix < edge_limit] <- 0
customer_correlation_matrix %>% as_tibble(rownames = "CUST_ID")

sum(customer_correlation_matrix > 0)


# 6.4 Filter relationships to subset of customers that have relationships
customer_correlation_matrix <- customer_correlation_matrix[rowSums(customer_correlation_matrix) > 0, colSums(customer_correlation_matrix) > 0] 
customer_correlation_matrix %>% dim()

customer_correlation_matrix %>% as_tibble(rownames = "CUST_ID")


# 6.5 Convert to Long Tibble with From & To Column Relating Customers
customer_relationship_tbl <- customer_correlation_matrix %>%
    as_tibble(rownames = "from") %>%
    gather(key = "to", value = "weight", -from) %>%
    filter(weight > 0)

customer_relationship_tbl


# 7.0 WORKFLOW - Convert to Function for Dynamic Filtering of Edge Limit ----

prep_corr_matrix_for_tbl_graph <- function(correlation_matrix, edge_limit = 0.9999) {
    
    diag(correlation_matrix) <- 0
    
    correlation_matrix[upper.tri(correlation_matrix)] <- 0

    correlation_matrix[correlation_matrix < edge_limit] <- 0
    
    correlation_matrix <- correlation_matrix[rowSums(correlation_matrix) > 0, colSums(correlation_matrix) > 0] 
    
    correlation_matrix %>%
        as_tibble(rownames = "from") %>%
        gather(key = "to", value = "weight", -from) %>%
        filter(weight > 0)

}

prep_corr_matrix_for_tbl_graph(customer_correlation_matrix, edge_limit = 0.9999)


# 7.0 NETWORK VISUALIZATION ----

customer_correlation_matrix %>%
    
    prep_corr_matrix_for_tbl_graph(edge_limit = 0.99) %>%
    
    as_tbl_graph(directed = FALSE) %>%
    
    ggraph(layout = "kk") +
    geom_edge_link(alpha = 0.5, color = palette_light()["blue"]) +
    geom_node_point(alpha = 0.5, color = palette_light()["blue"]) +
    theme_graph(background = "white")




# 8.0 TBL GRAPH MANIPULATION ----
customer_tbl_graph <- customer_correlation_matrix %>%
    prep_corr_matrix_for_tbl_graph(edge_limit = 0.997) %>%
    as_tbl_graph(directed = FALSE)

customer_tbl_graph

# 8.1 Ranking Nodes - Rank by topological traits
customer_tbl_graph %>%
    activate(nodes) %>%
  
    mutate(node_rank = node_rank_traveller()) %>%
    arrange(node_rank)
    
    
# 8.2 Centrality - Number of edges going in/out of node
customer_tbl_graph %>%
    activate(nodes) %>%
  
    mutate(neighbors = centrality_degree()) %>%
    arrange(desc(neighbors))


# 8.3 Grouping Nodes (Clustering)
?group_graph

grouped_tbl_graph <- customer_tbl_graph %>%
    activate(nodes) %>%
    mutate(neighbors = centrality_degree()) %>%
    
    mutate(group = group_components()) %>%
    
    arrange(desc(neighbors)) %>%
    mutate(group_lump = group %>% as_factor() %>% fct_lump(n = 5))


grouped_tbl_graph %>%
    ggraph(layout = "kk") +
    geom_edge_link(alpha = 0.5) +
    geom_node_point(aes(color = group_lump), alpha = 0.5, size = 3) +
    
    theme_graph() +
    scale_color_tq(theme = "light") +
    theme(legend.position = "bottom") +
    labs(title = "Customer Network Detection")



# 9.0 COMMUNITY ANALYSIS ----
# - Join Communities and Inspect Key Features

credit_card_group_tbl <- credit_card_tbl %>%
    left_join(as_tibble(grouped_tbl_graph), by = c("CUST_ID" = "name")) %>%
    select(group_lump, CUST_ID, everything()) %>%
    filter(!is.na(group_lump))

credit_card_group_tbl %>% glimpse()


plot_density_by <- function(data, col, group_focus = 1, ncol = 1) {
    
    col_expr <- enquo(col)
    
    data %>%
        mutate(focus = as.character(group_lump)) %>%
        select(focus, everything()) %>%
        mutate(focus = ifelse(as.character(focus) == as.character(group_focus), 
                                    "1", "Other")) %>%
        mutate(focus = as.factor(focus)) %>%
        
        ggplot(aes(!! col_expr, fill = focus)) +
        geom_density(alpha = 0.4) +
        facet_wrap(~ focus, ncol = ncol) +
        scale_fill_tq() +
        theme_tq()
}

# 9.1 GROUP 1 -----

# By Balance
credit_card_group_tbl %>% plot_density_by(BALANCE, group_focus = 1)

# By Min Payment
credit_card_group_tbl %>% plot_density_by(MINIMUM_PAYMENTS, group_focus = 1)
credit_card_group_tbl %>% plot_density_by(log(MINIMUM_PAYMENTS), group_focus = 1)

# Cash Advance Frequency
credit_card_group_tbl %>% plot_density_by(CASH_ADVANCE_FREQUENCY, group_focus = 1)

# 9.2 GROUP 2 ----

# By Balance
credit_card_group_tbl %>% plot_density_by(BALANCE, group_focus = 2, ncol = 1)
credit_card_group_tbl %>% plot_density_by(log(BALANCE), group_focus = 2, ncol = 1)

# By Min Payment
credit_card_group_tbl %>% plot_density_by(MINIMUM_PAYMENTS, group_focus = 2)
credit_card_group_tbl %>% plot_density_by(log(MINIMUM_PAYMENTS), group_focus = 2)

# Cash Advance Frequency
credit_card_group_tbl %>% plot_density_by(CASH_ADVANCE_FREQUENCY, group_focus = 2)


  

# 10.0 LLPRO BONUS - H2O + LIME ----
# - Learn H2O & LIME IN DS4B 201-R

# BONUS!!!
source("LLPRO_BONUS_h2o_lime_bonus.R")

# BEHIND THE SCENES PERFORMING:
# - H2O 
#   - Multi-class prediction with H2O AutoML 
#   - Training a model to detect which group each customer belongs to
# - LIME EXPLANATION
#   - ML Explanation of which features contribute to each class with LIME
#   - Returns a function called explain_customer()

credit_card_group_tbl

h2o.predict(h2o_model, newdata = as.h2o(credit_card_group_tbl)) %>%
  as_tibble()

explain_customer(6)
