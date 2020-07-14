################################################################################
# Commute data play
#
# Author: Stefan Schliebs
# Created: 2020-06-21 08:45:49
################################################################################

library(tidyverse)
library(visNetwork)



# Config ------------------------------------------------------------------

F_COMMUTE_DATA <- "data/2018-census-main-means-of-travel-to-work-by-statistical-area.csv"



# Data --------------------------------------------------------------------

d_commute <- read_csv(F_COMMUTE_DATA) %>% 
  janitor::clean_names()

# replace all occurrences of -999 with NA
d_commute_clean <- 
  d_commute %>% 
  mutate(across(everything(), ~ifelse(.x == -999, NA, .x))) %>% 
  mutate(total_commute = ifelse(is.na(work_at_home), total, total - work_at_home))

View(d_commute_clean)



# Network -----------------------------------------------------------------

d_commute_network <- 
  d_commute_clean %>% 
  filter(sa2_code_usual_residence_address != sa2_code_workplace_address, total_commute > 100)

# d_nodes <-
#   bind_rows(
#     d_commute_network %>% 
#       count(sa2_code_usual_residence_address, sa2_name_usual_residence_address) %>% 
#       select(id = sa2_code_usual_residence_address, label = sa2_name_usual_residence_address),
#     d_commute_network %>% 
#       count(sa2_code_workplace_address, sa2_name_workplace_address) %>% 
#       select(id = sa2_code_workplace_address, label = sa2_name_workplace_address)
#   ) %>% 
#   distinct()

d_nodes <-
  d_commute_network %>% 
  group_by(sa2_code_usual_residence_address, sa2_name_usual_residence_address) %>%
  summarise(total_commute = sum(total_commute, na.rm = TRUE), .groups = "drop") %>% 
  mutate(n = 10 * log(total_commute)) %>% 
  select(
    id = sa2_code_usual_residence_address, 
    label = sa2_name_usual_residence_address,
    value = n
  ) %>% 
  arrange(desc(value))

d_edges <- 
  d_commute_network %>% 
  select(
    from = sa2_code_usual_residence_address,
    to = sa2_code_workplace_address,
    value = total_commute
  )
  
visNetwork(
  nodes = d_nodes,
  edges = d_edges
) %>% 
  visPhysics(
    solver = "forceAtlas2Based"
    # timestep = 1,
    # minVelocity = 1,
    # maxVelocity = 30,
    # forceAtlas2Based = list(gravitationalConstant = -800, damping = 1),
    # stabilization = list(iterations = 100, updateInterval = 10),
    # adaptiveTimestep = TRUE
  )
  
 View(d_nodes)
