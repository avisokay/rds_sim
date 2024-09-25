library(igraph)
library(ggplot2)
library(data.table)
library(extrafont)

# for fonts
# font_import()  # Will import all system fonts, including Arial
# loadfonts(device = "win")  # For Windows


# --- GENERATE NETWORK -----------

# Create a graph with 10,000 nodes
num_nodes <- 10000
g <- make_empty_graph(n = num_nodes, directed = FALSE)

# Assign age distribution to nodes with a normal distribution between 18 and 65
set.seed(123)  # Set seed for reproducibility
ages <- round(rnorm(num_nodes, mean = 41.5, sd = 10))  # Mean of (18+65)/2 and sd chosen
ages <- pmax(pmin(ages, 100), 18)  # Ensure ages are within 18 to 65
V(g)$age <- ages

# Assign binary female distribution 70/30 female/male
females <- sample(c(1, 0), num_nodes, replace = TRUE, prob = c(0.7, 0.3))
V(g)$female <- females

# Set average and sd degree
avg_degree <- 6
sd_degree <- 4

# Choose level of homophily
homophily = 0.9

# Compute number of edges needed
num_edges <- round((avg_degree * num_nodes) / 2)

# Create an empty edge list=
edge_list <- vector("list", num_edges)

# Generate edges with trait homophily based on gender and age similarity
set.seed(123)  # Set seed for reproducibility
for (i in 1:num_edges) {
  # Choose first node randomly
  node1 <- sample(1:num_nodes, 1)
  
  # Determine potential nodes with same gender (higher probability)
  same_gender_nodes <- which(V(g)$female == V(g)$female[node1])
  
  # Within same gender nodes, further narrow down to same age (even higher probability)
  same_age_nodes <- same_gender_nodes[V(g)$age[same_gender_nodes] == V(g)$age[node1]]
  
  # Define probabilities for connecting based on the homophily criteria
  if (length(same_age_nodes) > 1) {
    # Higher probability for nodes with the same gender and age
    node2 <- sample(same_age_nodes[same_age_nodes != node1], homophily)
  } else {
    # Otherwise, choose from nodes with the same gender
    node2 <- sample(same_gender_nodes[same_gender_nodes != node1], homophily)
  }
  
  # Add edge to the edge list
  edge_list[[i]] <- c(node1, node2)
}

# Add edges to the graph
g <- add_edges(g, unlist(edge_list))

# Find the clusters (connected components)
components <- components(g)

# Keep nodes that are part of any connected component with size > 1
connected_nodes <- which(components$csize[components$membership] > 1)

# Extract the subgraph with only connected nodes
connected_subgraph <- induced_subgraph(g, connected_nodes)
g = induced_subgraph(g, connected_nodes)

# Summary of the graph
mean(V(g)$age)
mean(V(g)$female)
mean(degree(g))

# --- SELECT SEEDS ---------

# non-representative seeds

# Step 1: Calculate node degrees and find extremes
node_degrees <- degree(g)
high_degree_nodes <- which(node_degrees > quantile(node_degrees, 0.95))  # Top 5% degree nodes
low_degree_nodes <- which(node_degrees < quantile(node_degrees, 0.05))   # Bottom 5% degree nodes

# Step 2: Find nodes with extreme ages
node_ages <- V(g)$age
extreme_age_nodes <- which(node_ages > quantile(node_ages, 0.95) | node_ages < quantile(node_ages, 0.05))  # Top and bottom 5% age nodes

# Step 3: Select nodes with a skewed gender distribution (over-representing one gender)
gender_distribution <- table(V(g)$female)
if (gender_distribution[1] > gender_distribution[2]) {
  skewed_gender_nodes <- which(V(g)$female == 0)  # If females are overrepresented, pick males
} else {
  skewed_gender_nodes <- which(V(g)$female == 1)  # If males are overrepresented, pick females
}

# Step 4: Combine all non-representative nodes found by the criteria
non_representative_candidates <- unique(c(high_degree_nodes, low_degree_nodes, extreme_age_nodes, skewed_gender_nodes))

# Step 5: Randomly select 100 nodes from the non-representative candidates
set.seed(123)  # Set seed for reproducibility
if (length(non_representative_candidates) >= 100) {
  non_representative_sample <- sample(non_representative_candidates, 100)
} else {
  non_representative_sample <- non_representative_candidates
  cat("Less than 100 non-representative nodes found; selected all", length(non_representative_sample), "nodes.\n")
}

# mostly young men 
young_men = V(g)[V(g)$age < 22 & V(g)$female == 0]
mostly_young_men = union(young_men, sample(V(g), 20))

# mostly old women
older_women = V(g)[V(g)$age > 63 & V(g)$female == 1]
mostly_older_women = union(older_women, sample(V(g), 20))

# --- RDS ALGORITHM --------
# Randomly choose referral from neighbors

# Step 1: Set Parameters
set.seed(15)  # Set random seed for reproducibility
num_seeds <- 6  # Number of initial seeds
recruits_per_person <- 2  # Number of recruits per person
num_waves <- 8  # Number of waves

# Step 2: Select Initial Seeds
# seeds <- sample(V(g), num_seeds)
# seeds = sample(V(g)[non_representative_sample], num_seeds)
# seeds = sample(V(g)[older_women], num_seeds)
# seeds = sample(V(g)[mostly_older_women], num_seeds)
# seeds = sample(V(g)[young_men], num_seeds)
seeds = sample(V(g)[mostly_young_men], num_seeds)

rds_participants <- vector("list", num_waves + 1)  # To store participants in each wave
rds_participants[[1]] <- seeds  # Initial seeds are the first wave

# Step 3: Perform rds Recruitment
for (wave in 1:num_waves) {
  current_wave_participants <- rds_participants[[wave]]
  next_wave_participants <- c()  # Initialize next wave participants
  
  for (participant in current_wave_participants) {
    # Find neighbors of the current participant
    neighbors <- neighbors(g, participant)
    
    # Exclude already sampled participants to prevent duplicate recruitment
    eligible_neighbors <- setdiff(neighbors, unlist(rds_participants))
    
    # Recruit up to 3 new participants from eligible neighbors
    if (length(eligible_neighbors) > 0) {
      recruits <- sample(eligible_neighbors, min(recruits_per_person, length(eligible_neighbors)))
      next_wave_participants <- c(next_wave_participants, recruits)
    }
  }
  
  # Store the participants for the next wave
  rds_participants[[wave + 1]] <- next_wave_participants
  
  # Stop if no more recruits are possible
  if (length(next_wave_participants) == 0) {
    break
  }
}

# --- CREATE RDS RESULTS ---------

# create empty vectors to store the values
n_wave = c() # wave number
n_participants = c() # number of participants in this wave
avg_age = c() # average age of participants in this wave
prop_female = c() # proportion female in this wave
c_participants = c() # cumulative number of participants through this wave
c_avg_age = c() # cumulative average age of participants through this wave
c_prop_female = c() # cumulative proportion female through this wave


# Initialize cumulative lists to keep track of cumulative participants
cumulative_participants <- c()

# Output details about each wave and compute cumulative statistics
for (wave in 1:length(rds_participants)) {
  
  # wave
  n_wave[[wave]] = wave-1 # assign
  
  # n_participants
  participants <- rds_participants[[wave]]
  n_participants[[wave]] = length(participants) # assign
  cat("Wave", wave - 1, "Participants:", length(participants), "\n")
  
  # average age of participants in this wave
  avg_age[[wave]] = mean(V(g)$age[participants])
  
  # proportion female in this wave
  gender_counts <- table(V(g)$female[participants])
  prop_female[[wave]] <- prop.table(gender_counts)[2]
  
  # Combine participants from all waves up to the current wave
  cumulative_participants <- c(cumulative_participants, rds_participants[[wave]])
  cumulative_participants <- unique(cumulative_participants)  # Remove duplicates
  c_participants[[wave]] = length(cumulative_participants) # assign
  
  cat("Cumulative Statistics up to Wave", wave - 1, "\n")
  cat("  Number of Participants:", length(cumulative_participants), "\n")
  
  if (length(cumulative_participants) > 0) {
    # Calculate cumulative mean age for the participants up to the current wave
    cumulative_mean_age <- mean(V(g)$age[cumulative_participants])
    c_avg_age[[wave]] = cumulative_mean_age
    
    # Calculate cumulative gender proportion for the participants up to the current wave
    cumulative_gender_counts <- table(V(g)$female[cumulative_participants])
    cumulative_gender_proportions <- prop.table(cumulative_gender_counts)
    c_prop_female[[wave]] = cumulative_gender_proportions[2]
    
    # Output cumulative statistics
    cat("  Cumulative Average Age:", round(cumulative_mean_age, 2), "\n")
    cat("  Cumulative Gender Proportion (Female/Male):\n")
    print(cumulative_gender_proportions)
  } else {
    cat("  No participants up to this wave.\n")
  }
  
  cat("\n")
}

rds_results_table = data.frame(Wave = unlist(n_wave),
                                Participants = unlist(n_participants),
                                Mean_age = unlist(avg_age), 
                                Female = unlist(prop_female),
                                Participants_cumulative = unlist(c_participants),
                                Mean_age_cumulative = unlist(c_avg_age),
                                Female_cumulative = unlist(c_prop_female))

# # --- CREATE RDS RESULTS WITH PROBABILITY SAMPLE STATS ---------
# 
# # Create empty vectors to store values for RDS and PS stats
# PS_mean_age = c() # mean age for probability sample
# PS_female = c() # proportion female for probability sample
# PS_mean_age_cumulative = c() # cumulative mean age for probability sample
# PS_female_cumulative = c() # cumulative proportion female for probability sample
# 
# # Initialize cumulative lists to keep track of cumulative participants
# cumulative_participants <- c()
# ps_cumulative_participants <- c()
# 
# # Output details about each wave and compute cumulative statistics, including PS
# for (wave in 1:length(rds_participants)) {
#   
#   # Existing code for RDS statistics
#   n_wave[[wave]] = wave-1
#   participants <- rds_participants[[wave]]
#   n_participants[[wave]] = length(participants)
#   
#   # Calculate average age and proportion female for the current wave
#   avg_age[[wave]] = mean(V(g)$age[participants])
#   gender_counts <- table(V(g)$female[participants])
#   prop_female[[wave]] <- prop.table(gender_counts)[2]
#   
#   # --- Fix Participants_cumulative calculation ---
#   # For wave 0, Participants_cumulative is the number of participants in wave 0
#   if (wave == 1) {
#     c_participants[[wave]] <- n_participants[[wave]]
#   } else {
#     # For subsequent waves, sum the participants from all previous waves including current wave
#     c_participants[[wave]] <- c_participants[[wave - 1]] + n_participants[[wave]]
#   }
#   
#   # Cumulative stats for RDS
#   cumulative_participants <- c(cumulative_participants, participants)
#   cumulative_participants <- unique(cumulative_participants)
#   if (length(cumulative_participants) > 0) {
#     c_avg_age[[wave]] = mean(V(g)$age[cumulative_participants])
#     cumulative_gender_counts <- table(V(g)$female[cumulative_participants])
#     c_prop_female[[wave]] = prop.table(cumulative_gender_counts)[2]
#   }
#   
#   # --- Add Probability Sample (PS) calculations ---
#   if (n_participants[[wave]] > 0) {
#     ps_sample <- sample(V(g), size = n_participants[[wave]], replace = FALSE)
#     
#     # PS mean age and proportion female for this wave
#     PS_mean_age[[wave]] <- mean(V(g)$age[ps_sample])
#     ps_gender_counts <- table(V(g)$female[ps_sample])
#     PS_female[[wave]] <- prop.table(ps_gender_counts)[2]
#     
#     # Cumulative PS stats
#     ps_cumulative_participants <- c(ps_cumulative_participants, ps_sample)
#     ps_cumulative_participants <- unique(ps_cumulative_participants)
#     if (length(ps_cumulative_participants) > 0) {
#       PS_mean_age_cumulative[[wave]] <- mean(V(g)$age[ps_cumulative_participants])
#       ps_cumulative_gender_counts <- table(V(g)$female[ps_cumulative_participants])
#       PS_female_cumulative[[wave]] <- prop.table(ps_cumulative_gender_counts)[2]
#     }
#   }
# }
# 
# # Add PS columns to rds_results_table
# rds_results_table = data.frame(Wave = unlist(n_wave),
#                                Participants = unlist(n_participants),
#                                Mean_age = unlist(avg_age), 
#                                Female = unlist(prop_female),
#                                Participants_cumulative = unlist(c_participants),
#                                Mean_age_cumulative = unlist(c_avg_age),
#                                Female_cumulative = unlist(c_prop_female),
#                                PS_mean_age = unlist(PS_mean_age),
#                                PS_female = unlist(PS_female),
#                                PS_mean_age_cumulative = unlist(PS_mean_age_cumulative),
#                                PS_female_cumulative = unlist(PS_female_cumulative))

# # --- CREATE RDS AND PROBABILITY SAMPLE RESULTS WITH STRATIFIED SUBSAMPLE ---------
# 
# # Create empty vectors to store values for Probability Sample with Selection (PSS) stats
# PSS_mean_age = c()  # mean age for PSS
# PSS_female = c()  # proportion female for PSS
# PSS_mean_age_cumulative = c()  # cumulative mean age for PSS
# PSS_female_cumulative = c()  # cumulative proportion female for PSS
# 
# # Initialize cumulative lists to keep track of cumulative PSS participants
# pss_cumulative_participants <- c()
# 
# # Output details about each wave and compute cumulative statistics, including PSS
# for (wave in 1:length(rds_participants)) {
#   
#   # Existing code for RDS statistics
#   n_wave[[wave]] = wave - 1
#   participants <- rds_participants[[wave]]
#   n_participants[[wave]] = length(participants)
#   
#   # --- Fix Participants_cumulative calculation ---
#   if (wave == 1) {
#     c_participants[[wave]] <- n_participants[[wave]]
#   } else {
#     c_participants[[wave]] <- c_participants[[wave - 1]] + n_participants[[wave]]
#   }
#   
#   # Cumulative stats for RDS
#   cumulative_participants <- c(cumulative_participants, participants)
#   cumulative_participants <- unique(cumulative_participants)
#   if (length(cumulative_participants) > 0) {
#     c_avg_age[[wave]] = mean(V(g)$age[cumulative_participants])
#     cumulative_gender_counts <- table(V(g)$female[cumulative_participants])
#     c_prop_female[[wave]] = prop.table(cumulative_gender_counts)[2]
#   }
#   
#   # --- Add Probability Sample (PS) calculations ---
#   if (n_participants[[wave]] > 0) {
#     ps_sample <- sample(V(g), size = n_participants[[wave]], replace = FALSE)
#     
#     # PS mean age and proportion female for this wave
#     PS_mean_age[[wave]] <- mean(V(g)$age[ps_sample])
#     ps_gender_counts <- table(V(g)$female[ps_sample])
#     PS_female[[wave]] <- prop.table(ps_gender_counts)[2]
#     
#     # Cumulative PS stats
#     ps_cumulative_participants <- c(ps_cumulative_participants, ps_sample)
#     ps_cumulative_participants <- unique(ps_cumulative_participants)
#     if (length(ps_cumulative_participants) > 0) {
#       PS_mean_age_cumulative[[wave]] <- mean(V(g)$age[ps_cumulative_participants])
#       ps_cumulative_gender_counts <- table(V(g)$female[ps_cumulative_participants])
#       PS_female_cumulative[[wave]] <- prop.table(ps_cumulative_gender_counts)[2]
#     }
#   }
#   
#   # --- Add Probability Sample with Selection (PSS) calculations ---
#   if (length(ps_sample) > 0) {
#     # Define inclusion probabilities based on age
#     inclusion_probabilities <- ifelse(V(g)$age[ps_sample] < 50, 0.6, 0.9)
#     
#     # Perform stratified subsample based on the inclusion probabilities
#     pss_sample <- ps_sample[runif(length(ps_sample)) < inclusion_probabilities]
#     
#     if (length(pss_sample) > 0) {
#       # Calculate PSS mean age and proportion female for this wave
#       PSS_mean_age[[wave]] <- mean(V(g)$age[pss_sample])
#       pss_gender_counts <- table(V(g)$female[pss_sample])
#       PSS_female[[wave]] <- prop.table(pss_gender_counts)[2]
#       
#       # Cumulative PSS stats
#       pss_cumulative_participants <- c(pss_cumulative_participants, pss_sample)
#       pss_cumulative_participants <- unique(pss_cumulative_participants)
#       if (length(pss_cumulative_participants) > 0) {
#         PSS_mean_age_cumulative[[wave]] <- mean(V(g)$age[pss_cumulative_participants])
#         pss_cumulative_gender_counts <- table(V(g)$female[pss_cumulative_participants])
#         PSS_female_cumulative[[wave]] <- prop.table(pss_cumulative_gender_counts)[2]
#       }
#     }
#   }
# }
# 
# # Add PSS columns to rds_results_table
# rds_results_table <- data.frame(Wave = unlist(n_wave),
#                                 Participants = unlist(n_participants),
#                                 Mean_age = unlist(avg_age), 
#                                 Female = unlist(prop_female),
#                                 Participants_cumulative = unlist(c_participants),
#                                 Mean_age_cumulative = unlist(c_avg_age),
#                                 Female_cumulative = unlist(c_prop_female),
#                                 PS_mean_age = unlist(PS_mean_age),
#                                 PS_female = unlist(PS_female),
#                                 PS_mean_age_cumulative = unlist(PS_mean_age_cumulative),
#                                 PS_female_cumulative = unlist(PS_female_cumulative),
#                                 PSS_mean_age = unlist(PSS_mean_age),
#                                 PSS_female = unlist(PSS_female),
#                                 PSS_mean_age_cumulative = unlist(PSS_mean_age_cumulative),
#                                 PSS_female_cumulative = unlist(PSS_female_cumulative))

# set NA to 0
rds_results_table[is.na(rds_results_table)] = 0

# --- PROBABILITY SAMPLING --------------

# randomly sample from g based on the # of RDS participants in each wave
PS_sample = c()
for(wave in rds_results_table$Wave){
  PS_sample[[wave+1]] = sample(x = V(g), 
                                 size = rds_results_table$Participants[wave+1])
}

# Compute statistics for each sample
PS_mean_age = c()
for(i in 1:length(PS_sample)){
  PS_mean_age[[i]] =  mean(V(g)$age[unlist(PS_sample[i])])
}

PS_female = c()
for(i in 1:length(PS_sample)){
  PS_female[[i]] =  mean(V(g)$female[unlist(PS_sample[i])])
}

# --- PROBABILITY SAMPLING WITH SELECTION --------

# specify selection parameters
# Probabilities
young_prob <- 0.6
mid_age_prob <- 0.8
aged_prob <- 0.9

# Function to include nodes based on age probabilities
include_based_on_age <- function(nodes, g) {
  included_nodes <- c()
  print(nodes)
  for (node in nodes) {
    age <- V(g)$age[node]  # Get the age of the node
    
    if (age < 30) {
      prob <- young_prob
    } else if (age >= 30 && age < 60) {
      prob <- mid_age_prob
    } else {
      prob <- aged_prob
    }
    
    # Include the node with the specified probability
    if (runif(1) <= prob) {
      included_nodes <- c(included_nodes, node)
    }
  }
  
  return(included_nodes)
}

PSS_sample = c()
for(wave in unlist(PS_sample)){
  PSS_sample[[wave+1]] = include_based_on_age(wave, g)
}

for (wave in 1:length(rds_participants)) {
  rds_participants[[wave]] <- include_based_on_age(rds_participants[[wave]], g)
}


# --- COMBINE RESULTS ----

rds_results_table$PS_mean_age_cumulative = unlist(PS_mean_age)
rds_results_table$PS_female_cumulative = unlist(PS_female)

# --- PLOT RESULTS -------
rds_data = as.data.table(rds_results_table)

# Define the function
plot_wave_data <- function(dt, custom_title) {
  # Ensure dt is a data.table
  if (!is.data.table(dt)) {
    stop("Input must be a data.table.")
  }
  
  # Create the plot
  p <- ggplot(dt, aes(x = Wave)) +
    # Plot for Mean_age
    geom_point(aes(y = Mean_age, color = "Mean Age", size = Participants), alpha=0.6) +
    scale_size_continuous(range=c(min(dt$Participants)/min(dt$Participants), 
                                  max(dt$Participants)/min(dt$Participants))) +
    scale_y_continuous(name = "Mean Age (yr)", 
                       breaks = seq(0, 70, by = 10),
                       limits = c(0, 100),
                       sec.axis = sec_axis(~ ., name = "Percent Female")) +
    # Plot for Female
    geom_point(aes(y = Female * 100, color = "Female", size = Participants), alpha=0.6) +
    scale_color_manual(values = c("Mean Age" = "darkblue", "Female" = "tomato")) +
    labs(title = custom_title,
         x = "Wave") +
    scale_x_continuous(breaks = seq(0, num_waves, by = 1), limits = c(0, num_waves)) +
    theme_minimal() +
    theme(
      axis.title.y.right = element_text(color = "tomato"),
      axis.text.y.right = element_text(color = "tomato"),
      axis.title.y.left = element_text(color = "darkblue"),
      axis.text.y.left = element_text(color = "darkblue"),
      legend.position = "none"  # Remove all legends
    ) +
    # Add horizontal lines
    geom_hline(yintercept = 41.5, linetype = "dashed", color = "darkblue") +
    geom_hline(yintercept = 70, linetype = "dashed", color = "tomato") +  # Female * 100
    # Add labels for the horizontal lines
    annotate("text", x = 3.32, y = 44, label = "Population Average", hjust = 1, color = "darkblue") +
    annotate("text", x = 3.65, y = 73, label = "Population Proportion", hjust = 1, color = "tomato") +
    annotate("text", x = 1.8, y = 18, label = "Seed n=6", hjust = 1, color = "black")
  
  # Print the plot
  print(p)
}

# Define the function to plot cumulative waves
plot_cumulative_data <- function(dt, custom_title) {
  # Ensure dt is a data.table
  if (!is.data.table(dt)) {
    stop("Input must be a data.table.")
  }
  
  # Create the plot
  p <- ggplot(dt, aes(x = Wave)) +
    # Add a shaded background area for Burn In and Final Sample
    geom_rect(aes(xmin = 0, xmax = 2.5, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.3) +
    # geom_rect(aes(xmin = 7.5, xmax = 8.5, ymin = -Inf, ymax = Inf), fill = "lightgrey", alpha = 0.3) +
    
    # RDS Points
    # Add small inner points for Mean_age_cumulative
    geom_point(aes(y = Mean_age_cumulative), color = "darkblue", 
               size = 5, shape = 16, alpha = 0.6) +
    geom_line(aes(y = Mean_age_cumulative), color = "darkblue", 
              linewidth = 1, alpha = 0.6) +
  
    # # Add small inner points for Female_cumulative
    # geom_point(aes(y = Female_cumulative * 100), color = "tomato", 
    #            size = 3, shape = 16, alpha = 0.8) +
    
    # Probability Sampling Points
    # Add small inner points for Mean_age_cumulative
    geom_point(aes(y = PS_mean_age_cumulative), color = "tomato",
               size = 3, shape = 4, stroke = 3, alpha = 0.6) +
    geom_line(aes(y = PS_mean_age_cumulative), color = "tomato", 
              linewidth = 1, alpha = 0.6) +
    
    # # Add small inner points for Female_cumulative
    # geom_point(aes(y = PS_female_cumulative * 100), color = "tomato",
    #            size = 3, shape = 15, alpha = 0.8) +
    
    # # Probability Sampling with Selection Points
    # # Add small inner points for Mean_age_cumulative
    # geom_point(aes(y = PSS_mean_age_cumulative), color = "darkblue",
    #            size = 3, shape = 16, alpha = 0.8) +
    # # Add small inner points for Female_cumulative
    # geom_point(aes(y = PSS_female_cumulative * 100), color = "tomato",
    #            size = 3, shape = 16, alpha = 0.8) +

    
    # # Configure the y-axis for Mean Age and Percent Female
    # scale_y_continuous(name = "Mean Age (yr)", 
    #                    breaks = seq(0, 100, by = 10),
    #                    limits = c(0, 100),
    #                    sec.axis = sec_axis(~ ., 
    #                                        name = "Percent Female",
    #                                        breaks = seq(0, 100, by = 10))) + # Breaks every 10% for Percent Female
    
    # Configure the y-axis for Mean Age and Percent Female
    scale_y_continuous(name = "Mean Age (yr)", 
                       breaks = seq(0, 100, by = 10),
                       limits = c(0, 70)) + 
    
    # Configure colors
    scale_color_manual(values = c("Mean Age" = "darkblue", "Female" = "tomato")) +
    
    # Add labels and titles
    labs(title = custom_title, x = "Wave") +
    
    # Configure the x-axis
    scale_x_continuous(breaks = seq(0, num_waves, by = 1), limits = c(0, num_waves+0.5)) +
    # Theme settings, increasing font size for axis labels and ticks
    theme_minimal() +
    theme(
      axis.title.y.right = element_text(color = "tomato", size = 28),   # Increase font size for right y-axis label
      axis.text.y.right = element_text(color = "tomato", size = 28),    # Increase font size for right y-axis ticks
      axis.title.y.left = element_text(color = "darkblue", size = 28),  # Increase font size for left y-axis label
      axis.text.y.left = element_text(color = "darkblue", size = 28),   # Increase font size for left y-axis ticks
      axis.title.x = element_text(size = 28),  # Increase font size for x-axis label
      axis.text.x = element_text(size = 28),   # Increase font size for x-axis ticks
      legend.position = "none"  # Remove all legends
    ) +
    
    # Add points and labels for Legend
    geom_point(aes(x = 4, y = 70), color = "tomato", 
               size = 6, shape = 4, stroke = 3, alpha = 0.6) +
    geom_point(aes(x = 4, y = 62), color = "darkblue", shape = 16, 
               size = 10, alpha = 0.6) +
    # PS
    annotate("text", x = 7.9, y = 70, label = "Probability Sample", hjust = 1, color = "black", size = 10) +
    # RDS
    annotate("text", x = 5.3, y = 62, label = "RDS", hjust = 1, color = "black", size = 10) +
    

    
    # Add horizontal lines
    geom_hline(yintercept = 41.5, linetype = "dashed", color = "darkblue") +
    # geom_hline(yintercept = 70, linetype = "dashed", color = "tomato") +  # Female * 100
    # Add labels for the horizontal lines
    annotate("text", x = 8, y = 35, label = "Pop Mean (41)", hjust = 1, color = "darkblue", size = 10) +
    # annotate("text", x = 3.65, y = 73, label = "Population Proportion", hjust = 1, color = "tomato") +
    # Add labels for the shaded areas and sample sizes
    annotate("text", x = 1.25, y = 65, label = "Burn In: \n3 Waves", 
             hjust = 0.5, color = "black", fontface = "bold", size = 10) +
    annotate("text", x = 1.4, y = 15, label = paste0("Seed\nn=", as.character(min(dt$Participants_cumulative))),
             hjust = 1, color = "black", fontface = "bold", size = 10) + 
    annotate("text", x = 5, y = 15, label = paste0("Wave 5\nn=", as.character(dt$Participants_cumulative[5])),
             hjust = 1, color = "black", fontface = "bold", size = 10) + 
    annotate("text", x = 8.5, y = 15, label = paste0("Final \nn=", as.character(dt$Participants_cumulative[9])),
             hjust = 1, color = "black", fontface = "bold", size = 10)
    # annotate("text", x = 8.4, y = 18, label = paste0("n=", as.character(max(dt$Participants_cumulative))),
    #          hjust = 1, color = "black", fontface = "bold")
  
  # Print the plot
  print(p)
}

# Plot the data
plot_cumulative_data(rds_data[burn_in:nrow(rds_data),], 
                     "")

# plot_wave_data(rds_data[burn_in:nrow(rds_data),],
#                "Average Age and Proportion Female in Each Wave After ")










