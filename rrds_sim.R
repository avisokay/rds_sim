library(igraph)
library(ggplot2)
library(data.table)

# --- GENERATE NETWORK -----------

# Create a graph with 10,000 nodes
num_nodes <- 10000
g <- make_empty_graph(n = num_nodes, directed = FALSE)

# Assign age distribution to nodes with a normal distribution between 18 and 65
set.seed(123)  # Set seed for reproducibility
ages <- round(rnorm(num_nodes, mean = 41.5, sd = 10))  # Mean of (18+65)/2 and sd chosen
ages <- pmax(pmin(ages, 65), 18)  # Ensure ages are within 18 to 65
V(g)$age <- ages

# Assign binary female distribution 70/30 female/male
females <- sample(c(1, 0), num_nodes, replace = TRUE, prob = c(0.7, 0.3))
V(g)$female <- females

# Set average and sd degree
avg_degree <- 2
sd_degree <- 2

# Choose level of homophily
homophily = 0.9

# Compute number of edges needed
num_edges <- round((avg_degree * num_nodes) / 2)

# Create an empty edge list
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

# --- RRDS ALGORITHM --------
# Randomly choose referral from neighbors

# Step 1: Set Parameters
set.seed(12)  # Set random seed for reproducibility
num_seeds <- 10  # Number of initial seeds
recruits_per_person <- 3  # Number of recruits per person
num_waves <- 12  # Number of waves

# Step 2: Select Initial Seeds
# seeds <- sample(V(g), num_seeds)
# seeds = V(g)[non_representative_sample]
# seeds = V(g)[older_women]
# seeds = V(g)[mostly_older_women]
# seeds = V(g)[young_men]
seeds = V(g)[mostly_young_men]

rrds_participants <- vector("list", num_waves + 1)  # To store participants in each wave
rrds_participants[[1]] <- seeds  # Initial seeds are the first wave

# Step 3: Perform rrds Recruitment
for (wave in 1:num_waves) {
  current_wave_participants <- rrds_participants[[wave]]
  next_wave_participants <- c()  # Initialize next wave participants

  for (participant in current_wave_participants) {
    # Find neighbors of the current participant
    neighbors <- neighbors(g, participant)

    # Exclude already sampled participants to prevent duplicate recruitment
    eligible_neighbors <- setdiff(neighbors, unlist(rrds_participants))

    # Recruit up to 3 new participants from eligible neighbors
    if (length(eligible_neighbors) > 0) {
      recruits <- sample(eligible_neighbors, min(recruits_per_person, length(eligible_neighbors)))
      next_wave_participants <- c(next_wave_participants, recruits)
    }
  }

  # Store the participants for the next wave
  rrds_participants[[wave + 1]] <- next_wave_participants

  # Stop if no more recruits are possible
  if (length(next_wave_participants) == 0) {
    break
  }
}

# --- CREATE RRDS RESULTS ---------

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
for (wave in 1:length(rrds_participants)) {

  # wave
  n_wave[[wave]] = wave-1 # assign

  # n_participants
  participants <- rrds_participants[[wave]]
  n_participants[[wave]] = length(participants) # assign
  cat("Wave", wave - 1, "Participants:", length(participants), "\n")

  # average age of participants in this wave
  avg_age[[wave]] = mean(V(g)$age[participants])

  # proportion female in this wave
  gender_counts <- table(V(g)$female[participants])
  prop_female[[wave]] <- prop.table(gender_counts)[2]

  # Combine participants from all waves up to the current wave
  cumulative_participants <- c(cumulative_participants, rrds_participants[[wave]])
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

rrds_results_table = data.frame(Wave = unlist(n_wave),
                               Participants = unlist(n_participants),
                               Mean_age = unlist(avg_age),
                               Female = unlist(prop_female),
                               Participants_cumulative = unlist(c_participants),
                               Mean_age_cumulative = unlist(c_avg_age),
                               Female_cumulative = unlist(c_prop_female))

# --- RDS ALGORITHIM -------
# refer your most similar neighbors

rds_participants <- vector("list", num_waves + 1)  # To store participants in each wave
rds_participants[[1]] <- seeds  # Initial seeds are the first wave

# Step 3: Perform RDS Recruitment
for (wave in 1:num_waves) {
  current_wave_participants <- rds_participants[[wave]]
  next_wave_participants <- c()  # Initialize next wave participants

  for (participant in current_wave_participants) {
    # Find neighbors of the current participant
    neighbors <- neighbors(g, participant)

    # Exclude already sampled participants to prevent duplicate recruitment
    eligible_neighbors <- setdiff(neighbors, unlist(rds_participants))

    # Step 4: Sort Eligible Neighbors by Similarity (same gender first, then closest age)
    if (length(eligible_neighbors) > 0) {
      # Convert eligible_neighbors to numeric indices
      eligible_neighbor_indices <- as.numeric(eligible_neighbors)

      # Extract participant gender and age
      participant_gender <- V(g)$female[participant]
      participant_age <- V(g)$age[participant]

      # Sort eligible neighbors: same gender first, then by closest age
      sorted_neighbors <- eligible_neighbor_indices[order(
        V(g)$female[eligible_neighbor_indices] != participant_gender,  # Sort by same gender first
        abs(V(g)$age[eligible_neighbor_indices] - participant_age)  # Then by closest age
      )]

      # Recruit up to specified number of participants from sorted eligible neighbors
      recruits <- head(sorted_neighbors, min(recruits_per_person, length(sorted_neighbors)))
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

# --- CREATE RDS RESULTS -----

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

# --- PLOT RESULTS -------
source("plot_rds.R")

rrds_data <- as.data.table(rrds_results_table)
rds_data <- as.data.table(rds_results_table)

burn_in <- 0
seed_n <- length(seeds)

# Calculate global maximum participants across both datasets for consistent scaling
global_max <- calc_global_max(rds_data, rrds_data)

# Create comparison plots with consistent scaling
sample_plot <- plot_wave_comparison(
  rds_data[burn_in:nrow(rds_data), ],
  rrds_data[burn_in:nrow(rrds_data), ],
  custom_title = "Wave-by-wave Statistics",
  global_max_participants = global_max,
  seed_n = seed_n
)

cumulative_plot <- plot_cumulative_comparison(
  rds_data[burn_in:nrow(rds_data), ],
  rrds_data[burn_in:nrow(rrds_data), ],
  custom_title = "Cumulative Statistics",
  global_max_participants = global_max,
  seed_n = seed_n
)

# Save plots as high-resolution PDFs
save_plot(sample_plot, "plots/sample.pdf")
save_plot(cumulative_plot, "plots/cumulative.pdf")

# Create the combined 2x2 plot
combined_plot <- plot_combined_comparison(
  rds_data[burn_in:nrow(rds_data), ],
  rrds_data[burn_in:nrow(rrds_data), ],
  paste0("RDS vs RRDS Comparison: Wave and Cumulative Analysis\nBurn-in waves: ", burn_in),
  global_max_participants = global_max
)

# Save the combined plot
save_plot(combined_plot, "plots/combined_comparison.pdf", width = 12, height = 10)

# --- CLEAN VISUALIZATIONS ---

# Simple convergence plots
age_convergence <- plot_convergence_age(rds_data, rrds_data)
save_plot(age_convergence, "plots/convergence_age.pdf", width = 8, height = 5)

female_convergence <- plot_convergence_female(rds_data, rrds_data)
save_plot(female_convergence, "plots/convergence_female.pdf", width = 8, height = 5)

sample_size_plot <- plot_sample_size(rds_data, rrds_data)
save_plot(sample_size_plot, "plots/sample_size.pdf", width = 8, height = 5)

# Two-panel figure (age + sample size)
two_panel <- plot_two_panel(rds_data, rrds_data)
save_plot(two_panel, "plots/two_panel.pdf", width = 10, height = 5)

# Three-panel figure (age + female + sample size)
three_panel <- plot_three_panel(rds_data, rrds_data)
save_plot(three_panel, "plots/three_panel.pdf", width = 12, height = 4)
