# rrds_sim_robustness.R
# Robustness simulation addressing reviewer comments:
#
# Reviewer 1: Test robustness to non-response, biased contact listing, and
#             partial enumeration of contact lists.
#
# Reviewer 2: (a) Separate the two distinct uses of "homophily":
#               - alpha_net: NETWORK STRUCTURAL HOMOPHILY — the tendency of the
#                 underlying social network to connect similar individuals. This is
#                 a property of the network itself that RRDS cannot address.
#               - alpha_rec: PREFERENTIAL RECRUITMENT — the tendency of RDS
#                 participants to actively recruit contacts similar to themselves
#                 from among their known contacts. RRDS addresses this by requiring
#                 random selection from the contact list.
#             (b) Use a realistic mean degree (10 vs. 2 in original). With mean
#                 degree 2 and max 3 recruits, RDS and RRDS are nearly equivalent
#                 because participants have almost no choice in who to recruit.
#                 Higher degree creates meaningful differentiation.
#             (c) Report VH (Volz-Heckathorn) degree-weighted estimator, which
#                 corrects for degree bias so estimates approach true population
#                 values regardless of whether degree varies across groups.
#
# Reviewer 3: With higher mean degree and alpha_rec = 0.9, RDS preferential
#             recruitment now produces clearly biased estimates as expected by
#             theory. In the original low-degree simulation, the near-exhaustive
#             recruitment of all eligible neighbors left no room for selective
#             bias to accumulate.

library(igraph)
library(ggplot2)
library(data.table)
library(patchwork)
library(scales)

# ============================================================
# 1. PARAMETERS
# ============================================================

# Population
NUM_NODES       <- 10000
POP_MEAN_AGE    <- 41.5
POP_SD_AGE      <- 10
POP_PROP_FEMALE <- 0.70

# Network: increased from mean degree 2 to 10 (addressing Reviewer 2).
# Social networks of general populations typically have mean degree of 10-30.
# Mean degree 2 caused near-exhaustive recruitment in the original simulation,
# which artifactually eliminated differences between RDS and RRDS.
AVG_DEGREE      <- 10

# Two distinct homophily parameters (Reviewer 2 & 4):
#
# ALPHA_NET: Network structural homophily (a property of the network).
# Probability that an edge in the network connects two same-gender individuals
# rather than random individuals. RRDS does not address this — the contact
# list of any participant is already shaped by who they know in the network.
#
# ALPHA_REC: Preferential recruitment (a property of the recruitment behavior).
# In traditional RDS, probability that a participant recruits a same-gender
# (or similar-age) contact rather than selecting randomly from their contacts.
# RRDS is designed to eliminate this by requiring random contact list selection.
ALPHA_NET       <- 0.40
ALPHA_REC       <- 0.90

# Sampling
RECRUITS_PER_PERSON <- 3
NUM_WAVES           <- 12
N_TARGET            <- 3000  # stop recruitment here — simulates a real study sample

# Monte Carlo — set RUN_MC <- TRUE for final publication run (slow)
RUN_MC <- FALSE
N_SIMS <- 30

# ============================================================
# 2. NETWORK GENERATION
# ============================================================

build_network <- function(num_nodes, avg_degree, alpha_net,
                          pop_mean_age, pop_sd_age, pop_prop_female,
                          seed = 123) {
  set.seed(seed)

  ages    <- round(rnorm(num_nodes, mean = pop_mean_age, sd = pop_sd_age))
  ages    <- pmax(pmin(ages, 65), 18)
  females <- sample(c(1L, 0L), num_nodes, replace = TRUE,
                    prob = c(pop_prop_female, 1 - pop_prop_female))

  num_edges <- round((avg_degree * num_nodes) / 2)
  edge_v    <- integer(num_edges * 2)

  for (i in seq_len(num_edges)) {
    node1 <- sample.int(num_nodes, 1)

    if (runif(1) < alpha_net) {
      # Structural homophily: connect to a same-gender node, weighted by age proximity
      same_g <- which(females == females[node1])
      same_g <- same_g[same_g != node1]

      if (length(same_g) == 0) {
        node2 <- sample(setdiff(seq_len(num_nodes), node1), 1)
      } else {
        age_diffs <- abs(ages[same_g] - ages[node1])
        wts       <- exp(-age_diffs / 10)
        node2     <- same_g[sample.int(length(same_g), 1, prob = wts)]
      }
    } else {
      # Random connection (cross-group or different age)
      node2 <- sample(setdiff(seq_len(num_nodes), node1), 1)
    }

    edge_v[2 * i - 1] <- node1
    edge_v[2 * i]     <- node2
  }

  g <- make_empty_graph(n = num_nodes, directed = FALSE)
  V(g)$age    <- ages
  V(g)$female <- females
  g <- add_edges(g, edge_v)
  g <- simplify(g)  # remove multi-edges and self-loops

  comps      <- components(g)
  keep_nodes <- which(comps$csize[comps$membership] > 1)
  g          <- induced_subgraph(g, keep_nodes)

  return(g)
}

cat("Building network...\n")
g <- build_network(NUM_NODES, AVG_DEGREE, ALPHA_NET,
                   POP_MEAN_AGE, POP_SD_AGE, POP_PROP_FEMALE)

cat("Network statistics:\n")
cat("  Nodes:", vcount(g), "\n")
cat("  Mean degree:", round(mean(degree(g)), 2), "\n")
cat("  Mean age:", round(mean(V(g)$age), 2), "\n")
cat("  Prop female:", round(mean(V(g)$female), 3), "\n\n")

# True population values in the retained connected subgraph
TRUE_MEAN_AGE    <- mean(V(g)$age)
TRUE_PROP_FEMALE <- mean(V(g)$female)

# ============================================================
# 3. SEED SELECTION (biased: 10 young men)
# ============================================================
# Previous code used union(all_young_men, 20_random) which produced ~270 seeds,
# causing the network to saturate by wave 1. Now exactly 10 biased seeds.

young_men_all <- as.integer(V(g)[V(g)$age < 22 & V(g)$female == 0])
set.seed(42)
seeds     <- sample(young_men_all, min(10L, length(young_men_all)))
NUM_SEEDS <- length(seeds)

cat("Seeds (young men, n=10):", NUM_SEEDS, "\n")
cat("  Seed mean age:", round(mean(V(g)$age[seeds]), 2), "\n")
cat("  Seed prop female:", round(mean(V(g)$female[seeds]), 3), "\n\n")

# ============================================================
# 4. RRDS ALGORITHM
#
# Reviewer 1 robustness parameters:
#   nonresponse_rate — fraction of nominated contacts who decline to participate.
#   enumeration_rate — fraction of a participant's contacts they actually list
#                      (1.0 = exhaustive enumeration as intended; <1.0 = partial).
#   contact_bias     — when enumeration is partial, which contacts tend to be listed:
#                      "none"        = random subset
#                      "high_degree" = hubs disproportionately named (more visible)
#                      "same_group"  = same-gender contacts disproportionately named
# ============================================================

run_rrds <- function(g, seeds, num_waves, recruits_per_person,
                     nonresponse_rate = 0,
                     enumeration_rate = 1.0,
                     contact_bias     = "none",
                     n_target         = Inf,
                     seed_offset      = 0) {

  set.seed(100 + seed_offset)

  participants        <- vector("list", num_waves + 1)
  participants[[1]]   <- as.integer(seeds)

  for (wave in seq_len(num_waves)) {
    if (length(unique(unlist(participants[1:wave]))) >= n_target) break
    current   <- participants[[wave]]
    next_wave <- integer(0)

    for (p in current) {
      full_contacts <- as.integer(neighbors(g, p))
      if (length(full_contacts) == 0) next

      # --- Partial enumeration (Reviewer 1) ---
      if (enumeration_rate < 1.0 && length(full_contacts) > 1) {
        n_listed <- max(1L, round(length(full_contacts) * enumeration_rate))

        if (contact_bias == "high_degree") {
          degs  <- degree(g, full_contacts)
          probs <- degs / sum(degs)
          idx   <- sample.int(length(full_contacts),
                              min(n_listed, length(full_contacts)),
                              prob = probs)

        } else if (contact_bias == "same_group") {
          # Same-gender contacts are 80% of the listed contacts
          same_idx <- which(V(g)$female[full_contacts] == V(g)$female[p])
          diff_idx <- setdiff(seq_along(full_contacts), same_idx)
          n_same   <- min(round(n_listed * 0.80), length(same_idx))
          n_diff   <- min(n_listed - n_same, length(diff_idx))
          idx <- c(
            if (n_same > 0 && length(same_idx) > 0) sample(same_idx, n_same) else integer(0),
            if (n_diff > 0 && length(diff_idx) > 0) sample(diff_idx, n_diff) else integer(0)
          )

        } else {
          idx <- sample.int(length(full_contacts), min(n_listed, length(full_contacts)))
        }

        listed_contacts <- full_contacts[idx]
      } else {
        listed_contacts <- full_contacts
      }

      eligible <- setdiff(listed_contacts, unlist(participants))

      if (length(eligible) > 0) {
        n_recruit <- min(recruits_per_person, length(eligible))
        recruits  <- sample(eligible, n_recruit)  # RRDS: random selection

        # --- Non-response (Reviewer 1) ---
        if (nonresponse_rate > 0) {
          recruits <- recruits[runif(length(recruits)) > nonresponse_rate]
        }

        next_wave <- c(next_wave, recruits)
      }
    }

    participants[[wave + 1]] <- next_wave
    if (length(next_wave) == 0) break
  }

  return(participants)
}

# ============================================================
# 5. RDS ALGORITHM (PREFERENTIAL RECRUITMENT)
#
# alpha_rec controls how strongly participants prefer to recruit same-group
# contacts. This is "preferential recruitment" — distinct from network
# structural homophily (alpha_net). RRDS eliminates this by design.
# ============================================================

run_rds <- function(g, seeds, num_waves, recruits_per_person,
                    alpha_rec   = 0.9,
                    n_target    = Inf,
                    seed_offset = 0) {

  set.seed(200 + seed_offset)

  participants        <- vector("list", num_waves + 1)
  participants[[1]]   <- as.integer(seeds)

  for (wave in seq_len(num_waves)) {
    if (length(unique(unlist(participants[1:wave]))) >= n_target) break
    current   <- participants[[wave]]
    next_wave <- integer(0)

    for (p in current) {
      all_neighbors <- as.integer(neighbors(g, p))
      eligible      <- setdiff(all_neighbors, unlist(participants))

      if (length(eligible) > 0) {
        n_recruit <- min(recruits_per_person, length(eligible))
        recruits  <- integer(0)

        for (j in seq_len(n_recruit)) {
          remaining <- setdiff(eligible, recruits)
          if (length(remaining) == 0) break

          same_g <- remaining[V(g)$female[remaining] == V(g)$female[p]]

          if (runif(1) < alpha_rec && length(same_g) > 0) {
            # Preferential recruitment: same gender, weighted toward similar age
            age_diff <- abs(V(g)$age[same_g] - V(g)$age[p])
            wts      <- exp(-age_diff / 10)
            pick     <- same_g[sample.int(length(same_g), 1, prob = wts / sum(wts))]
          } else {
            pick <- sample(remaining, 1)
          }

          recruits <- c(recruits, pick)
        }

        next_wave <- c(next_wave, recruits)
      }
    }

    participants[[wave + 1]] <- next_wave
    if (length(next_wave) == 0) break
  }

  return(participants)
}

# ============================================================
# 6. RESULTS COMPUTATION
#
# Includes the Volz-Heckathorn (VH) degree-weighted estimator (Reviewer 2).
# The naive sample mean only equals the population mean when degree is equal
# across groups. The VH estimator uses inverse-degree weighting to correct
# for the fact that higher-degree nodes are more likely to be sampled in
# chain-referral designs:
#   VH_mean(x) = sum(x_i / d_i) / sum(1 / d_i)
# where d_i is the network degree of participant i.
# ============================================================

compute_results <- function(g, participants) {
  n_waves     <- length(participants)
  cumulative  <- integer(0)
  all_degrees <- degree(g)

  rows <- lapply(seq_len(n_waves), function(wave) {
    pts       <- as.integer(participants[[wave]])
    cumulative <<- unique(c(cumulative, pts))
    n_cum      <- length(cumulative)

    cum_age    <- mean(V(g)$age[cumulative])
    cum_female <- mean(V(g)$female[cumulative])

    # VH estimator: down-weight high-degree nodes
    degs_cum  <- all_degrees[cumulative]
    inv_degs  <- 1 / degs_cum
    vh_age    <- sum(V(g)$age[cumulative]    * inv_degs) / sum(inv_degs)
    vh_female <- sum(V(g)$female[cumulative] * inv_degs) / sum(inv_degs)

    list(
      Wave                 = wave - 1L,
      Participants         = length(pts),
      Mean_age             = if (length(pts) > 0) mean(V(g)$age[pts]) else NA_real_,
      Female               = if (length(pts) > 0) mean(V(g)$female[pts]) else NA_real_,
      Participants_cumulative  = n_cum,
      Mean_age_cumulative  = cum_age,
      Female_cumulative    = cum_female,
      VH_age_cumulative    = vh_age,
      VH_female_cumulative = vh_female
    )
  })

  rbindlist(lapply(rows, as.data.table))
}

# ============================================================
# 7. RUN ALL SCENARIOS
# ============================================================

cat("Running single-run scenarios...\n")

rrds_base <- run_rrds(g, seeds, NUM_WAVES, RECRUITS_PER_PERSON, n_target = N_TARGET)
rds_base  <- run_rds( g, seeds, NUM_WAVES, RECRUITS_PER_PERSON, ALPHA_REC, n_target = N_TARGET)

res_rrds_base <- compute_results(g, rrds_base)
res_rds_base  <- compute_results(g, rds_base)

# Reviewer 1 robustness scenarios
res_rrds_nr20 <- compute_results(g, run_rrds(g, seeds, NUM_WAVES, RECRUITS_PER_PERSON,
                                              nonresponse_rate = 0.20, n_target = N_TARGET))
res_rrds_nr40 <- compute_results(g, run_rrds(g, seeds, NUM_WAVES, RECRUITS_PER_PERSON,
                                              nonresponse_rate = 0.40, n_target = N_TARGET))
res_rrds_pe70 <- compute_results(g, run_rrds(g, seeds, NUM_WAVES, RECRUITS_PER_PERSON,
                                              enumeration_rate = 0.70, n_target = N_TARGET))
res_rrds_pe50 <- compute_results(g, run_rrds(g, seeds, NUM_WAVES, RECRUITS_PER_PERSON,
                                              enumeration_rate = 0.50, n_target = N_TARGET))
res_rrds_bias <- compute_results(g, run_rrds(g, seeds, NUM_WAVES, RECRUITS_PER_PERSON,
                                              enumeration_rate = 0.70,
                                              contact_bias = "same_group", n_target = N_TARGET))
res_rrds_hd      <- compute_results(g, run_rrds(g, seeds, NUM_WAVES, RECRUITS_PER_PERSON,
                                              enumeration_rate = 0.70,
                                              contact_bias = "high_degree", n_target = N_TARGET))
res_rrds_worst   <- compute_results(g, run_rrds(g, seeds, NUM_WAVES, RECRUITS_PER_PERSON,
                                              nonresponse_rate = 0.40,
                                              enumeration_rate = 0.50, n_target = N_TARGET))

cat("Single-run scenarios complete.\n\n")

# ============================================================
# 8. MONTE CARLO: CONFIDENCE INTERVALS ACROSS SIMULATIONS
# ============================================================
# Each run draws a fresh biased seed set (mostly young men) to estimate
# variability across different starting configurations.

run_mc_scenario <- function(g, n_sims, scenario_fn, ...) {
  young_men_all <- as.integer(V(g)[V(g)$age < 22 & V(g)$female == 0])

  all_results <- vector("list", n_sims)

  for (sim in seq_len(n_sims)) {
    set.seed(sim * 17)
    sim_seeds <- sample(young_men_all, min(10L, length(young_men_all)))

    pts            <- scenario_fn(g, sim_seeds, NUM_WAVES, RECRUITS_PER_PERSON,
                                  ..., n_target = N_TARGET, seed_offset = sim)
    res            <- compute_results(g, pts)
    res$sim        <- sim
    all_results[[sim]] <- res
  }

  rbindlist(all_results)
}

if (RUN_MC) {
  cat("Running Monte Carlo simulations (n =", N_SIMS, "each)...\n")
  mc_rrds_base <- run_mc_scenario(g, N_SIMS, run_rrds)
  mc_rds_base  <- run_mc_scenario(g, N_SIMS, run_rds,  alpha_rec = ALPHA_REC)
  mc_rrds_nr20 <- run_mc_scenario(g, N_SIMS, run_rrds, nonresponse_rate = 0.20)
  mc_rrds_nr40 <- run_mc_scenario(g, N_SIMS, run_rrds, nonresponse_rate = 0.40)
  mc_rrds_pe70 <- run_mc_scenario(g, N_SIMS, run_rrds, enumeration_rate = 0.70)
  mc_rrds_pe50 <- run_mc_scenario(g, N_SIMS, run_rrds, enumeration_rate = 0.50)
  mc_rrds_bias <- run_mc_scenario(g, N_SIMS, run_rrds, enumeration_rate = 0.70,
                                   contact_bias = "same_group")
  mc_rrds_hd   <- run_mc_scenario(g, N_SIMS, run_rrds, enumeration_rate = 0.70,
                                   contact_bias = "high_degree")
  cat("Monte Carlo complete.\n\n")
} else {
  cat("Skipping Monte Carlo (RUN_MC = FALSE).\n\n")
}

# ============================================================
# 9. SUMMARY TABLE
# ============================================================

final_bias_table <- function(res_list, labels, true_age, true_female) {
  rows <- mapply(function(res, label) {
    last <- res[nrow(res), ]
    data.frame(
      Scenario         = label,
      Final_n          = last$Participants_cumulative,
      Naive_Age        = round(last$Mean_age_cumulative, 2),
      VH_Age           = round(last$VH_age_cumulative, 2),
      Naive_Age_Bias   = round(last$Mean_age_cumulative - true_age, 2),
      VH_Age_Bias      = round(last$VH_age_cumulative   - true_age, 2),
      Naive_Female     = round(last$Female_cumulative, 3),
      VH_Female        = round(last$VH_female_cumulative, 3),
      Naive_Female_Bias= round(last$Female_cumulative - true_female, 3),
      VH_Female_Bias   = round(last$VH_female_cumulative - true_female, 3),
      stringsAsFactors = FALSE
    )
  }, res_list, labels, SIMPLIFY = FALSE)

  do.call(rbind, rows)
}

results_list <- list(
  res_rrds_base, res_rds_base,
  res_rrds_nr20, res_rrds_nr40,
  res_rrds_pe70, res_rrds_pe50,
  res_rrds_bias, res_rrds_hd,
  res_rrds_worst
)
scenario_labels <- c(
  "RRDS Baseline",
  "RDS (Preferential Recruitment)",
  "RRDS: 20% Non-response",
  "RRDS: 40% Non-response",
  "RRDS: 70% Contact Enumeration",
  "RRDS: 50% Contact Enumeration",
  "RRDS: Same-group Listing Bias",
  "RRDS: High-degree Listing Bias",
  "RRDS: 40% NR + 50% Enumeration"
)

summary_table <- final_bias_table(results_list, scenario_labels,
                                   TRUE_MEAN_AGE, TRUE_PROP_FEMALE)
cat("=== Final-wave estimate summary ===\n")
print(summary_table, row.names = FALSE)
cat("\n")

# ============================================================
# 10. PLOTTING
# ============================================================

source("plot_rds.R")

# Colors: consistent across all plots
scenario_colors <- c(
  "RRDS Baseline"                   = "#3498DB",
  "RDS (Preferential Recruitment)"  = "#E74C3C",
  "RRDS: 20% Non-response"          = "#2ECC71",
  "RRDS: 40% Non-response"          = "#1A8A4A",
  "RRDS: 70% Contact Enumeration"   = "#F39C12",
  "RRDS: 50% Contact Enumeration"   = "#E67E22",
  "RRDS: Same-group Listing Bias"   = "#9B59B6",
  "RRDS: High-degree Listing Bias"  = "#6C3483",
  "RRDS: 40% NR + 50% Enumeration" = "#27AE60"
)

# RDS = triangle (17), all RRDS variants = circle (16)
scenario_shapes <- c(
  "RRDS Baseline"                   = 16,
  "RDS (Preferential Recruitment)"  = 17,
  "RRDS: 20% Non-response"          = 16,
  "RRDS: 40% Non-response"          = 16,
  "RRDS: 70% Contact Enumeration"   = 16,
  "RRDS: 50% Contact Enumeration"   = 16,
  "RRDS: Same-group Listing Bias"   = 16,
  "RRDS: High-degree Listing Bias"  = 16,
  "RRDS: 40% NR + 50% Enumeration" = 16
)
main_shapes <- c(
  "RDS (Preferential Recruitment)"  = 17,
  "RRDS Baseline"                   = 16,
  "RRDS: 40% NR + 50% Enumeration" = 16
)

add_scenario <- function(dt, label) {
  dt2 <- copy(dt)
  dt2$Scenario <- label
  dt2
}

all_scenarios <- rbind(
  add_scenario(res_rrds_base,  "RRDS Baseline"),
  add_scenario(res_rds_base,   "RDS (Preferential Recruitment)"),
  add_scenario(res_rrds_nr20,  "RRDS: 20% Non-response"),
  add_scenario(res_rrds_nr40,  "RRDS: 40% Non-response"),
  add_scenario(res_rrds_pe70,  "RRDS: 70% Contact Enumeration"),
  add_scenario(res_rrds_pe50,  "RRDS: 50% Contact Enumeration"),
  add_scenario(res_rrds_bias,  "RRDS: Same-group Listing Bias"),
  add_scenario(res_rrds_hd,    "RRDS: High-degree Listing Bias"),
  add_scenario(res_rrds_worst, "RRDS: 40% NR + 50% Enumeration")
)

# Drop artefact rows after chain stopped (Participants = 0)
all_scenarios <- all_scenarios[Participants > 0]

all_scenarios$Scenario <- factor(all_scenarios$Scenario, levels = names(scenario_colors))


max_wave  <- max(all_scenarios$Wave)
max_cum_n <- max(all_scenarios$Participants_cumulative)

# Shared plot subtitle
# net_subtitle <- paste0(
#   "Network: n=", vcount(g), ", mean degree=", round(mean(degree(g)), 1),
#   "  |  α_net=", ALPHA_NET, "  |  α_rec=", ALPHA_REC,
#   "  |  N target=", N_TARGET, "  |  Seeds: 10 young men"
# )

# Helper: common theme
theme_conv <- function() {
  theme_minimal(base_size = 32) +
    theme(
      plot.title              = element_text(size = 24),
      axis.title              = element_text(size = 22),
      axis.text               = element_text(size = 22),
      # plot.subtitle         = element_text(color = "gray40", size = 10),
      legend.position         = c(0.98, 0.02),
      legend.justification    = c(1, 0),
      legend.background       = element_rect(fill = "white", color = "gray80", linewidth = 0.3),
      legend.margin           = margin(4, 6, 4, 6),
      legend.text             = element_text(size = 16),
      legend.title            = element_text(size = 0),
      panel.grid.minor        = element_blank()
    )
}

# ============================================================
# 10a. MAIN COMPARISON (3 scenarios, by wave)
#      RDS vs RRDS Baseline vs RRDS worst-case
# ============================================================

main_levels <- c(
  "RDS (Preferential Recruitment)",
  "RRDS Baseline",
  "RRDS: 40% NR + 50% Enumeration"
)
main_colors <- scenario_colors[main_levels]
main_dt     <- all_scenarios[Scenario %in% main_levels]
main_dt$Scenario <- factor(main_dt$Scenario, levels = main_levels)
main_wave   <- max(main_dt$Wave)

p_main_age <- ggplot(main_dt,
                      aes(x = Wave, y = Mean_age_cumulative, color = Scenario)) +
  geom_line(linewidth = 1.3) +
  geom_point(aes(shape = Scenario), size = 3.5) +
  geom_hline(yintercept = TRUE_MEAN_AGE, linetype = "dashed",
             color = "gray30", linewidth = 1) +
  annotate("text", x = main_wave * 0.98, y = TRUE_MEAN_AGE + 1.2,
           label = paste0("Population (", round(TRUE_MEAN_AGE, 1), ")"),
           hjust = 1, color = "gray30", size = 6) +
  scale_color_manual(values = main_colors, name = "Method") +
  scale_shape_manual(values = main_shapes, name = "Method") +
  scale_y_continuous(limits = c(19, 50)) +
  scale_x_continuous(breaks = seq(0, main_wave, by = 2)) +
  labs(title    = "Convergence to Population Mean Age",
       # subtitle = net_subtitle,
       x = "Wave", y = "Cumulative Mean Age (years)") +
  theme_conv()

p_main_female <- ggplot(main_dt,
                         aes(x = Wave, y = Female_cumulative * 100, color = Scenario)) +
  geom_line(linewidth = 1.3) +
  geom_point(aes(shape = Scenario), size = 3.5) +
  geom_hline(yintercept = TRUE_PROP_FEMALE * 100, linetype = "dashed",
             color = "gray30", linewidth = 1) +
  annotate("text", x = main_wave * 0.98, y = TRUE_PROP_FEMALE * 100 + 2.5,
           label = paste0("Population (", round(TRUE_PROP_FEMALE * 100, 1), "%)"),
           hjust = 1, color = "gray30", size = 6) +
  scale_color_manual(values = main_colors, name = "Method") +
  scale_shape_manual(values = main_shapes, name = "Method") +
  scale_y_continuous(limits = c(0, 80)) +
  scale_x_continuous(breaks = seq(0, main_wave, by = 2)) +
  labs(title    = "Convergence to Population % Female",
       # subtitle = net_subtitle,
       x = "Wave", y = "Cumulative % Female") +
  theme_conv()

ggsave("plots/main_age.pdf",    p_main_age,    width = 9, height = 6, device = "pdf")
ggsave("plots/main_female.pdf", p_main_female, width = 9, height = 6, device = "pdf")
cat("Saved: plots/main_age.pdf  plots/main_female.pdf\n")

# ============================================================
# 10b. FULL ROBUSTNESS (all scenarios, by wave)
# ============================================================

p_rob_age <- ggplot(all_scenarios,
                     aes(x = Wave, y = Mean_age_cumulative, color = Scenario)) +
  geom_line(linewidth = 1.1) +
  geom_point(aes(shape = Scenario), size = 2.5) +
  geom_hline(yintercept = TRUE_MEAN_AGE, linetype = "dashed",
             color = "gray30", linewidth = 1) +
  annotate("text", x = max_wave * 0.98, y = TRUE_MEAN_AGE + 1.2,
           label = paste0("Population (", round(TRUE_MEAN_AGE, 1), ")"),
           hjust = 1, color = "gray30", size = 6) +
  scale_color_manual(values = scenario_colors, name = "Sampling Condition") +
  scale_shape_manual(values = scenario_shapes, name = "Sampling Condition") +
  scale_y_continuous(limits = c(17, 50)) +
  scale_x_continuous(breaks = seq(0, max_wave, by = 2)) +
  labs(title    = "Robustness: Convergence to Population Mean Age",
       # subtitle = net_subtitle,
       x = "Wave", y = "Cumulative Mean Age (years)") +
  theme_conv()

p_rob_female <- ggplot(all_scenarios,
                        aes(x = Wave, y = Female_cumulative * 100, color = Scenario)) +
  geom_line(linewidth = 1.1) +
  geom_point(aes(shape = Scenario), size = 2.5) +
  geom_hline(yintercept = TRUE_PROP_FEMALE * 100, linetype = "dashed",
             color = "gray30", linewidth = 1) +
  annotate("text", x = max_wave * 0.98, y = TRUE_PROP_FEMALE * 100 + 2.5,
           label = paste0("Population (", round(TRUE_PROP_FEMALE * 100, 1), "%)"),
           hjust = 1, color = "gray30", size = 6) +
  scale_color_manual(values = scenario_colors, name = "Sampling Condition") +
  scale_shape_manual(values = scenario_shapes, name = "Sampling Condition") +
  scale_y_continuous(limits = c(0, 80)) +
  scale_x_continuous(breaks = seq(0, max_wave, by = 2)) +
  labs(title    = "Robustness: Convergence to Population % Female",
       # subtitle = net_subtitle,
       x = "Wave", y = "Cumulative % Female") +
  theme_conv()

ggsave("plots/robustness_age.pdf",    p_rob_age,    width = 11, height = 6, device = "pdf")
ggsave("plots/robustness_female.pdf", p_rob_female, width = 11, height = 6, device = "pdf")
cat("Saved: plots/robustness_age.pdf  plots/robustness_female.pdf\n")

# ============================================================
# 11. CONVERGENCE BY SAMPLE SIZE (x = cumulative n, not wave)
# ============================================================

# --- 11a. Main (3 scenarios) ---
p_main_by_n_age <- ggplot(main_dt,
                            aes(x = Participants_cumulative, y = Mean_age_cumulative,
                                color = Scenario)) +
  geom_line(linewidth = 1.3) +
  geom_point(aes(shape = Scenario), size = 3.5) +
  geom_hline(yintercept = TRUE_MEAN_AGE, linetype = "dashed",
             color = "gray30", linewidth = 1) +
  geom_vline(xintercept = N_TARGET, linetype = "dotted",
             color = "gray50", linewidth = 0.8) +
  annotate("text", x = N_TARGET * 0.97, y = 21.5,
           label = paste0("N=", N_TARGET), hjust = 1, color = "gray50", size = 4) +
  annotate("text", x = max(main_dt$Participants_cumulative) * 0.98,
           y = TRUE_MEAN_AGE + 1.2,
           label = paste0("Population (", round(TRUE_MEAN_AGE, 1), ")"),
           hjust = 1, color = "gray30", size = 6) +
  scale_color_manual(values = main_colors, name = "Method") +
  scale_shape_manual(values = main_shapes, name = "Method") +
  scale_y_continuous(limits = c(17, 50)) +
  scale_x_continuous(labels = scales::comma) +
  labs(title    = "Convergence to Population Mean Age",
       # subtitle = net_subtitle,
       x = "Cumulative Sample Size (n)", y = "Cumulative Mean Age (years)") +
  theme_conv()

p_main_by_n_female <- ggplot(main_dt,
                               aes(x = Participants_cumulative,
                                   y = Female_cumulative * 100, color = Scenario)) +
  geom_line(linewidth = 1.3) +
  geom_point(aes(shape = Scenario), size = 3.5) +
  geom_hline(yintercept = TRUE_PROP_FEMALE * 100, linetype = "dashed",
             color = "gray30", linewidth = 1) +
  geom_vline(xintercept = N_TARGET, linetype = "dotted",
             color = "gray50", linewidth = 0.8) +
  annotate("text", x = N_TARGET * 0.97, y = 4,
           label = paste0("N=", N_TARGET), hjust = 1, color = "gray50", size = 4) +
  annotate("text", x = max(main_dt$Participants_cumulative) * 0.98,
           y = TRUE_PROP_FEMALE * 100 + 2.5,
           label = paste0("Population (", round(TRUE_PROP_FEMALE * 100, 1), "%)"),
           hjust = 1, color = "gray30", size = 6) +
  scale_color_manual(values = main_colors, name = "Method") +
  scale_shape_manual(values = main_shapes, name = "Method") +
  scale_y_continuous(limits = c(0, 80)) +
  scale_x_continuous(labels = scales::comma) +
  labs(title    = "Convergence to Population % Female",
       # subtitle = net_subtitle,
       x = "Cumulative Sample Size (n)", y = "Cumulative % Female") +
  theme_conv()

ggsave("plots/main_by_n_age.pdf",    p_main_by_n_age,    width = 9, height = 6, device = "pdf")
ggsave("plots/main_by_n_female.pdf", p_main_by_n_female, width = 9, height = 6, device = "pdf")
cat("Saved: plots/main_by_n_age.pdf  plots/main_by_n_female.pdf\n")

# --- 11b. Full robustness by sample size ---
p_rob_by_n_age <- ggplot(all_scenarios,
                          aes(x = Participants_cumulative, y = Mean_age_cumulative,
                              color = Scenario)) +
  geom_line(linewidth = 1.1) +
  geom_point(aes(shape = Scenario), size = 2.5) +
  geom_hline(yintercept = TRUE_MEAN_AGE, linetype = "dashed",
             color = "gray30", linewidth = 1) +
  geom_vline(xintercept = N_TARGET, linetype = "dotted",
             color = "gray50", linewidth = 0.8) +
  annotate("text", x = N_TARGET * 0.97, y = 21.5,
           label = paste0("N=", N_TARGET), hjust = 1, color = "gray50", size = 4) +
  annotate("text", x = max_cum_n * 0.98, y = TRUE_MEAN_AGE + 1.2,
           label = paste0("Population (", round(TRUE_MEAN_AGE, 1), ")"),
           hjust = 1, color = "gray30", size = 6) +
  scale_color_manual(values = scenario_colors, name = "Sampling Condition") +
  scale_shape_manual(values = scenario_shapes, name = "Sampling Condition") +
  scale_y_continuous(limits = c(17, 50)) +
  scale_x_continuous(labels = scales::comma) +
  labs(title    = "Robustness: Mean Age vs. Sample Size",
       # subtitle = net_subtitle,
       x = "Cumulative Sample Size (n)", y = "Cumulative Mean Age (years)") +
  theme_conv()

p_rob_by_n_female <- ggplot(all_scenarios,
                              aes(x = Participants_cumulative,
                                  y = Female_cumulative * 100, color = Scenario)) +
  geom_line(linewidth = 1.1) +
  geom_point(aes(shape = Scenario), size = 2.5) +
  geom_hline(yintercept = TRUE_PROP_FEMALE * 100, linetype = "dashed",
             color = "gray30", linewidth = 1) +
  geom_vline(xintercept = N_TARGET, linetype = "dotted",
             color = "gray50", linewidth = 0.8) +
  annotate("text", x = N_TARGET * 0.97, y = 4,
           label = paste0("N=", N_TARGET), hjust = 1, color = "gray50", size = 4) +
  annotate("text", x = max_cum_n * 0.98, y = TRUE_PROP_FEMALE * 100 + 2.5,
           label = paste0("Population (", round(TRUE_PROP_FEMALE * 100, 1), "%)"),
           hjust = 1, color = "gray30", size = 6) +
  scale_color_manual(values = scenario_colors, name = "Sampling Condition") +
  scale_shape_manual(values = scenario_shapes, name = "Sampling Condition") +
  scale_y_continuous(limits = c(0, 80)) +
  scale_x_continuous(labels = scales::comma) +
  labs(title    = "Robustness: % Female vs. Sample Size",
       # subtitle = net_subtitle,
       x = "Cumulative Sample Size (n)", y = "Cumulative % Female") +
  theme_conv()

ggsave("plots/robustness_by_n_age.pdf",    p_rob_by_n_age,    width = 11, height = 6, device = "pdf")
ggsave("plots/robustness_by_n_female.pdf", p_rob_by_n_female, width = 11, height = 6, device = "pdf")
cat("Saved: plots/robustness_by_n_age.pdf  plots/robustness_by_n_female.pdf\n\n")

# ============================================================
# 13. MONTE CARLO RIBBON PLOTS (only when RUN_MC = TRUE)
# ============================================================

build_mc_summary <- function(mc_dt, label, var) {
  mc_dt[, .(
    mean_val = mean(get(var), na.rm = TRUE),
    lo95     = quantile(get(var), 0.025, na.rm = TRUE),
    hi95     = quantile(get(var), 0.975, na.rm = TRUE),
    Scenario = label
  ), by = Wave]
}

if (RUN_MC) {

mc_age_list <- list(
  build_mc_summary(mc_rrds_base, "RRDS Baseline",                  "Mean_age_cumulative"),
  build_mc_summary(mc_rds_base,  "RDS (Preferential Recruitment)",  "Mean_age_cumulative"),
  build_mc_summary(mc_rrds_nr20, "RRDS: 20% Non-response",          "Mean_age_cumulative"),
  build_mc_summary(mc_rrds_nr40, "RRDS: 40% Non-response",          "Mean_age_cumulative"),
  build_mc_summary(mc_rrds_pe50, "RRDS: 50% Contact Enumeration",   "Mean_age_cumulative"),
  build_mc_summary(mc_rrds_bias, "RRDS: Same-group Listing Bias",   "Mean_age_cumulative"),
  build_mc_summary(mc_rrds_hd,   "RRDS: High-degree Listing Bias",  "Mean_age_cumulative")
)
mc_age <- rbindlist(mc_age_list)
mc_age$Scenario <- factor(mc_age$Scenario, levels = names(scenario_colors))

mc_female_list <- list(
  build_mc_summary(mc_rrds_base, "RRDS Baseline",                  "Female_cumulative"),
  build_mc_summary(mc_rds_base,  "RDS (Preferential Recruitment)",  "Female_cumulative"),
  build_mc_summary(mc_rrds_nr20, "RRDS: 20% Non-response",          "Female_cumulative"),
  build_mc_summary(mc_rrds_nr40, "RRDS: 40% Non-response",          "Female_cumulative"),
  build_mc_summary(mc_rrds_pe50, "RRDS: 50% Contact Enumeration",   "Female_cumulative"),
  build_mc_summary(mc_rrds_bias, "RRDS: Same-group Listing Bias",   "Female_cumulative"),
  build_mc_summary(mc_rrds_hd,   "RRDS: High-degree Listing Bias",  "Female_cumulative")
)
mc_female <- rbindlist(mc_female_list)
mc_female[, c("mean_val", "lo95", "hi95") :=
            .(mean_val * 100, lo95 * 100, hi95 * 100)]
mc_female$Scenario <- factor(mc_female$Scenario, levels = names(scenario_colors))

max_mc_wave <- max(mc_age$Wave)

p_mc_age <- ggplot(mc_age,
                    aes(x = Wave, y = mean_val,
                        color = Scenario, fill = Scenario)) +
  geom_ribbon(aes(ymin = lo95, ymax = hi95), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.1) +
  geom_hline(yintercept = TRUE_MEAN_AGE, linetype = "dashed",
             color = "gray30", linewidth = 1) +
  annotate("text", x = max_mc_wave * 0.98, y = TRUE_MEAN_AGE + 1.2,
           label = paste0("Population (", round(TRUE_MEAN_AGE, 1), ")"),
           hjust = 1, color = "gray30", size = 6) +
  scale_color_manual(values = scenario_colors) +
  scale_fill_manual(values = scenario_colors) +
  scale_y_continuous(limits = c(17, 50)) +
  scale_x_continuous(breaks = seq(0, max_mc_wave, by = 2)) +
  labs(
    title    = paste0("A. Mean Age: Monte Carlo (n=", N_SIMS, " runs)"),
    # subtitle = "Line = mean across runs; ribbon = 95% interval",
    x        = "Wave",
    y        = "Cumulative Mean Age (years)",
    color    = "Sampling Condition",
    fill     = "Sampling Condition"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(color = "gray40", size = 10),
    legend.position  = "right",
    legend.text      = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

p_mc_female <- ggplot(mc_female,
                       aes(x = Wave, y = mean_val,
                           color = Scenario, fill = Scenario)) +
  geom_ribbon(aes(ymin = lo95, ymax = hi95), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.1) +
  geom_hline(yintercept = TRUE_PROP_FEMALE * 100, linetype = "dashed",
             color = "gray30", linewidth = 1) +
  annotate("text", x = max_mc_wave * 0.98, y = TRUE_PROP_FEMALE * 100 + 2.5,
           label = paste0("Population (", round(TRUE_PROP_FEMALE * 100, 1), "%)"),
           hjust = 1, color = "gray30", size = 6) +
  scale_color_manual(values = scenario_colors) +
  scale_fill_manual(values = scenario_colors) +
  scale_y_continuous(limits = c(30, 90)) +
  scale_x_continuous(breaks = seq(0, max_mc_wave, by = 2)) +
  labs(
    title    = paste0("B. % Female: Monte Carlo (n=", N_SIMS, " runs)"),
    # subtitle = "Line = mean across runs; ribbon = 95% interval",
    x        = "Wave",
    y        = "Cumulative % Female",
    color    = "Sampling Condition",
    fill     = "Sampling Condition"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold"),
    # plot.subtitle    = element_text(color = "gray40", size = 10),
    legend.position  = "right",
    legend.text      = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

p_mc_combined <- (p_mc_age / p_mc_female) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

p_mc_combined <- p_mc_combined +
  plot_annotation(
    title    = "RRDS Robustness: Monte Carlo Analysis",
    # subtitle = paste0(
    #   "Network: n=", vcount(g), ", mean degree=", round(mean(degree(g)), 1),
    #   "  |  α_net=", ALPHA_NET, "  |  α_rec=", ALPHA_REC,
    #   "\nSeeds biased toward young men for each run"
    # ),
    theme = theme(
      plot.title    = element_text(face = "bold", size = 16),
      # plot.subtitle = element_text(color = "gray40", size = 11)
    )
  )

ggsave("plots/robustness_mc.pdf", p_mc_combined,
       width = 13, height = 13, device = "pdf")
cat("Saved: plots/robustness_mc.pdf\n")

} # end if (RUN_MC)

# ============================================================
# 13. VH ESTIMATOR COMPARISON (Reviewer 2)
#
# Demonstrates that the VH estimator reduces bias in RDS relative to
# the naive sample mean, and that RRDS + VH provides the best estimates.
# ============================================================

vh_dt <- rbind(
  data.table(Wave  = res_rds_base$Wave,
             Naive = res_rds_base$Mean_age_cumulative,
             VH    = res_rds_base$VH_age_cumulative,
             Method = "RDS"),
  data.table(Wave  = res_rrds_base$Wave,
             Naive = res_rrds_base$Mean_age_cumulative,
             VH    = res_rrds_base$VH_age_cumulative,
             Method = "RRDS")
)

vh_long <- melt(vh_dt, id.vars = c("Wave", "Method"),
                measure.vars = c("Naive", "VH"),
                variable.name = "Estimator", value.name = "MeanAge")
vh_long$Label <- paste(vh_long$Method, vh_long$Estimator)
vh_long$Label <- factor(vh_long$Label,
                         levels = c("RDS Naive", "RDS VH", "RRDS Naive", "RRDS VH"))

vh_colors <- c("RDS Naive"  = "#E74C3C", "RDS VH"  = "#922B21",
               "RRDS Naive" = "#3498DB", "RRDS VH" = "#1A5276")

p_vh <- ggplot(vh_long,
               aes(x = Wave, y = MeanAge, color = Label, linetype = Estimator)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_hline(yintercept = TRUE_MEAN_AGE, linetype = "dashed",
             color = "gray30", linewidth = 1) +
  annotate("text", x = max(vh_long$Wave) * 0.98, y = TRUE_MEAN_AGE + 1.2,
           label = paste0("Population (", round(TRUE_MEAN_AGE, 1), ")"),
           hjust = 1, color = "gray30", size = 5) +
  scale_color_manual(values = vh_colors, name = "Method & Estimator") +
  scale_linetype_manual(values = c("Naive" = "solid", "VH" = "dashed"),
                        name = "Estimator") +
  scale_y_continuous(limits = c(17, 50)) +
  scale_x_continuous(breaks = seq(0, max(vh_long$Wave), by = 2)) +
  labs(
    title    = "Naive vs. Volz-Heckathorn (Degree-Weighted) Estimator",
    # subtitle = paste0(
  #     "VH estimator corrects for degree-based sampling probability\n",
  #     "Dashed = VH estimator  |  Solid = naive sample mean"
  #   ),
  #   x = "Wave",
  #   y = "Cumulative Mean Age (years)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title       = element_text(face = "bold", size = 16),
    # plot.subtitle    = element_text(color = "gray40", size = 12),
    legend.position  = "right",
    panel.grid.minor = element_blank()
  )

ggsave("plots/robustness_vh_estimator.pdf", p_vh,
       width = 10, height = 6, device = "pdf")
cat("Saved: plots/robustness_vh_estimator.pdf\n")

# ============================================================
# 14. BASELINE COMPARISON USING ORIGINAL PLOT FUNCTIONS
#     (for direct comparison with paper figures)
# ============================================================

rrds_dt_base <- as.data.table(res_rrds_base)
rds_dt_base  <- as.data.table(res_rds_base)

three_panel_rob <- plot_three_panel(rds_dt_base, rrds_dt_base)
save_plot(three_panel_rob, "plots/robustness_three_panel.pdf", width = 12, height = 4)
cat("Saved: plots/robustness_three_panel.pdf\n")

cat("\n=== True population values in network ===\n")
cat("Mean age:     ", round(TRUE_MEAN_AGE, 2), "\n")
cat("Prop female:  ", round(TRUE_PROP_FEMALE, 3), "\n")
cat("Mean degree:  ", round(mean(degree(g)), 2), "\n")
cat("\nAll plots saved to plots/\n")
