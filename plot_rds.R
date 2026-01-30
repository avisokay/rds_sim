# plot_rds.R
# Plotting functions for RDS simulation results

library(ggplot2)
library(data.table)

# --- SINGLE METHOD PLOTS ---

# Plot wave-by-wave statistics for a single method
plot_wave_data <- function(dt, custom_title, num_waves = NULL, seed_n = NULL) {
  if (!is.data.table(dt)) {
    stop("Input must be a data.table.")
  }

  if (is.null(num_waves)) num_waves <- max(dt$Wave)
  if (is.null(seed_n)) seed_n <- dt$Participants[1]

  p <- ggplot(dt, aes(x = Wave)) +
    geom_point(aes(y = Mean_age, color = "Mean Age", size = Participants), alpha = 0.6) +
    geom_point(aes(y = Female * 100, color = "Female", size = Participants), alpha = 0.6) +
    scale_size_continuous(range = c(1, max(dt$Participants) / min(dt$Participants[dt$Participants > 0]))) +
    scale_y_continuous(
      name = "Mean Age (yr)",
      breaks = seq(0, 100, by = 10),
      limits = c(0, 100),
      sec.axis = sec_axis(~ ., name = "Percent Female")
    ) +
    scale_color_manual(values = c("Mean Age" = "darkblue", "Female" = "tomato")) +
    labs(title = custom_title, x = "Wave") +
    scale_x_continuous(breaks = seq(0, num_waves, by = 1), limits = c(0, num_waves)) +
    theme_minimal() +
    theme(
      axis.title.y.right = element_text(color = "tomato"),
      axis.text.y.right = element_text(color = "tomato"),
      axis.title.y.left = element_text(color = "darkblue"),
      axis.text.y.left = element_text(color = "darkblue"),
      legend.position = "none"
    ) +
    geom_hline(yintercept = 41.5, linetype = "dashed", color = "darkblue") +
    geom_hline(yintercept = 70, linetype = "dashed", color = "tomato") +
    annotate("text", x = 3.32, y = 44, label = "Population Average", hjust = 1, color = "darkblue") +
    annotate("text", x = 3.65, y = 73, label = "Population Proportion", hjust = 1, color = "tomato") +
    annotate("text", x = 1.8, y = 18, label = paste0("Seed n=", seed_n), hjust = 1, color = "black")

  print(p)
  invisible(p)
}

# Plot cumulative statistics for a single method
plot_cumulative_data <- function(dt, custom_title, num_waves = NULL, burn_in_waves = 3) {
  if (!is.data.table(dt)) {
    stop("Input must be a data.table.")
  }

  if (is.null(num_waves)) num_waves <- max(dt$Wave)

  p <- ggplot(dt, aes(x = Wave)) +
    geom_rect(aes(xmin = 0, xmax = burn_in_waves - 0.5, ymin = -Inf, ymax = Inf),
              fill = "lightgrey", alpha = 0.3) +
    geom_rect(aes(xmin = num_waves - 0.5, xmax = num_waves + 0.5, ymin = -Inf, ymax = Inf),
              fill = "lightgrey", alpha = 0.3) +
    geom_point(aes(y = Mean_age_cumulative, color = "Mean Age", size = Participants_cumulative), alpha = 0.6) +
    geom_point(aes(y = Mean_age_cumulative), color = "blue", size = 1) +
    geom_point(aes(y = Female_cumulative * 100, color = "Female", size = Participants_cumulative), alpha = 0.6) +
    geom_point(aes(y = Female_cumulative * 100), color = "red", size = 1) +
    scale_size_continuous(range = c(1, log(max(dt$Participants_cumulative) / min(dt$Participants_cumulative), 1.3))) +
    scale_y_continuous(
      name = "Mean Age (yr)",
      breaks = seq(0, 100, by = 10),
      limits = c(0, 100),
      sec.axis = sec_axis(~ ., name = "Percent Female", breaks = seq(0, 100, by = 10))
    ) +
    scale_color_manual(values = c("Mean Age" = "darkblue", "Female" = "tomato")) +
    labs(title = custom_title, x = "Wave") +
    scale_x_continuous(breaks = seq(0, num_waves, by = 1), limits = c(0, num_waves + 0.5)) +
    theme_minimal() +
    theme(
      axis.title.y.right = element_text(color = "tomato", size = 14),
      axis.text.y.right = element_text(color = "tomato", size = 12),
      axis.title.y.left = element_text(color = "darkblue", size = 14),
      axis.text.y.left = element_text(color = "darkblue", size = 12),
      axis.title.x = element_text(size = 14),
      axis.text.x = element_text(size = 12),
      legend.position = "none"
    ) +
    geom_hline(yintercept = 41.5, linetype = "dashed", color = "darkblue") +
    geom_hline(yintercept = 70, linetype = "dashed", color = "tomato") +
    annotate("text", x = 3.32, y = 44, label = "Population Average", hjust = 1, color = "darkblue") +
    annotate("text", x = 3.65, y = 73, label = "Population Proportion", hjust = 1, color = "tomato") +
    annotate("text", x = burn_in_waves / 2, y = 90, label = paste0("Burn In: ", burn_in_waves, " Waves"),
             hjust = 0.5, color = "black", fontface = "bold") +
    annotate("text", x = burn_in_waves / 2, y = 18,
             label = paste0("Seed n=", min(dt$Participants_cumulative)),
             hjust = 0.5, color = "black", fontface = "bold") +
    annotate("text", x = num_waves + 0.4, y = 18,
             label = paste0("n=", max(dt$Participants_cumulative)),
             hjust = 1, color = "black", fontface = "bold")

  print(p)
  invisible(p)
}

# --- COMPARISON PLOTS (RDS vs RRDS) ---

# Plot wave comparison between two methods (RDS vs RRDS)
plot_wave_comparison <- function(rds_dt, rrds_dt, custom_title, global_max_participants = NULL, seed_n = NULL) {
  if (!is.data.table(rds_dt) || !is.data.table(rrds_dt)) {
    stop("Both inputs must be data.tables.")
  }

  rds_dt_plot <- copy(rds_dt)
  rrds_dt_plot <- copy(rrds_dt)
  rds_dt_plot$Method <- "RDS"
  rrds_dt_plot$Method <- "RRDS"

  combined_dt <- rbind(rds_dt_plot, rrds_dt_plot)

  max_participants <- if (!is.null(global_max_participants)) global_max_participants else max(combined_dt$Participants, na.rm = TRUE)
  if (is.null(seed_n)) seed_n <- rds_dt$Participants[1]

  p <- ggplot(combined_dt, aes(x = Wave)) +
    geom_point(aes(y = Mean_age, color = "Mean Age", size = Participants), alpha = 0.7,
               show.legend = c(color = FALSE, size = TRUE)) +
    geom_point(aes(y = Female * 100, color = "Female", size = Participants), alpha = 0.7,
               show.legend = c(color = FALSE, size = TRUE)) +
    facet_wrap(~ Method, ncol = 2, scales = "free_x") +
    scale_size_continuous(range = c(3, 10), name = "Number of Participants", limits = c(0, max_participants)) +
    scale_color_manual(values = c("Mean Age" = "darkblue", "Female" = "tomato"), name = "Measure") +
    scale_y_continuous(
      name = "Mean Age (yr)",
      breaks = seq(0, 100, by = 10),
      limits = c(0, 100),
      sec.axis = sec_axis(~ ., name = "Percent Female")
    ) +
    labs(title = custom_title, x = "Wave") +
    scale_x_continuous(breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 1)) +
    theme_minimal(base_size = 16) +
    theme(
      axis.title.y.right = element_text(color = "tomato", size = 16, face = "bold"),
      axis.text.y.right = element_text(color = "tomato", size = 14),
      axis.title.y.left = element_text(color = "darkblue", size = 16, face = "bold"),
      axis.text.y.left = element_text(color = "darkblue", size = 14),
      axis.title.x = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(size = 14),
      legend.position = "bottom",
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16, face = "bold"),
      strip.text = element_text(size = 16, face = "bold"),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
    ) +
    geom_hline(yintercept = 41.5, linetype = "dashed", color = "darkblue", alpha = 0.7) +
    geom_hline(yintercept = 70, linetype = "dashed", color = "tomato", alpha = 0.7) +
    annotate("text", x = Inf, y = 30, label = "Population Mean", hjust = 1.1, color = "darkblue", size = 6) +
    annotate("text", x = Inf, y = 90, label = "Population Proportion", hjust = 1.1, color = "tomato", size = 6) +
    annotate("text", x = 1, y = 18, label = paste0("Seed n=", seed_n), hjust = 0, color = "black", size = 6)

  print(p)
  invisible(p)
}

# Plot cumulative comparison between two methods (RDS vs RRDS)
plot_cumulative_comparison <- function(rds_dt, rrds_dt, custom_title, global_max_participants = NULL, seed_n = NULL) {
  if (!is.data.table(rds_dt) || !is.data.table(rrds_dt)) {
    stop("Both inputs must be data.tables.")
  }

  rds_dt_plot <- copy(rds_dt)
  rrds_dt_plot <- copy(rrds_dt)
  rds_dt_plot$Method <- "RDS"
  rrds_dt_plot$Method <- "RRDS"

  combined_dt <- rbind(rds_dt_plot, rrds_dt_plot)

  max_participants <- if (!is.null(global_max_participants)) global_max_participants else max(combined_dt$Participants_cumulative, na.rm = TRUE)
  if (is.null(seed_n)) seed_n <- rds_dt$Participants_cumulative[1]

  p <- ggplot(combined_dt, aes(x = Wave)) +
    geom_point(aes(y = Mean_age_cumulative, color = "Mean Age", size = Participants_cumulative), alpha = 0.7,
               show.legend = c(color = FALSE, size = TRUE)) +
    geom_point(aes(y = Female_cumulative * 100, color = "Female", size = Participants_cumulative), alpha = 0.7,
               show.legend = c(color = FALSE, size = TRUE)) +
    facet_wrap(~ Method, ncol = 2, scales = "free_x") +
    scale_size_continuous(range = c(3, 10), name = "Number of Participants", limits = c(0, max_participants)) +
    scale_color_manual(values = c("Mean Age" = "darkblue", "Female" = "tomato"), name = "Measure") +
    scale_y_continuous(
      name = "Mean Age (yr)",
      breaks = seq(0, 100, by = 10),
      limits = c(0, 100),
      sec.axis = sec_axis(~ ., name = "Percent Female")
    ) +
    labs(title = custom_title, x = "Wave") +
    scale_x_continuous(breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 1)) +
    theme_minimal(base_size = 16) +
    theme(
      axis.title.y.right = element_text(color = "tomato", size = 16, face = "bold"),
      axis.text.y.right = element_text(color = "tomato", size = 14),
      axis.title.y.left = element_text(color = "darkblue", size = 16, face = "bold"),
      axis.text.y.left = element_text(color = "darkblue", size = 14),
      axis.title.x = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(size = 14),
      legend.position = "bottom",
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16, face = "bold"),
      strip.text = element_text(size = 16, face = "bold"),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
    ) +
    geom_hline(yintercept = 41.5, linetype = "dashed", color = "darkblue", alpha = 0.7) +
    geom_hline(yintercept = 70, linetype = "dashed", color = "tomato", alpha = 0.7) +
    annotate("text", x = Inf, y = 30, label = "Population Mean", hjust = 1.1, color = "darkblue", size = 6) +
    annotate("text", x = Inf, y = 80, label = "Population Proportion", hjust = 1.1, color = "tomato", size = 6) +
    annotate("text", x = 0, y = 15, label = paste0("Seed n=", seed_n), hjust = 0, color = "black", size = 6)

  print(p)
  invisible(p)
}

# Plot combined 2x2 facet (Wave vs Cumulative, RDS vs RRDS)
plot_combined_comparison <- function(rds_dt, rrds_dt, custom_title, global_max_participants = NULL) {
  if (!is.data.table(rds_dt) || !is.data.table(rrds_dt)) {
    stop("Both inputs must be data.tables.")
  }

  # Prepare wave data
  rds_wave <- copy(rds_dt)
  rrds_wave <- copy(rrds_dt)
  rds_wave$Method <- "RDS"
  rrds_wave$Method <- "RRDS"
  rds_wave$Analysis_Type <- "Wave"
  rrds_wave$Analysis_Type <- "Wave"
  rds_wave$Mean_age_plot <- rds_wave$Mean_age
  rrds_wave$Mean_age_plot <- rrds_wave$Mean_age
  rds_wave$Female_plot <- rds_wave$Female
  rrds_wave$Female_plot <- rrds_wave$Female
  rds_wave$Participants_plot <- rds_wave$Participants
  rrds_wave$Participants_plot <- rrds_wave$Participants

  # Prepare cumulative data
  rds_cumulative <- copy(rds_dt)
  rrds_cumulative <- copy(rrds_dt)
  rds_cumulative$Method <- "RDS"
  rrds_cumulative$Method <- "RRDS"
  rds_cumulative$Analysis_Type <- "Cumulative"
  rrds_cumulative$Analysis_Type <- "Cumulative"
  rds_cumulative$Mean_age_plot <- rds_cumulative$Mean_age_cumulative
  rrds_cumulative$Mean_age_plot <- rrds_cumulative$Mean_age_cumulative
  rds_cumulative$Female_plot <- rds_cumulative$Female_cumulative
  rrds_cumulative$Female_plot <- rrds_cumulative$Female_cumulative
  rds_cumulative$Participants_plot <- rds_cumulative$Participants_cumulative
  rrds_cumulative$Participants_plot <- rrds_cumulative$Participants_cumulative

  # Combine all data
  combined_dt <- rbind(
    rds_wave[, .(Wave, Method, Analysis_Type, Mean_age_plot, Female_plot, Participants_plot)],
    rrds_wave[, .(Wave, Method, Analysis_Type, Mean_age_plot, Female_plot, Participants_plot)],
    rds_cumulative[, .(Wave, Method, Analysis_Type, Mean_age_plot, Female_plot, Participants_plot)],
    rrds_cumulative[, .(Wave, Method, Analysis_Type, Mean_age_plot, Female_plot, Participants_plot)]
  )

  # Set factor levels
  combined_dt$Analysis_Type <- factor(combined_dt$Analysis_Type, levels = c("Wave", "Cumulative"))
  combined_dt$Method <- factor(combined_dt$Method, levels = c("RDS", "RRDS"))

  max_participants <- if (!is.null(global_max_participants)) global_max_participants else max(combined_dt$Participants_plot, na.rm = TRUE)

  p <- ggplot(combined_dt, aes(x = Wave)) +
    geom_point(aes(y = Mean_age_plot, color = "Mean Age", size = Participants_plot), alpha = 0.7) +
    geom_point(aes(y = Female_plot * 100, color = "Female", size = Participants_plot), alpha = 0.7) +
    facet_grid(Analysis_Type ~ Method, scales = "free_x",
               switch = "y",
               labeller = labeller(Analysis_Type = c("Wave" = "Wave Analysis", "Cumulative" = "Cumulative Analysis"))) +
    scale_size_continuous(range = c(3, 10), name = "Participants", limits = c(0, max_participants)) +
    scale_color_manual(values = c("Mean Age" = "darkblue", "Female" = "tomato"), name = "Measure") +
    scale_y_continuous(
      name = "Mean Age (yr)",
      breaks = seq(20, 90, by = 10),
      limits = c(20, 90),
      sec.axis = sec_axis(~ ., name = "Percent Female")
    ) +
    labs(title = custom_title, x = "Wave") +
    scale_x_continuous(breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 1)) +
    theme_minimal(base_size = 16) +
    theme(
      axis.title.y.right = element_text(color = "tomato", size = 16, face = "bold"),
      axis.text.y.right = element_text(color = "tomato", size = 14),
      axis.title.y.left = element_text(color = "darkblue", size = 16, face = "bold"),
      axis.text.y.left = element_text(color = "darkblue", size = 14),
      axis.title.x = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(size = 14),
      legend.position = "bottom",
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16, face = "bold"),
      strip.text = element_text(size = 16, face = "bold"),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
    ) +
    geom_hline(yintercept = 41.5, linetype = "dashed", color = "darkblue", alpha = 0.7) +
    geom_hline(yintercept = 70, linetype = "dashed", color = "tomato", alpha = 0.7) +
    annotate("text", x = Inf, y = 30, label = "Population Mean", hjust = 1.1, color = "darkblue", size = 5) +
    annotate("text", x = Inf, y = 85, label = "Population Proportion", hjust = 1.1, color = "tomato", size = 5)

  print(p)
  invisible(p)
}

# --- RDS vs PROBABILITY SAMPLING PLOTS ---

# Plot cumulative comparison of RDS vs Probability Sampling (age only)
plot_rds_vs_ps_cumulative <- function(dt, custom_title, num_waves = NULL, burn_in_waves = 3) {
  if (!is.data.table(dt)) {
    stop("Input must be a data.table.")
  }

  if (is.null(num_waves)) num_waves <- max(dt$Wave)

  p <- ggplot(dt, aes(x = Wave)) +
    geom_rect(aes(xmin = 0, xmax = burn_in_waves - 0.5, ymin = -Inf, ymax = Inf),
              fill = "lightgrey", alpha = 0.3) +
    # RDS points (circles)
    geom_point(aes(y = Mean_age_cumulative), color = "darkblue",
               size = 3, shape = 16, alpha = 0.8) +
    # Probability Sampling points (squares)
    geom_point(aes(y = PS_mean_age_cumulative), color = "darkblue",
               size = 3, shape = 15, alpha = 0.8) +
    scale_y_continuous(
      name = "Mean Age (yr)",
      breaks = seq(0, 100, by = 10),
      limits = c(0, 100)
    ) +
    scale_color_manual(values = c("Mean Age" = "darkblue", "Female" = "tomato")) +
    labs(title = custom_title, x = "Wave") +
    scale_x_continuous(breaks = seq(0, num_waves, by = 1), limits = c(0, num_waves + 0.5)) +
    theme_minimal() +
    theme(
      axis.title.y.right = element_text(color = "tomato", size = 14),
      axis.text.y.right = element_text(color = "tomato", size = 12),
      axis.title.y.left = element_text(color = "darkblue", size = 14),
      axis.text.y.left = element_text(color = "darkblue", size = 12),
      axis.title.x = element_text(size = 14),
      axis.text.x = element_text(size = 12),
      legend.position = "none"
    ) +
    geom_hline(yintercept = 41.5, linetype = "dashed", color = "darkblue") +
    annotate("text", x = 2.4, y = 50, label = "Population Mean (41)", hjust = 1, color = "darkblue") +
    annotate("text", x = burn_in_waves / 2, y = 90, label = paste0("Burn In: ", burn_in_waves, " Waves"),
             hjust = 0.5, color = "black", fontface = "bold") +
    annotate("text", x = 1.4, y = 18, label = paste0("Seed n=", min(dt$Participants_cumulative)),
             hjust = 1, color = "black", fontface = "bold") +
    annotate("text", x = num_waves, y = 25, label = "Square: Probability Sample",
             hjust = 1, color = "black", fontface = "bold") +
    annotate("text", x = num_waves - 2, y = 18, label = "Circle: RDS",
             hjust = 1, color = "black", fontface = "bold")

  print(p)
  invisible(p)
}

# --- CLEAN COMPARISON PLOTS (Simplified) ---

# Simple line plot: Cumulative Mean Age over waves (RDS vs RRDS)
plot_convergence_age <- function(rds_dt, rrds_dt, pop_mean = 41.5) {
  if (!is.data.table(rds_dt) || !is.data.table(rrds_dt)) {
    stop("Both inputs must be data.tables.")
  }

  rds_dt_plot <- copy(rds_dt)
  rrds_dt_plot <- copy(rrds_dt)
  rds_dt_plot$Method <- "RDS"
  rrds_dt_plot$Method <- "RRDS"

  combined_dt <- rbind(rds_dt_plot, rrds_dt_plot)

  max_wave <- max(combined_dt$Wave)

  p <- ggplot(combined_dt, aes(x = Wave, y = Mean_age_cumulative, color = Method)) +
    geom_line(linewidth = 1.4) +
    geom_point(size = 4) +
    geom_hline(yintercept = pop_mean, linetype = "dashed", color = "gray40", linewidth = 1) +
    annotate("text", x = max_wave, y = pop_mean + 1.5,
             label = paste0("Population Mean (", pop_mean, ")"),
             hjust = 1, color = "gray40", size = 6) +
    scale_color_manual(values = c("RDS" = "#E74C3C", "RRDS" = "#3498DB")) +
    scale_y_continuous(limits = c(20, 50)) +
    scale_x_continuous(breaks = seq(0, max_wave, by = 2), limits = c(0, max_wave)) +
    labs(
      title = "Convergence to Population Mean Age",
      subtitle = "Cumulative sample mean by recruitment wave",
      x = "Wave",
      y = "Cumulative Mean Age (years)",
      color = "Method"
    ) +
    theme_minimal(base_size = 18) +
    theme(
      plot.title = element_text(face = "bold", size = 22),
      plot.subtitle = element_text(color = "gray40", size = 16),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 16),
      legend.text = element_text(size = 16),
      legend.title = element_blank(),
      legend.position = "inside",
      legend.position.inside = c(0.85, 0.25),
      legend.background = element_rect(fill = "white", color = NA),
      panel.grid.minor = element_blank()
    )

  print(p)
  invisible(p)
}

# Simple line plot: Cumulative Proportion Female over waves (RDS vs RRDS)
plot_convergence_female <- function(rds_dt, rrds_dt, pop_prop = 0.70) {
  if (!is.data.table(rds_dt) || !is.data.table(rrds_dt)) {
    stop("Both inputs must be data.tables.")
  }

  rds_dt_plot <- copy(rds_dt)
  rrds_dt_plot <- copy(rrds_dt)
  rds_dt_plot$Method <- "RDS"
  rrds_dt_plot$Method <- "RRDS"

  combined_dt <- rbind(rds_dt_plot, rrds_dt_plot)

  max_wave <- max(combined_dt$Wave)

  p <- ggplot(combined_dt, aes(x = Wave, y = Female_cumulative * 100, color = Method)) +
    geom_line(linewidth = 1.4) +
    geom_point(size = 4) +
    geom_hline(yintercept = pop_prop * 100, linetype = "dashed", color = "gray40", linewidth = 1) +
    annotate("text", x = max_wave, y = pop_prop * 100 + 3,
             label = paste0("Population (", pop_prop * 100, "%)"),
             hjust = 1, color = "gray40", size = 6) +
    scale_color_manual(values = c("RDS" = "#E74C3C", "RRDS" = "#3498DB")) +
    scale_y_continuous(limits = c(20, 80)) +
    scale_x_continuous(breaks = seq(0, max_wave, by = 2), limits = c(0, max_wave)) +
    labs(
      title = "Convergence to Population Proportion Female",
      subtitle = "Cumulative sample proportion by recruitment wave",
      x = "Wave",
      y = "Cumulative % Female",
      color = "Method"
    ) +
    theme_minimal(base_size = 18) +
    theme(
      plot.title = element_text(face = "bold", size = 22),
      plot.subtitle = element_text(color = "gray40", size = 16),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 16),
      legend.text = element_text(size = 16),
      legend.title = element_blank(),
      legend.position = "inside",
      legend.position.inside = c(0.85, 0.25),
      legend.background = element_rect(fill = "white", color = NA),
      panel.grid.minor = element_blank()
    )

  print(p)
  invisible(p)
}

# Simple line plot: Cumulative Sample Size over waves (RDS vs RRDS)
plot_sample_size <- function(rds_dt, rrds_dt) {
  if (!is.data.table(rds_dt) || !is.data.table(rrds_dt)) {
    stop("Both inputs must be data.tables.")
  }

  rds_dt_plot <- copy(rds_dt)
  rrds_dt_plot <- copy(rrds_dt)
  rds_dt_plot$Method <- "RDS"
  rrds_dt_plot$Method <- "RRDS"

  combined_dt <- rbind(rds_dt_plot, rrds_dt_plot)

  # Calculate final sample sizes for annotation
  final_rds <- max(rds_dt$Participants_cumulative)
  final_rrds <- max(rrds_dt$Participants_cumulative)
  pct_diff <- round((final_rrds - final_rds) / final_rds * 100)

  max_wave <- max(combined_dt$Wave)

  p <- ggplot(combined_dt, aes(x = Wave, y = Participants_cumulative, color = Method)) +
    geom_line(linewidth = 1.4) +
    geom_point(size = 4) +
    scale_color_manual(values = c("RDS" = "#E74C3C", "RRDS" = "#3498DB")) +
    scale_x_continuous(breaks = seq(0, max_wave, by = 2), limits = c(0, max_wave)) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = "Cumulative Sample Size",
      subtitle = paste0("RRDS yields ", pct_diff, "% more participants than RDS"),
      x = "Wave",
      y = "Cumulative Participants (n)",
      color = "Method"
    ) +
    theme_minimal(base_size = 18) +
    theme(
      plot.title = element_text(face = "bold", size = 22),
      plot.subtitle = element_text(color = "gray40", size = 16),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 16),
      legend.text = element_text(size = 16),
      legend.title = element_blank(),
      legend.position = "inside",
      legend.position.inside = c(0.15, 0.85),
      legend.background = element_rect(fill = "white", color = NA),
      panel.grid.minor = element_blank()
    )

  print(p)
  invisible(p)
}

# Two-panel figure: Age convergence + Sample size
plot_two_panel <- function(rds_dt, rrds_dt, pop_mean = 41.5) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package 'patchwork' is required. Install with: install.packages('patchwork')")
  }

  library(patchwork)

  rds_dt_plot <- copy(rds_dt)
  rrds_dt_plot <- copy(rrds_dt)
  rds_dt_plot$Method <- "RDS"
  rrds_dt_plot$Method <- "RRDS"

  combined_dt <- rbind(rds_dt_plot, rrds_dt_plot)
  max_wave <- max(combined_dt$Wave)

  # Panel A: Age convergence
  p1 <- ggplot(combined_dt, aes(x = Wave, y = Mean_age_cumulative, color = Method)) +
    geom_line(linewidth = 1.4) +
    geom_point(size = 3.5) +
    geom_hline(yintercept = pop_mean, linetype = "dashed", color = "gray40", linewidth = 1) +
    annotate("text", x = 0.5, y = pop_mean + 1.5,
             label = paste0("Population (", pop_mean, ")"),
             hjust = 0, color = "gray40", size = 5) +
    scale_color_manual(values = c("RDS" = "#E74C3C", "RRDS" = "#3498DB")) +
    scale_y_continuous(limits = c(20, 50)) +
    scale_x_continuous(breaks = seq(0, max_wave, by = 2), limits = c(0, max_wave)) +
    labs(
      title = "A. Convergence to Population Mean Age",
      x = "Wave",
      y = "Cumulative Mean Age (years)"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      plot.title = element_text(face = "bold", size = 18),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14),
      legend.text = element_text(size = 14),
      legend.title = element_blank(),
      legend.position = "inside",
      legend.position.inside = c(0.8, 0.25),
      legend.background = element_rect(fill = "white", color = NA),
      panel.grid.minor = element_blank()
    )

  # Panel B: Sample size
  final_rds <- max(rds_dt$Participants_cumulative)
  final_rrds <- max(rrds_dt$Participants_cumulative)
  pct_diff <- round((final_rrds - final_rds) / final_rds * 100)

  p2 <- ggplot(combined_dt, aes(x = Wave, y = Participants_cumulative, color = Method)) +
    geom_line(linewidth = 1.4) +
    geom_point(size = 3.5) +
    scale_color_manual(values = c("RDS" = "#E74C3C", "RRDS" = "#3498DB")) +
    scale_x_continuous(breaks = seq(0, max_wave, by = 2), limits = c(0, max_wave)) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = paste0("B. Cumulative Sample Size (+", pct_diff, "% for RRDS)"),
      x = "Wave",
      y = "Cumulative Participants (n)"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      plot.title = element_text(face = "bold", size = 18),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14),
      legend.position = "none",
      panel.grid.minor = element_blank()
    )

  # Combine
  combined <- p1 + p2 +
    plot_layout(ncol = 2) +
    plot_annotation(
      title = "RDS vs RRDS: Bias Correction and Sample Efficiency",
      subtitle = "Starting from biased seeds (mostly young men)",
      theme = theme(
        plot.title = element_text(face = "bold", size = 20),
        plot.subtitle = element_text(color = "gray40", size = 16)
      )
    )

  print(combined)
  invisible(combined)
}

# Three-panel figure: Age + Female + Sample size
plot_three_panel <- function(rds_dt, rrds_dt, pop_mean = 41.5, pop_female = 0.70) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package 'patchwork' is required. Install with: install.packages('patchwork')")
  }

  library(patchwork)

  rds_dt_plot <- copy(rds_dt)
  rrds_dt_plot <- copy(rrds_dt)
  rds_dt_plot$Method <- "RDS"
  rrds_dt_plot$Method <- "RRDS"

  combined_dt <- rbind(rds_dt_plot, rrds_dt_plot)
  max_wave <- max(combined_dt$Wave)

  # Panel A: Age convergence
  p1 <- ggplot(combined_dt, aes(x = Wave, y = Mean_age_cumulative, color = Method)) +
    geom_line(linewidth = 1.4) +
    geom_point(size = 3) +
    geom_hline(yintercept = pop_mean, linetype = "dashed", color = "gray40", linewidth = 1) +
    scale_color_manual(values = c("RDS" = "#E74C3C", "RRDS" = "#3498DB")) +
    scale_y_continuous(limits = c(20, 50)) +
    scale_x_continuous(breaks = seq(0, max_wave, by = 2), limits = c(0, max_wave)) +
    labs(title = "A. Mean Age", x = NULL, y = "Years") +
    theme_minimal(base_size = 16) +
    theme(plot.title = element_text(face = "bold", size = 18),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          legend.text = element_text(size = 14),
          legend.title = element_blank(),
          legend.position = "inside",
          legend.position.inside = c(0.75, 0.25),
          legend.background = element_rect(fill = "white", color = NA))

  # Panel B: Female proportion convergence
  p2 <- ggplot(combined_dt, aes(x = Wave, y = Female_cumulative * 100, color = Method)) +
    geom_line(linewidth = 1.4) +
    geom_point(size = 3) +
    geom_hline(yintercept = pop_female * 100, linetype = "dashed", color = "gray40", linewidth = 1) +
    scale_color_manual(values = c("RDS" = "#E74C3C", "RRDS" = "#3498DB")) +
    scale_y_continuous(limits = c(20, 80)) +
    scale_x_continuous(breaks = seq(0, max_wave, by = 2), limits = c(0, max_wave)) +
    labs(title = "B. % Female", x = NULL, y = "Percent") +
    theme_minimal(base_size = 16) +
    theme(plot.title = element_text(face = "bold", size = 18),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          legend.position = "none")

  # Panel C: Sample size
  p3 <- ggplot(combined_dt, aes(x = Wave, y = Participants_cumulative, color = Method)) +
    geom_line(linewidth = 1.4) +
    geom_point(size = 3) +
    scale_color_manual(values = c("RDS" = "#E74C3C", "RRDS" = "#3498DB")) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(breaks = seq(0, max_wave, by = 2), limits = c(0, max_wave)) +
    labs(title = "C. Sample Size", x = NULL, y = "n") +
    theme_minimal(base_size = 16) +
    theme(plot.title = element_text(face = "bold", size = 18),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          legend.position = "none")

  # Combine
  combined <- p1 + p2 + p3 +
    plot_layout(ncol = 3) +
    plot_annotation(
      # title = "RDS vs RRDS Comparison",
      # subtitle = "Cumulative statistics by recruitment wave (dashed lines = population truth)",
      caption = "Seeds: mostly young men | Population: mean age 41.5, 70% female",
      theme = theme(
        plot.title = element_text(face = "bold", size = 20),
        plot.subtitle = element_text(color = "gray40", size = 16),
        plot.caption = element_text(color = "gray50", size = 14)
      )
    )

  print(combined)
  invisible(combined)
}

# --- UTILITY FUNCTIONS ---

# Calculate global max participants across datasets for consistent scaling
calc_global_max <- function(...) {
  dts <- list(...)
  max_vals <- sapply(dts, function(dt) {
    max(
      max(dt$Participants, na.rm = TRUE),
      max(dt$Participants_cumulative, na.rm = TRUE)
    )
  })
  max(max_vals)
}

# Save plot to PDF
save_plot <- function(plot, filename, width = 10, height = 6, dpi = 300) {
  ggsave(filename, plot = plot, width = width, height = height, dpi = dpi, device = "pdf")
}
