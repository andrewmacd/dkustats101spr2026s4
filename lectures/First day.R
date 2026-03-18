library(ggplot2)

# Approximate data based on the graph
year <- seq(1964, 2010, by = 2)

tuition <- c(40000,42000,43000,45000,46000,44000,43000,42000,40000,42000,
             48000,52000,55000,58000,60000,63000,65000,68000,70000,72000,
             75000,78000,80000,82000)

earnings <- c(38000,40000,42000,45000,44000,43000,42000,41000,39000,40000,
              42000,45000,44000,45000,44000,45000,45000,46000,50000,51000,
              50000,48000,49000,46000)

data <- data.frame(
  year = rep(year, 2),
  value = c(tuition, earnings),
  type = rep(c("Tuition", "Earnings"), each = length(year))
)

# Fixed graph
ggplot(data, aes(x = year, y = value, color = type)) +
  geom_line(size = 1.2) +
  scale_y_continuous(
    limits = c(0, 100000),
    labels = scales::dollar
  ) +
  labs(
    title = "Costs vs Earnings of a 4-Year Degree (Corrected Scale)",
    x = "Year",
    y = "Amount (USD)",
    color = ""
  ) +
  theme_minimal()




library(ggplot2)
library(dplyr)
library(scales)

# Data
data <- data.frame(
  model = c("Verna", "Swift Dzire", "SX4", "City", "Vento", "Rapid", "Scala"),
  sales = c(56486, 26545, 10069, 12804, 25822, 22182, 3712)
)

# Compute percentages
data <- data %>%
  mutate(
    percent = sales / sum(sales),
    label = percent(percent, accuracy = 0.1)
  )

# Order bars (largest on top)
data <- data %>%
  arrange(sales) %>%
  mutate(model = factor(model, levels = model))

# Plot with different colors
ggplot(data, aes(x = sales, y = model, fill = model)) +
  geom_col() +
  geom_text(
    aes(label = label),
    hjust = -0.1,
    size = 4
  ) +
  scale_x_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title = "Car Sales by Model (Corrected Visualization)",
    x = "Sales (Units)",
    y = "Model"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


library(ggplot2)

# Approximate data reconstructed from the graph
data <- data.frame(
  year = 1990:2012,
  deaths = c(
    873, 800, 790, 800, 750, 700, 680, 650, 620, 580,
    450, 480, 500, 500, 550, 580, 550, 520,
    750, 820, 780, 700, 680
  )
)

# Corrected plot
ggplot(data, aes(x = year, y = deaths)) +
  geom_line(size = 1.2, color = "black") +
  geom_point(size = 2) +
  scale_y_continuous(
    limits = c(0, 1000),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_x_continuous(
    breaks = seq(1990, 2012, by = 2)
  ) +
  labs(
    title = "Gun Deaths in Florida (Corrected Visualization)",
    x = "Year",
    y = "Number of Deaths"
  ) +
  theme_minimal()



library(ggplot2)
library(dplyr)
library(scales)

# Data with estimated totals and proportions from chart
data <- data.frame(
  year = rep(2008:2015, each = 3),
  group = rep(c("Mobile", "Laptop", "Other"), times = 8),
  percent = c(
    # 2008–2015 (approx from chart)
    0.10, 0.80, 0.10,
    0.10, 0.80, 0.10,
    0.12, 0.75, 0.13,
    0.20, 0.70, 0.10,
    0.35, 0.60, 0.05,
    0.45, 0.50, 0.05,
    0.50, 0.45, 0.05,
    0.50, 0.45, 0.05
  )
)

# Total time per year (key correction)
totals <- data.frame(
  year = 2008:2015,
  total_minutes = c(180, 185, 190, 210, 230, 250, 270, 290)
)

# Merge + compute absolute values
data <- data %>%
  left_join(totals, by = "year") %>%
  mutate(
    minutes = percent * total_minutes,
    label = percent(percent, accuracy = 1)
  )

# Plot
ggplot(data, aes(x = factor(year), y = minutes, fill = group)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = label),
    position = position_dodge(width = 0.8),
    vjust = -0.3,
    size = 3.5
  ) +
  labs(
    title = "Time Spent per Day by Device (Corrected)",
    x = "Year",
    y = "Minutes per Day",
    fill = "Device"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal()



library(ggplot2)

# Data
data <- data.frame(
  period = c("Now", "1-Jan-13"),
  tax_rate = c(35, 40)
)

# Plot with different colors
ggplot(data, aes(x = period, y = tax_rate, fill = period)) +
  geom_col(width = 0.6) +
  scale_y_continuous(
    limits = c(0, 45),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_manual(values = c("Now" = "steelblue", "1-Jan-13" = "tomato")) +
  labs(
    title = "Top Tax Rate (Corrected Scale)",
    x = "",
    y = "Top Tax Rate (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")