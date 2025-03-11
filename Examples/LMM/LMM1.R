mytheme = theme_bw() + theme(panel.grid.minor = element_blank(),
                             panel.grid.major = element_blank(), panel.background = element_blank(),
                             strip.background = element_blank(), strip.text.y = element_text(),
                             legend.background = element_blank(), legend.key = element_blank(),
                             panel.border = element_rect(colour = "black", fill = NA))
# Load necessary library
library(ggplot2)

# Create a sample dataset to mimic the original image
set.seed(123)  # For reproducibility

# Generate data for 10 clusters arranged diagonally
n_points <- 20  # Number of points per group
groups <- 7     # Number of groups

# Generate the x-values, each group along a diagonal
data <- data.frame(
  x = rep(seq(1, n_points*groups, by = 1)),  # Adjust x-values for groups
  y = c(rnorm(n_points, mean = 2, sd = 0.2) - seq(1, n_points)*0.05,  # Negative slope within each group
        rnorm(n_points, mean = 3, sd = 0.2) - seq(1, n_points)*0.05,
        rnorm(n_points, mean = 4, sd = 0.2) - seq(1, n_points)*0.05,
        rnorm(n_points, mean = 5, sd = 0.2) - seq(1, n_points)*0.05,
        rnorm(n_points, mean = 6, sd = 0.2) - seq(1, n_points)*0.05,
        rnorm(n_points, mean = 7, sd = 0.2) - seq(1, n_points)*0.05,
        rnorm(n_points, mean = 8, sd = 0.2) - seq(1, n_points)*0.05),  # Y-values for each group with negative slope
  group = rep(1:groups, each = n_points)  # Group labels
)
#data$x = data$x *  rnorm(nrow(data), mean = 1, sd = 0.01)

# Create the scatter plot with regression lines
png("Examples/LMM/LMM1.png", units = "in", res = 300, width = 4, height = 4)
ggplot(data, aes(x = x/10, y = y)) +
  geom_point(size = 1) +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, size = 0.8, colour = "black") +  # Add thin black regression lines
  geom_smooth(method = "lm", se = FALSE, aes(group = factor(group), colour = factor(group)), size = 0.8) +  # Add thin black regression lines
  labs(x = "X",
       y = "Y") +
  mytheme +
  theme(legend.position = "none", text = element_text(size = 20)) 
dev.off()
