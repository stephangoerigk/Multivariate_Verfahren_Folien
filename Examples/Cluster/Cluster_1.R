
mytheme = theme_bw() + theme(panel.grid.minor = element_blank(),
                             panel.grid.major = element_blank(), panel.background = element_blank(),
                             strip.background = element_blank(), strip.text.y = element_text(),
                             legend.background = element_blank(), legend.key = element_blank(),
                             panel.border = element_rect(colour = "black", fill = NA))


# Load necessary library
library(ggplot2)

# Create a sample dataset with three clusters
set.seed(123)  # For reproducibility

n <- 150  # Total number of points

# Generate random points for 3 clusters
data <- data.frame(
  x = c(rnorm(n/3, mean = 5, sd = 1),
        rnorm(n/3, mean = 10, sd = 1),
        rnorm(n/3, mean = 15, sd = 1)),
  y = c(rnorm(n/3, mean = 5, sd = 1),
        rnorm(n/3, mean = 10, sd = 1),
        rnorm(n/3, mean = 15, sd = 1)),
  cluster = factor(rep(1:3, each = n/3))  # Assign clusters 1, 2, and 3
)

# Create a scatter plot with ggplot
png("Examples/Cluster/Cluster1.png", units = "in", res = 300, width = 4, height = 4)
ggplot(data, aes(x = x, y = y, color = cluster, fill = cluster)) +
  geom_point(size = 1) +  # Scatter plot points
  stat_ellipse(type = "norm", level = 0.95, alpha = 0.2, geom = "polygon") +  # Add ellipses (as circles)
  labs(x = "X",
       y = "Y",
       color = "Cluster", 
       fill = "Cluster") +
  mytheme +
  theme(legend.position = "none", text = element_text(size = 20))  # Center the title
dev.off()
