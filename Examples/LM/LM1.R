
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

data <- data.frame(
  x = c(rnorm(n/5, mean = 5, sd = 1),
        rnorm(n/5, mean = 7.5, sd = 1),
        rnorm(n/5, mean = 10, sd = 1),
        rnorm(n/5, mean = 12.5, sd = 1),
        rnorm(n/5, mean = 15, sd = 1)),
  y =c(rnorm(n/5, mean = 5, sd = 1),
       rnorm(n/5, mean = 7.5, sd = 1),
       rnorm(n/5, mean = 10, sd = 1),
       rnorm(n/5, mean = 12.5, sd = 1),
       rnorm(n/5, mean = 15, sd = 1))
)

png("Examples/LM/LM1.png", units = "in", res = 300, width = 4, height = 4)
ggplot(data, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "X",
       y = "Y") +
  mytheme +
  theme(legend.position = "none", text = element_text(size = 20))  # Center the title
dev.off()
