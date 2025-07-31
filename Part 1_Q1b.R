# Calculating the Convergence statistic (R_hat)
# Generating the chained sequences, J=4
  
set.seed(12)
numbers_0 <- rnorm(n = 2000, sd = 0.001)

set.seed(13)
numbers_1 <- rnorm(n = 2000, sd = 0.001)

set.seed(14)
numbers_2 <- rnorm(n = 2000, sd = 0.001)

set.seed(15)
numbers_3 <- rnorm(n = 2000, sd = 0.001)

# sample mean , (M_j)
M_j <- (sum(numbers_0)/2000) + (sum(numbers_1)/2000) + (sum(numbers_2)/2000) +(sum(numbers_3)/2000)

# within sample variance , (V_j)
M_0 <- mean(numbers_0)
M_1 <- mean(numbers_1)
M_2 <- mean(numbers_2)
M_3 <- mean(numbers_3)

V_j <- (sum((numbers_0 - M_0)^2)/2000) + (sum((numbers_1 - M_1)^2)/2000) + (sum((numbers_2 - M_2)^2)/2000) + (sum((numbers_3 - M_3)^2)/2000)

# overall within sample variance (W)
W <- (V_j)/4

# overall sample mean (M)
M <- (M_j)/4

# between sample variance (B)
B <- ((M_0 - M)^2 + (M_1 - M)^2 + (M_2 - M)^2 + (M_3 - M)^2)/4

# convergence statistic, (r_hat)
r_hat <- sqrt( (B + W)/W)

print(M_j)
print(V_j)
print(W)
print(M)
print(B)
print(r_hat)

# Plot 
compute_rhat <- function(s, N=2000, j=4){
  set.seed(12)
  numbers_0 <- rnorm(N, sd = s)
  
  set.seed(13)
  numbers_1 <- rnorm(N, sd = s)
  
  set.seed(14)
  numbers_2 <- rnorm(N, sd = s)
  
  set.seed(15)
  numbers_3 <- rnorm(N, sd = s)
  chains <-list(numbers_0,numbers_1,numbers_2,numbers_3)[1:j]
  M_j <- sapply(chains, mean)
  V_j <- sapply(chains, var)
  M <- mean(M_j)
  W <- mean(V_j)
  B <- mean((M_j - M)^2)
  r_hat <- sqrt( (B + W)/W)
  return(r_hat)
  }
s_values <- seq(0.001,1,length.out = 100)
rhat_values <- sapply(s_values, compute_rhat)

library(ggplot2)

plot_data <- data.frame(s = s_values, Rhat = rhat_values)

ggplot(plot_data, aes(x = s, y = Rhat)) +
  geom_line() +
  labs(x = "'s' values", y = "r_hat")
  ylim(0.999999, 1.000001)
ggsave("Part 1b.png")
