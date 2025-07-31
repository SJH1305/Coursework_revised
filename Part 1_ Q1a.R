# Random walk Metropolis algorithm 
N <- 10000
s <- 1
x <- numeric(N) # create vector 
x[1] <- 0 # initial value

# computing the ratio 
log_function<- function(x) {
  log(0.5) - abs(x)
}

set.seed(12)
for (i in 2:N ) {
  x_star <- rnorm(n = 1, sd = 1, mean = x[i -1]) # normal distribution generator
  r <- log_function(x_star) - log_function(x[i-1])
  u <- runif(1)
  
  if (log(u) < r) {
    x[i] <- x_star} 
  else {
    x[i] <- x[i-1]}}

# histogram with KDE 
library(ggplot2)

set.seed(12)
numbers <- rnorm(n = 10000, sd = 1, mean = 0) # note, mean set as 0!

# convert to data frame
numbers_df <- data.frame(x = x)

ggplot(numbers_df, aes(x=x)) +
  geom_histogram(aes(y = after_stat(density)), colour = 1, fill = "blue")+
  geom_density(aes(colour = "KDE")) + 
  stat_function(fun = function(x) 0.5 * exp(-abs(x)), aes(color = "f(x)"))+ 
  labs(title = "Normally distributed values")
ggsave("Part 1a_Hist,KDE,f(x).png")

# sample mean and std. 
mean <- mean(x)
standard_deviation <- sd(x)
print(mean)
print(standard_deviation)









