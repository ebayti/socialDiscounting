
#Import libs
library(tidyverse)
library(ggplot2)
library(brms)
library(future)
library(runjags)


# Jones and Rachlin (2006) model of social discounting
# v = V / (1 + sN)
# where:
# - v is the amount forgone (generosity) for a person at social distance N.
# - V is the undiscounted value (generosity at N=0).  free parameter
# - s is the discounting rate (how quickly generosity decreases with social distance). free parameter
# - N is the social distance (from 1 to 100).

# ----------------- Simulate Social Discounting Data ----------------- #
# Simulate for one person as Maike did in her script
set.seed(34)  # Set seed for reproducibility

n_subject <- 1                  # Number of subjects
social_distances <- c(0, 1, 2, 3, 5, 10, 20, 50, 100) #only simulate for these distances
selfish_amounts <- seq(155, 75, by = -10)  # Selfish amounts i.e. offer_a (V)
generous_amount <- 75                    # Fixed generous amount (both receive 75)
n_trials <- length(social_distances) * length(selfish_amounts)  # 8 distances x 9 amounts
#social_distances <- c(1:100)  # Social distances (N) 

s <- runif(n_subject, 0, 0.05) # Social discount rate for one subject (prior suggests 0.052)
individual_V <- c((rep(rnorm(n_subject, mean = 83, sd = 5),length(social_distances))))# undiscounted value (generosity at N=0) for each subject - for whole trials
fixed_V <- c((rep(83,length(social_distances)))) # undiscounted value (generosity at N=0) from Jones and Rachlin 2006 paper

v <- rep(0, length(social_distances))  # Amount forgone (generosity) for each social distance
noise <- rnorm(length(social_distances), mean = 0, sd = 1) # Generate noise
for (i in 1:length(social_distances)) {
  v[i] <- individual_V[i]/(1 + s*social_distances[i]) + noise[i] # Add noise to v[i]
}

# Adjust plot margins for better visibility of labels
#mar = c(bottom, left, top, right)
#mgp = c(bottom, left, top)
par(mar =  c(7, 8, 6, 2), mgp = c(5, 2, 0))  # Increase left margin to make space for y-axis label

title_text <- paste(
  "Participant #1, s =", round(s, 2), "\n",  # s value with a line break
  "Social premium =", round(individual_V[1], 2)  # individual_V value with a line break
)

plot(social_distances, v,
     type = "b",  # "b" for both points and lines
     pch = 15,      # Use filled circles (adjust as needed)
     cex = 1,       # Make points thicker (adjust as needed)
     lwd = 2,      # Make lines thicker
     xlab = "Social Distance", 
     ylab = "Amount forgone (generosity)",
     main = title_text, # Title with s value
     cex.axis = 3.5,   # Increase axis text size
     cex.lab = 3.5,    # Increase label size
     cex.main = 3,   # Increase title size
)

# Custom y-axis with larger tick labels and ticks
axis(2, cex.axis = 2, tck = 0.1, las = 15) 
# Add connecting lines
lines(social_distances, v, lwd=2)

# ----------------- Simulate Social Discounting like Wu ----------------- #
# Parameters from literature Jones & Rachlin (2006) and Wu paper
n_subject <- 15            # Number of subjects
social_distances <- c(1, 2, 3, 5, 10, 20, 50, 100) #only simulate for these distances
selfish_amounts <- seq(155, 75, by = -10)  # Selfish amounts i.e. offer_a (V)
n_trials <- length(social_distances) * length(selfish_amounts)  # 8 distances x 9 amounts

generous_amount <- 75                    # Fixed generous amount (both receive 75)
s <- runif(n_subject, 0, 0.5) # Social discount rate for each subject (prior suggests 0.052)
individual_V <- c((rep(rnorm(n_subject, mean = 83, sd = 5),length(n_subject))))# undiscounted value (generosity at N=0) for each subject
fixed_V <- 83 # undiscounted value (generosity at N=0) from Jones and Rachlin 2006 paper


# ----------------- Create the DataFrame ----------------- #
df <- data.frame(
    "subID" = sort(rep(1:n_subject, n_trials)), # each participant plays 72 times
    "distance" = rep(rep(social_distances, each = length(selfish_amounts)), n_subject),
    "selfish" = rep(rep(selfish_amounts, times = length(social_distances)), n_subject),
    "generous" = rep(rep(generous_amount, times = length(social_distances)), n_subject),
    "s" = rep(s, each = n_trials),
    "V" = rep(individual_V, each = n_trials)  # Each subject keeps their V
    )

# get df length
df_length <- nrow(df)

#Add noise to the data
df$noise <- NA
# Creare empt column for v
df$v <- NA

for (i in 1:df_length) {
  df$v[i] <- pmax(df$V[i]/(1 + df$s[i]*df$distance[i]) + rnorm(1, mean = 0, sd = 3)) #siugfdhoaupsjvdbguadhsosdi (noise)
}

# ----------------- Plot the Data ----------------- #
title_text <- paste(
  "Number of participants =", n_subject, "\n",  # s value with a line break
  "Mean social premium =", round(individual_V[1], 2)  # individual_V value with a line break
)


ggplot(df, aes(x = distance, y = v, color = as.factor(subID), group = subID)) +
  geom_jitter(width = 0.2, size = 2, shape = 15) +  # Add jitter and adjust point size and shape (filled squares)
  geom_line(size = 1) +  # Adjust line thickness
  labs(
    x = "Social Distance", 
    y = "Amount forgone (generosity)", 
    color = "Subject ID",
    title = title_text  # Dynamic title
  ) +
  theme_bw() + 
  theme(
    axis.text = element_text(size = 20),      # Increase axis text size
    axis.title = element_text(size = 24),     # Increase axis title size
    plot.title = element_text(size = 26, hjust = 0.5),     # Increase title size
    legend.title = element_text(size = 20),   # Adjust legend title size
    legend.text = element_text(size = 18)     # Adjust legend text size
  )


# ----------------- Plot the mean of each participant ----------------- #
# Summarize the data
df_summary <- df %>%
  group_by(subID, distance) %>%
  summarize(mean_v = mean(v, na.rm = TRUE), .groups = "drop")

ggplot(df_summary, aes(x = distance, y = mean_v, color = as.factor(subID), group = subID)) +
  geom_point(size = 3, shape = 16) +  # Add points for the mean values
  geom_line(size = 1) +  # Connect the mean points with lines
  labs(
    x = "Social Distance", 
    y = "Mean Amount Forgone (Generosity)", 
    color = "Subject ID",
    title = "Generosity by Social Distance"
  ) +
  theme_bw() + 
  theme(
    axis.text = element_text(size = 30),       # Increase axis tick label size
    axis.title = element_text(size = 25, face = "bold"),  # Increase axis title size and make bold
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),  # Increase title size and make bold
    legend.title = element_text(size = 25),   # Increase legend title size
    legend.text = element_text(size = 22)     # Increase legend text size
  )
# ----------------- OVERALL PLOT ----------------- #
errors <- tapply(df$v, df$distance, sd)  # Check error?
v_means <- tapply(df$v, df$distance, mean)  # Check mean?
mean_s <- mean(df$s)  # Check mean s?

# Title for the plot
title_text <- paste("Hyperbolic Discounting\n",
  "(mean s =", round(mean_s, 2), ")\n",  # s value with a line break # Average generosity value with a line break
)

# Create the plot
plot(social_distances, v_means,  # Plot the means, not v directly
     type = "b",  # "b" for both points and lines
     pch = 15,      # Filled squares for points (adjust as needed)
     cex = 1.5,     # Increase point size
     lwd = 2,       # Increase line width
     xlab = "Social Distance", 
     ylab = "Generosity (Amount Forgone)",
     main = title_text, # Use the formatted title
     cex.axis = 2,   # Larger axis text size
     cex.lab = 2,    # Larger label size
     cex.main = 2,   # Larger title size
     ylim = c(min(v_means - errors), max(v_means + errors)))  # Y-limits for error bars


# Add error bars
arrows(social_distances, v_means - errors, social_distances, v_means + errors, 
       length = 0.05, angle = 90, code = 3) # code=3 for both directions

# Add connecting lines
lines(social_distances, v_means, lwd=1.2)

# Improve aesthetics (optional)
grid(col = "lightgray", lty = "dotted")  # Add a grid

axis(2, cex.axis = 2, tck = 0.1, las = 15) # Custom y-axis with larger tick labels and ticks

# ----------------- Fit the Model ----------------- #
data_list <- list(v = df$v,
                  N = df$distance,
                  V = df$V,
                  df_length = df_length,
                  n_subject = length(unique(df$subID)),
                  sub_id = as.vector(df$subID)
)
dat <- dump.format(data_list)

# Initialize the variables and chains
inits1 <- dump.format(list(.RNG.name="base::Super-Duper", .RNG.seed=99999 ))
inits2 <- dump.format(list(.RNG.name="base::Wichmann-Hill", .RNG.seed=1234 ))
inits3 <- dump.format(list(.RNG.name="base::Mersenne-Twister", .RNG.seed=6666 ))

monitor = c("s")

model_file <- "simulations/models/hyperbolic_discounting.txt"
# Run the function that fits the models using JAGS
fit <- run.jags(model=model_file, monitor=monitor,
                data=dat, n.chains=3, inits=c(inits1, inits2, inits3), plots = TRUE, burnin=10000, 
                sample=1000, adapt = 1000, thin=25)

summary <- summary(fit)
plot(unique(df$s), summary[ , 4])