# Set a seed for reproducibility
set.seed(1)  

# Parameters
n_subject <- 15                        # Number of subjects
s <- 0.052                            # Degree of discounting
generous_amount <- 75                 # Generous amount (offer_b)
social_distances <- c(1, 2, 3, 5, 10, 20, 50, 100)  # Social distances
selfish_amounts <- seq(155, 75, by = -10)  # Selfish amounts (offer_a)
n_trials <- length(social_distances) * length(selfish_amounts)  # Total trials

# Generate a single V value for the subject and repeat it for each social distance
amount_foregone <- rep(rnorm(n_subject, mean = 83, sd = 5), length(social_distances))

# Initialize a data frame to store simulation results (deterministic version)
simulation_results <- data.frame(
  "trial" = integer(n_trials),
  "distance" = integer(n_trials),
  "selfish_amount" = numeric(n_trials),
  "generous_amount" = numeric(n_trials),
  "b_selected" = logical(n_trials),
  "stringsAsFactors" = FALSE
)

# Deterministic simulation using a computed crossover point (CP)
# The model is: v = V / (1 + s * N)
# The crossover point (CP) is defined as:
#    CP = generous_amount + V(amount foregone)
simulate_choices <- function(N, V, s, offers, generous_amount) {
  # Compute the crossover point (CP)
  CP <- generous_amount + amount_foregone / (1 + s * N)
  # For each offered amount, if offer >= CP choose "A"; if offer < CP choose "B"
  choices <- ifelse(offers >= CP, "A", "B")
  return(choices)
}

# Fill the simulation_results data frame with deterministic choices
trial_idx <- 1
for (i in seq_along(social_distances)) {
  d <- social_distances[i]
  # Get the deterministic choices for this social distance (for all selfish_amounts)
  choices <- simulate_choices(d, V[i], s, selfish_amounts, generous_amount)
  for (j in seq_along(selfish_amounts)) {
    simulation_results$trial[trial_idx] <- trial_idx
    simulation_results$distance[trial_idx] <- d
    simulation_results$selfish_amount[trial_idx] <- selfish_amounts[j]
    simulation_results$generous_amount[trial_idx] <- generous_amount
    simulation_results$choice[trial_idx] <- choices[j]
    trial_idx <- trial_idx + 1
  }
}