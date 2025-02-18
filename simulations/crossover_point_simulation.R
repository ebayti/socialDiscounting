# Set a seed for reproducibility
set.seed(1)  

# Parameters
n_subject <- 15                        # Number of subjects
s <- 0.052                            # Degree of discounting
generous_amount <- 75                 # Generous amount (offer_b)
social_distances <- c(1, 2, 3, 5, 10, 20, 50, 100)  # Social distances
selfish_amounts <- seq(155, 75, by = -10)  # Selfish amounts (offer_a)
n_trials <- length(social_distances) * length(selfish_amounts)  # Total trials

# Have different amount foregone for each social distance
# Generate a single V value for the subject and repeat it for each social distance
V <- rep(rnorm(1, mean = 83, sd = 5), each = length(social_distances))

# Initialize a data frame to store simulation results (deterministic version)
simulation_results <- data.frame(
  "trial" = integer(n_trials),
  "distance" = integer(n_trials),
  "selfish_amount" = numeric(n_trials),
  "generous_amount" = numeric(n_trials),
  "stringsAsFactors" = FALSE
)

# Deterministic simulation 
# The model is: v = V / (1 + s * N)
simulate_choices <- function(N, V, s, offers, generous_amount,b = 0.1) {
  # Compute discounted value
  v_sim <- generous_amount + V / (1 + s * N)
  #v_yes <- 1 / (1 + exp(b * (generous_amount - v_sim)))  # Probability of choosing "A"
  #choices <- ifelse(rbinom(n = 1, size = 1, prob = v_yes), "A", "B")
  # For each offered amount, if offer >= v_sim choose "A"; if offer < v_sim choose "B"
  choices <- ifelse(offers >= v_sim, "A", "B")
  return(choices)
}

# Fill the simulation_results data frame with deterministic choices
trial_idx <- 1
for (i in seq_along(social_distances)) {
  d <- social_distances[i]
  # Get the deterministic choices for this social distance (for all selfish_amounts)
  choices <- simulate_choices(d, V[i], s, selfish_amounts, generous_amount,b = 0.1)
  for (j in seq_along(selfish_amounts)) {
    simulation_results$trial[trial_idx] <- trial_idx
    simulation_results$distance[trial_idx] <- d
    simulation_results$selfish_amount[trial_idx] <- selfish_amounts[j]
    simulation_results$generous_amount[trial_idx] <- generous_amount
    simulation_results$choice[trial_idx] <- choices[j]
    trial_idx <- trial_idx + 1
  }
}