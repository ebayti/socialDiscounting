# Social and Temporal Discounting and Ambience Effects

This repository contains the code and documentation for my first PhD project, which explores the phenomenon of Social Discounting. Jones & Rachlin (2006) demonstrated that individuals' willingness to forgo personal rewards in favor of benefiting others decreases hyperbolically with increasing social distance. In the current project, we will raise two questions:
1- Is there a relationship between social discounting and temporal discounting?
2- Do ambient temperature differences influence social discounting behavior?


## Repository Structure

```
socialDiscounting/
├── simulations/           # Contains code and documentation for power analysis
├── experiment/            # Contains code for the online experiment
└── README.md             # This file
```

# Simulations

## Define the Hyperbolic Social Discounting Model
The model from Jones & Rachlin (2006):

$$
v = \frac{V}{1 + sN}
$$

**Where:**
- v: Amount forgone (generosity) for a person at social distance \( N \)
- V: Baseline generosity (intercept at \( N = 0 \))
- s: Social discounting rate (how generosity decreases with \( N \))
- N: Social distance (1, 2, 3, 5, 10, 20, 50, 100)


## Simulation of Social Discounting Data
Set priors based on Jones & Rachlin (2006):

### Parameters
**Social Premium -  Amount foregone (V):**
Assign individual baseline generosity values $V \sim \text{Normal}(83,\, 5)$
**Social Discounting Rate (s):**
Randomly sample s : $s \sim \text{Normal}(0.05,\, 0.02)$

### Notes
- Social distances (N) are discrete values from the original study (Jones & Rachlin, 2006)
- Priors are based on the empirical estimates from previous literature

## References
Jones, B., & Rachlin, H. (2006). Social Discounting. Psychological Science, 17(4), 283-286.
