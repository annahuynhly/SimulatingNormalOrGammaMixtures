
# This is a simple R package to simulate finite mixtures of normal distributions
# and/or finite mixtures of gamma distributions.

#####################################################
# MAIN FUNCTIONS (For the user)                     #
#####################################################

simulate_normal_mixture = function(n_count, theta, mean_exact, sd_exact) {
  # The purpose of this function is to simulate a finite mixture of purely normal distributions.
  # Pre-conditions:
  # 1. length(theta) = length(mean_exact) = length(sd_exact)
  # 2. n_count >= 1
  # 3. sum(theta) = 1
  if ((length(theta) == length(mean_exact))
      & (length(mean_exact) == length(sd_exact))
      & n_count >= 1 & sum(theta) == 1) {
    x = numeric(n_count)
    for (i in 1:n_count) {
      u=runif(1)
      x[i] = check_u_value_norm(u, 0, length(theta), theta, mean_exact, sd_exact)
    }
    return(x)
  }
  else if (sum(theta) != 1) {
    print("You need the sum of the theta vector to be equal to 1. Example: theta = c(0.2, 0.3, 0.5)")
  }
  else if (!((length(theta) == length(mean_exact)) & (length(mean_exact) == length(sd_exact)))) {
    print("You need theta, mean_exact, and sd_exact to be the same length.")
  }
  else if (n_count < 1) {
    print("You need n to be greater than 0.")
  }
}

simulate_gamma_mixture = function(n_count, theta, shapes, scales) {
  # The purpose of this function is to simulate a finite mixture of purely gamma distributions.
  # Pre-conditions:
  # 1. length(theta) = length(shapes) = length(scales)
  # 2. n_count >= 1
  # 3. sum(theta) = 1
  if ((length(theta) == length(shapes))
      & (length(shapes) == length(scales))
      & n_count >= 1 & sum(theta) == 1) {
    x = numeric(n_count)
    for (i in 1:n_count) {
      u=runif(1)
      x[i] = check_u_value_gamma(u, 0, length(theta), theta, shapes, scales)
    }
    return(x)
  }
  else if (sum(theta) != 1) {
    print("You need the sum of the theta vector to be equal to 1. Example: theta = c(0.2, 0.3, 0.5)")
  }
  else if (!((length(theta) == length(shapes)) & (length(shapes) == length(scales)))) {
    print("You need theta, shapes, and scales to be of the same length.")
  }
  else if (n_count < 1) {
    print("You need n to be greater than 0.")
  }
}


#####################################################
# HELPER FUNCTIONS                                  #
#####################################################

check_u_value_norm = function(u, iteration, max_iteration, theta, mean_exact, sd_exact) {
  # Checks whether u is between a certain sum of thetas. This function calls itself
  # recursively until it reaches the maximum iteration for the normal distribution.
  next_iteration = iteration + 1
  if (iteration == 0) {
    if (0<=u & u<theta[1]) {
      return(rnorm(1, mean=mean_exact[1], sd=sd_exact[1]))
    } else {
      return(check_u_value_norm(u, 1, length(theta), theta, mean_exact, sd_exact))
    }
  }
  else if (iteration == max_iteration) {
    return(rnorm(1, mean=mean_exact[max_iteration], sd=sd_exact[max_iteration]))
  }
  else if (sum(theta[1:iteration]) <= u & u < sum(theta[1:next_iteration])) {
    return(rnorm(1, mean=mean_exact[next_iteration], sd=sd_exact[next_iteration]))
  }
  else {
    return(check_u_value_norm(u, next_iteration, max_iteration, theta, mean_exact, sd_exact))
  }
}

check_u_value_gamma = function(u, iteration, max_iteration, theta, shapes, scales) {
  # Checks whether u is between a certain sum of thetas. This function calls itself
  # recursively until it reaches the maximum iteration for the gamma distribution.
  next_iteration = iteration + 1
  if (iteration == 0) {
    if (0<=u & u<theta[1]) {
      return(rgamma(1, shape = shapes[1], scale=scales[1]))
    } else {
      return(check_u_value_gamma(u, 1, length(theta), theta, shapes, scales))
    }
  }
  else if (iteration == max_iteration) {
    return(rgamma(1, shape=shapes[max_iteration], scale=scales[max_iteration]))
  }
  else if (sum(theta[1:iteration]) <= u & u < sum(theta[1:next_iteration])) {
    return(rgamma(1, shape=shapes[next_iteration], scale=scales[next_iteration]))
  }
  else {
    return(check_u_value_gamma(u, next_iteration, max_iteration, theta, shapes, scales))
  }
}
