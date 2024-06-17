pacman::p_load(dplyr,ompr,ompr.roi,ROI.plugin.glpk)
# https://www.r-orms.org/mixed-integer-linear-programming/practicals/problem-tsp/
dist_fun <- function(i, j, distance) {
  # distance have to include in argument, otherwise, it can't be found
  vapply(seq_along(i), function(k) distance[i[k], j[k]], numeric(1L))
}

optim_d<- function(df){
  # calculate smallest route when considering all the x y 
  distance <- df %>%
    dplyr::select(stomata.cx,stomata.cy) %>% 
    stats::dist() %>%
    as.matrix(diag = TRUE, upper = TRUE)
  # number of rows
  n <- nrow(df)
  
  model <- MIPModel() %>%
    # we create a variable that is 1 iff we travel from point i to j
    add_variable(x[i, j], i = 1:n, j = 1:n, 
                 type = "integer", lb = 0, ub = 1) %>%
    # a helper variable for the MTZ formulation of the tsp
    add_variable(u[i], i = 1:n, lb = 1, ub = n) %>% 
    # minimize travel distance
    set_objective(sum_expr(dist_fun(i, j, distance) * x[i, j],
                           i = 1:n, j = 1:n), "min") %>%
    # you cannot go to the same point
    set_bounds(x[i, i], ub = 0, i = 1:n) %>%
    # leave each point
    add_constraint(sum_expr(x[i, j], j = 1:n) == 1, i = 1:n) %>%
    # visit each point
    add_constraint(sum_expr(x[i, j], i = 1:n) == 1, j = 1:n) %>%
    # ensure no subtours (arc constraints)
    add_constraint(u[i] >= 2, i = 2:n) %>% 
    add_constraint(u[i] - u[j] + 1 <= (n - 1) * (1 - x[i, j]), i = 2:n, j = 2:n)
  # solve model using glpk
  result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))
  # total distance
  data.frame(pic_name=df$pic_name[1],
             route=result$objective_value)
}

# -------------------------------------------------------------------------
optim_d2<- function(df){
  
dist_mat <-  df %>%
  dplyr::select(stomata.cx,stomata.cy) %>% 
  dist(method = 'euclidean')

# Initialize the TSP object
tsp_prob <- TSP(dist_mat)
# We add a dummy to the TSP, in this way we remove
# the constraint of having a path that ends at the
# starting point
tsp_prob <- insert_dummy(tsp_prob, label = 'dummy')
# TSP solver
tour <-
  solve_TSP(
    tsp_prob,
    method = 'two_opt',
    control = list(rep = 16)
  )
# Optimal path
# path <- names(cut_tour(tour, 'dummy'))

# system.time(
#   solve_TSP(
#     tsp_prob,
#     method = 'two_opt',
#     control = list(rep = 16)
#   )
# )

}

path <- names(cut_tour(a, 'dummy'))

library(magrittr)
library(ggplot2)
df %<>%
  mutate(id_order = order(as.integer(path)))
# Plot a map with the data and overlay the optimal path
df %>%
  dplyr::arrange(id_order) %>%
  ggplot(aes(x=stomata.cx,stomata.cy))+
  geom_point()+
  geom_path() 
