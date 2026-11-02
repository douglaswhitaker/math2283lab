# MATH 2283 Lab

library("math2283lab")

# Context: pollution11 ----------------------------------------------------

set.seed(3650)

output_pollution11_0 <- multisim_wrapper(
  context1 = "pollution11",
  increase_mean_by_vec = c(0),
  n1_vec = c(5, 10, 20, 40),
  n2_vec = c(5, 10, 20, 40),
  num_sim = 1000,
  simplify_output_table = TRUE
)
output_pollution11_5 <- multisim_wrapper(
  context1 = "pollution11",
  increase_mean_by_vec = c(5),
  n1_vec = c(5, 10, 20, 40),
  n2_vec = c(5, 10, 20, 40),
  num_sim = 1000,
  simplify_output_table = TRUE
)
output_pollution11_10 <- multisim_wrapper(
  context1 = "pollution11",
  increase_mean_by_vec = c(10),
  n1_vec = c(5, 10, 20, 40),
  n2_vec = c(5, 10, 20, 40),
  num_sim = 1000,
  simplify_output_table = TRUE
)


set.seed(3650)

output_pollution11 <- multisim_wrapper(
  context1 = "pollution11",
  increase_mean_by_vec = c(0, 5, 10),
  n1_vec = c(5, 10, 20, 40),
  n2_vec = c(5, 10, 20, 40),
  num_sim = 1000,
  simplify_output_table = TRUE
)
output_pollution11
write.csv(output_pollution11, file = "output_pollution11.csv", row.names = FALSE)

set.seed(3650)

output_pollution11_10k <- multisim_wrapper(
  context1 = "pollution11",
  increase_mean_by_vec = c(0, 5, 10),
  n1_vec = c(5, 10, 20, 40),
  n2_vec = c(5, 10, 20, 40),
  num_sim = 10000,
  simplify_output_table = TRUE
)
output_pollution11_10k
write.csv(output_pollution11_10k, file = "output_pollution11_10k.csv", row.names = FALSE)


# Context: swedishinfants -------------------------------------------------

set.seed(5150)

output_swedishinfants <- multisim_wrapper(
  context1 = "swedishinfants",
  infantvar = "length", infantFM = "girls", infantGA = "38",
  increase_mean_by_vec = c(0, 1, 2.5),
  n1_vec = c(5, 10, 20, 40),
  n2_vec = c(5, 10, 20, 40),
  num_sim = 1000,
  simplify_output_table = TRUE
)
output_swedishinfants
write.csv(output_swedishinfants, file = "output_swedishinfants.csv", row.names = FALSE)


# Context: derriford_interrarival_AEC1 ------------------------------------

set.seed(8597)

output_derriford_interrarival_AEC1 <- multisim_wrapper(
  context1 = "derriford_interrarival_AEC1",
  increase_mean_by_vec = c(0, 5, 15, 30),
  n1_vec = c(5, 10, 20, 40),
  n2_vec = c(5, 10, 20, 40),
  num_sim = 1000,
  simplify_output_table = TRUE
)

output_derriford_interrarival_AEC1
write.csv(output_derriford_interrarival_AEC1, file = "output_derriford_interrarival_AEC1.csv", row.names = FALSE)


# Context: watervelocity1 -------------------------------------------------

set.seed(1248)
output_watervelocity1 <- multisim_wrapper(
  context1 = "watervelocity1",
  increase_mean_by_vec = c(0, .01, .25, .75),
  n1_vec = c(5, 10, 20, 40, 200),
  n2_vec = c(5, 10, 20, 40, 200),
  num_sim = 1000,
  simplify_output_table = TRUE
)

output_watervelocity1
write.csv(output_watervelocity1, file = "output_watervelocity1.csv", row.names = FALSE)
