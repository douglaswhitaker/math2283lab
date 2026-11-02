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
write.csv(output_pollution11_0, file = "output_pollution11_0.csv", row.names = FALSE)
shell.exec(file.path(getwd(), "output_pollution11_0.csv"))
# Stop here to review the output in Excel.


output_pollution11_5 <- multisim_wrapper(
  context1 = "pollution11",
  increase_mean_by_vec = c(5),
  n1_vec = c(5, 10, 20, 40),
  n2_vec = c(5, 10, 20, 40),
  num_sim = 1000,
  simplify_output_table = TRUE
)
write.csv(output_pollution11_5, file = "output_pollution11_5.csv", row.names = FALSE)
shell.exec(file.path(getwd(), "output_pollution11_5.csv"))
# Stop here to review the output in Excel.


output_pollution11_10 <- multisim_wrapper(
  context1 = "pollution11",
  increase_mean_by_vec = c(10),
  n1_vec = c(5, 10, 20, 40),
  n2_vec = c(5, 10, 20, 40),
  num_sim = 1000,
  simplify_output_table = TRUE
)
write.csv(output_pollution11_10, file = "output_pollution11_10.csv", row.names = FALSE)
shell.exec(file.path(getwd(), "output_pollution11_10.csv"))
# Stop here to review the output in Excel.



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
write.csv(output_swedishinfants, file = "output_swedishinfants.csv", row.names = FALSE)
shell.exec(file.path(getwd(), "output_swedishinfants.csv"))
# Stop here to review the output in Excel.



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
write.csv(output_derriford_interrarival_AEC1, file = "output_derriford_interrarival_AEC1.csv", row.names = FALSE)
shell.exec(file.path(getwd(), "output_derriford_interrarival_AEC1.csv"))
# Stop here to review the output in Excel.



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
write.csv(output_watervelocity1, file = "output_watervelocity1.csv", row.names = FALSE)
shell.exec(file.path(getwd(), "output_watervelocity1.csv"))
# Stop here to review the output in Excel.
