# MATH 2283 Lab 4 prep code

install.packages("stringr")

download.file(
  url = "https://github.com/douglaswhitaker/math2283lab/raw/main/other/math2283lab_0.1.1.0.zip",
  destfile = "math2283lab_0.1.1.0.zip",
  method = "curl",
  extra = "-L",
  mode = "wb"
)

install.packages(
  file.path(getwd(), "math2283lab_0.1.1.0.zip"), 
  repos = NULL, type = "win.binary"
  )

download.file(
  url = "https://github.com/douglaswhitaker/math2283lab/raw/main/other/lab4-simulation-code.R",
  destfile = "lab4-simulation-code.R",
  method = "curl",
  extra = "-L",
  mode = "w"
)

file.edit(file.path(getwd(), "lab4-simulation-code.R"))
