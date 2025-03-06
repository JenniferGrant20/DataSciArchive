source("incidence.R")
library(dplyr)

# Load data tables
REG = read.csv("healthcare_registration_table.csv")
CD = read.csv("diagnostic_codes_table.csv")
PC = read.csv("primary_care_table.csv")
ED = read.csv("emergency_department_table.csv")
HA = read.csv("hospital_admissions_table.csv")

incidence(year=2020, diag='F42', washout=365, REG=REG, PC=PC, ED=ED, HA=HA)
