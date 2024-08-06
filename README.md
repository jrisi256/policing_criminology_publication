# Replication code for Risi & Graif 2024, "Community representation and policing: Effects on Black civilians".

The Github for this code repository can be found here: https://github.com/jrisi256/policing_criminology_publication, and it will likely to be kept more up-to-date.

## Step 1 - Cleaning the data

First, the data needs to be transformed into a format suitable for analysis. The initial *seed* files can be found here: https://codeocean.com/capsule/8907164/tree/v1. They were obtained by the Ba et al. (2021) research team in conjunction with other research groups and non-profit advocacy groups who FOIA'ed the Chicago Police Department to obtain this data. The seed files you will want for the data cleaning step are:

* **officers.csv.gz**
* **assignments.csv.gz**
* **stops.csv.gz**
* **arrests.csv.gz**
* **force.csv.gz**

Using these initial seed files, one can run the data cleaning R files in sequential order (1_join_officer_assignments.R, 2_join_assignments_outcomes.R, 3_create_outcomes.R). Briefly describing what each of the files do:

* **1_join_officer_assignments.R** --> Joins officers to their work assignments.
* **2_join_assignments_outcomes.R** --> Merge the outcome variables (stops, arrests, force) to work assignments to see which outcomes occurred during which officers' work assignment.
* **3_create_outcomes.R** --> Consolidate the merged outcomes + work assignments data table so that each row corresponds to one officer + one work assignment.

## Step 2 - Creating the measures/variables

Next, we create the main measures and variables used in our study. First, we create the aggregate-level (i.e., police district) variables. Then, we create the individual-shift-level variables.

### Aggregate-level measures

The aggregate-level measures are created using **1_create_police_district_vars.R**. We provided the publicly available Census data + Census shape files. We also provide the Chicago police district boundaries (https://data.cityofchicago.org/Public-Safety/Boundaries-Police-Districts-current-/fthy-xz3r). However, one will also need to obtain the following files from https://codeocean.com/capsule/8907164/tree/v1 in order to fully run the code:

* **monthly_police_unit_membership_1965_2016.csv.gz**
* **monthly_police_unit_membership_2002_2016.csv.gz**

### Individual-level measures

One only needs to run **2_create_individual_shift_vars.R** on the output files created from the data cleaning process.

## Step 3 - Recreating the tables and figures from the paper.

This step is hopefully pretty easy to follow. Each R code file is named such that it will output its corresponding namesake table or figure.
