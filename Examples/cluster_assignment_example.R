library(DeclareDesign)
library(truncnorm)
library(knitr)
library(ggplot2)
library(dplyr)

set.seed(101)
population <- declare_population(
  city = add_level(N=10,
                   city.avg = rnorm(mean = 1.2, n=N)), #creating clustering level variation
  waterway = add_level(N=14, 
                       baseline_wqi = rtruncnorm(n=N, a=0.38, b=8.21, 
                                                 mean=city.avg, sd=2), #cluster level variation informs waterway outcomes
                       annual_change_rate = rnorm(n=N, mean=1.1, sd=0.25))
)
pop <- population()

potential_outcomes <- 
  declare_potential_outcomes(
    Y_Z_0 = baseline_wqi * annual_change_rate,
    Y_Z_1 = baseline_wqi * annual_change_rate * 0.9)
po <- potential_outcomes(pop)

assigning_waterway <- declare_assignment(prob=0.5) #individual waterways assigned to treatment at prob=0.5
assigning_city <- declare_assignment(clusters=city, prob=0.5) #whole cities assigned to treatment at prob=0.5

#Check out how these two dataframe are different from each other in the "Z" column
assigned_w <- assigning_waterway(po)
assigned_c <- assigning_city(po)

revealing <- declare_reveal()

estimand <- declare_estimand(
  change = mean(Y_Z_1-Y_Z_0)) #using difference in means for simplicity
estimand(po)

answer_w <- declare_estimator(Y ~ Z, 
                           estimand = estimand,  
                           model =  lm_robust, label = "DIM")

answer_c <- declare_estimator(Y ~ Z, 
                            estimand = estimand,  
                            model =  lm_robust, 
                            clusters = city, #this command adjusts standard errors for clustering
                            label = "DIM_clustered")

#Waterway design
design_w <- population + potential_outcomes +
  assigning_waterway + revealing + estimand + answer_w

diagnosis <- diagnose_design(design_w)
diagnosis$diagnosands_df[,c(1,3,5,9,11)]

#City-cluster design
design_c <- population + potential_outcomes +
  assigning_city + revealing + estimand + answer_c

diagnosis <- diagnose_design(design_c)
diagnosis$diagnosands_df[,c(1,3,5,9,11)]