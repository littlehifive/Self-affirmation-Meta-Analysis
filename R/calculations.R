# calculation script

#  dat <- data.frame(study = NA, author = NA, year = NA, adapted = NA, type = NA,  outcome = NA, adjusted = NA, es = NA, v = NA, lowerCI = NA, upperCI = NA)


# 1. Baker 2019 --------------------------------------------------------------

Baker2019 <- function(){
  
  rbind(
    
    c("Baker", "2019", "No", "Interaction-Black", "GPA", "No", 
      getB(esc_B(0.315+0.003, 1.27, 564, 551,es.type = "g"))),
    
    c("Baker", "2019", "No", "Interaction-Hispanic", "GPA", "No", 
      getB(esc_B(-0.02+0.003, 1.27, 564, 551,es.type = "g"))),    
    
    c("Baker", "2019", "No", "Interaction-Asian", "GPA", "No", 
      getB(esc_B(0.167+0.003, 1.27, 564, 551,es.type = "g"))),   
    
    c("Baker", "2019", "No", "Interaction-Female", "GPA", "No", 
      getB(esc_B(-0.114+0.003, 1.27, 564, 551,es.type = "g"))),   
    
    c("Baker", "2019", "No", "Interaction-White", "GPA", "No", 
      getB(esc_B(0.003, 1.27, 564, 551,es.type = "g"))), 
    
    c("Baker", "2019", "No", "Interaction-White", "GPA", "No", 
      getB(esc_B(0.003, 1.27, 564, 551,es.type = "g"))),     
  
    
    c("Baker", "2019", "No", "Main", "GPA", "No", 
      getB(esc_mean_sd(2.30, 1.30, 564, 2.32, 1.23, 541, es.type = "g"))),
    
    c("Baker", "2019", "No", "Main", "Amount of time until withdraw from class", "No", 
      getB(esc_mean_sd(67.32, 24.98, 564, 65.23, 24.57, 541, es.type = "g")))

    )
  
  
}


# 2. Bancroft 2017 --------------------------------------------------------------

Bancroft2017 <- function(){
  
  c("Bancroft", "2017", "No", rep(NA,7))
  
}

# 3. Bayly 2017 --------------------------------------------------------------

Bayly2017 <- function(){
  
  rbind(
    
  # VA+CA vs. control
  c("Bayly", "2017", "Yes", "Minority subgroup-URM or First generation", "GPA", "No", 
    getB(esc_mean_sd(2.72, 0.95, 191, 2.88, 0.84, 197, es.type = "g"))),
  
  # VA vs. control
  c("Bayly", "2017", "No", "Minority subgroup-URM or First generation", "GPA", "No", 
    getB(esc_mean_sd(2.8, 0.94, 198, 2.88, 0.84, 197, es.type = "g"))),
  
  # URM (adjusted)
  c("Bayly", "2017", "No", "Minority subgroup-URM", "GPA", "Yes", 
    getB(esc_B(-0.05, sqrt(((198-1) * 0.94^2 + (197-1) * 0.84^2) /(198 + 197 - 2)), 198 * (1-0.084-0.176) , 197 * (1-0.084-0.176), es.type = "g"))),
  
  # First generation (adjusted)
  c("Bayly", "2017", "No", "Minority subgroup-First generation", "GPA", "Yes", 
    getB(esc_B(-0.02, sqrt(((198-1) * 0.94^2 + (197-1) * 0.84^2) /(198 + 197 - 2)), 198 * 0.56 ,  197 * 0.56, es.type = "g")))
  
  )
}


# 4. Binning under review --------------------------------------------------------------

BinningUR <- function(){
  
  rbind(
    
    # use longitudinal model coefficients as adjusted coefficients
    c("Binning", "under review", "No", "Minority subgroup-URM", "GPA", "Yes", 
      getB(esc_B(0.77-0.43, sqrt((0.86^2 + 0.98^2 + 0.99^2)/3), 163/2, 163/2, es.type = "g")))

  )
}
