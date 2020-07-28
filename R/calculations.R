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

# 5. Borman 2012 --------------------------------------------------------------

Borman2012 <- function(){
  
  rbind(
    # Table 6 Outcome is reported in standard deviations of 2012 test score, so it is technically an effect size already
    
    # MCA Reading & Math scores
    
    # Minority subgroup
    c("Borman","2012","No","Minority subgroup-Black","MCA reading score","Yes",
      getgv(0.023, 0.049^2, 401-1)),
    c("Borman","2012","No","Minority subgroup-Black","MCA math score","Yes",
      getgv(0.073, 0.054^2, 403-1)),
    c("Borman","2012","No","Minority subgroup-Hispanic","MCA reading score","Yes",
      getgv(0.055, 0.084^2, 187-1)),
    c("Borman","2012","No","Minority subgroup-Hispanic","MCA math score","Yes",
      getgv(0.053, 0.074^2, 186-1)),
    c("Borman","2012","No","Minority subgroup-Female","MCA reading score","Yes",
      getgv(0.019, 0.039^2, 651-1)),
    c("Borman","2012","No","Minority subgroup-Female","MCA math score","Yes",
      getgv(0.104, 0.035^2, 654-1)),
    # Majority subgroup
    c("Borman","2012","No","Majority subgroup-Asian","MCA reading score","Yes",
      getgv(-0.066, 0.050^2, 341-1)),
    c("Borman","2012","No","Majority subgroup-Asian","MCA math score","Yes",
      getgv(-0.075, 0.046^2, 342-1)),
    c("Borman","2012","No","Majority subgroup-Male","MCA reading score","Yes",
      getgv(-0.022, 0.042^2, 667-1)),
    c("Borman","2012","No","Majority subgroup-Male","MCA math score","Yes",
      getgv(-0.005, 0.039^2, 668-1))
    
  )
  
}

# 6. Borman 2015 --------------------------------------------------------------

Borman2015 <- function(){
  
  c("Borman", "2015", "No", rep(NA,7))
  
}

# 7. Borman 2016 --------------------------------------------------------------
Borman2016 <- function(){
  
  rbind(
    c("Borman","2016","No","Interaction-URM","7th GPA","Yes",
      d2g(0.11, 506, 506)),
    c("Borman","2016","No","Interaction-URM","Fall Reading test","Yes",
      d2g(0, 463, 463)),
    c("Borman","2016","No","Interaction-URM","Fall Math test","Yes",
      d2g(0.09, 463, 463)),
    c("Borman","2016","No","Interaction-URM","Spring Reading test","Yes",
      d2g(0.01, 463, 463)),
    c("Borman","2016","No","Interaction-URM","Spring Math test","Yes",
      d2g(0.08, 463, 463)),
    c("Borman","2016","No","Interaction-URM","Spring Language Usage test","Yes",
      d2g(0.04, 463, 463))
  )
  
}