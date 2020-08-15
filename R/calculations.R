##################### Main calculation by article ##################### 

#  dat <- data.frame(study = NA, author = NA, year = NA, adapted = NA, 
# type = NA,  outcome = NA, adjusted = NA, es = NA, v = NA, 
# lowerCI = NA, upperCI = NA, raw_gap_control = NA, residual_gap_control = NA)

# Interaction means the numbers are extracted from main effects and interaction terms to represent marginal effect for the disadvantaged group

# 1. Baker 2019 --------------------------------------------------------------

Baker2019 <- function(){
  
  rbind(
    # SD of the other measures (persistence, grade in class, passing grade and withdraw) were not reported
    c("Baker", "2019", "No", "Interaction-Black", "GPA", "No", 
      extract_g(esc_B(0.315+0.003, 1.27, 564, 551,es.type = "g")),
      NA,NA),
    
    c("Baker", "2019", "No", "Interaction-Hispanic", "GPA", "No", 
      extract_g(esc_B(-0.02+0.003, 1.27, 564, 551,es.type = "g")),
      NA,NA),    
    
    c("Baker", "2019", "No", "Interaction-Asian", "GPA", "No", 
      extract_g(esc_B(0.167+0.003, 1.27, 564, 551,es.type = "g")),
      NA,NA),   
    
    c("Baker", "2019", "No", "Interaction-Female", "GPA", "No", 
      extract_g(esc_B(-0.114+0.003, 1.27, 564, 551,es.type = "g")),
      NA,NA),   
    
    c("Baker", "2019", "No", "Interaction-White", "GPA", "No", 
      extract_g(esc_B(0.003, 1.27, 564, 551,es.type = "g")),
      NA,NA), 
    
    c("Baker", "2019", "No", "Interaction-White", "GPA", "No", 
      extract_g(esc_B(0.003, 1.27, 564, 551,es.type = "g")),
      NA,NA)     
  
    
    #c("Baker", "2019", "No", "Main", "GPA", "No", 
    #  extract_g(esc_mean_sd(2.30, 1.30, 564, 2.32, 1.23, 541, es.type = "g")),
    #  NA,NA),
    #
    #c("Baker", "2019", "No", "Main", "Amount of time until withdraw from class", "No", 
    #  extract_g(esc_mean_sd(67.32, 24.98, 564, 65.23, 24.57, 541, es.type = "g")),
    #  NA,NA)

    )
  
  
}


# 2. Bancroft 2017 --------------------------------------------------------------

Bancroft2017 <- function(){
  
  c("Bancroft", "2017", rep(NA,10))
  
}

# 3. Bayly 2017 --------------------------------------------------------------

Bayly2017 <- function(){
  
  rbind(
    
  # VA+CA vs. control
  c("Bayly", "2017", "Yes", "Minority subgroup-URM or First generation", "GPA", "No", 
    extract_g(esc_mean_sd(2.72, 0.95, 191, 2.88, 0.84, 197, es.type = "g")),
    NA,NA),
  
  # VA vs. control
  c("Bayly", "2017", "No", "Minority subgroup-URM or First generation", "GPA", "No", 
    extract_g(esc_mean_sd(2.8, 0.94, 198, 2.88, 0.84, 197, es.type = "g")),
    NA,NA),
  
  # URM (adjusted)
  c("Bayly", "2017", "No", "Minority subgroup-URM", "GPA", "Yes", 
    extract_g(esc_B(-0.05, sqrt(((198-1) * 0.94^2 + (197-1) * 0.84^2) /(198 + 197 - 2)), 198 * (1-0.084-0.176) , 197 * (1-0.084-0.176), es.type = "g")),
    NA,NA),
  
  # First generation (adjusted)
  c("Bayly", "2017", "No", "Minority subgroup-First generation", "GPA", "Yes", 
    extract_g(esc_B(-0.02, sqrt(((198-1) * 0.94^2 + (197-1) * 0.84^2) /(198 + 197 - 2)), 198 * 0.56 ,  197 * 0.56, es.type = "g")),
    NA,NA)
  
  )
}


# 4. Binning under review --------------------------------------------------------------

BinningUR <- function(){
  
  rbind(
    
    # use longitudinal model coefficients as adjusted coefficients
    # assuming overall main effect is the same for URM students (as instructed by the original investigator)
    c("Binning", "under review", "No", "Minority subgroup-URM", "GPA", "Yes", 
      extract_g(esc_B(0.77-0.43, sqrt((0.86^2 + 0.98^2 + 0.99^2)/3), 163/2, 163/2, es.type = "g")),
      NA,NA)

  )
}

# 5. Borman 2012 --------------------------------------------------------------

Borman2012 <- function(){
  
  rbind(
    # Table 6 Outcome is reported in standard deviations of 2012 test score, so it is technically an effect size already
    
    # MCA Reading & Math scores
    
    # Minority subgroup
    c("Borman","2012","No","Minority subgroup-Black","MCA reading score","Yes",
      dv2g(0.023, 0.049^2, 401-1),
      NA,NA),
    c("Borman","2012","No","Minority subgroup-Black","MCA math score","Yes",
      dv2g(0.073, 0.054^2, 403-1),
      NA,NA),
    c("Borman","2012","No","Minority subgroup-Hispanic","MCA reading score","Yes",
      dv2g(0.055, 0.084^2, 187-1),
      NA,NA),
    c("Borman","2012","No","Minority subgroup-Hispanic","MCA math score","Yes",
      dv2g(0.053, 0.074^2, 186-1),
      NA,NA),
    c("Borman","2012","No","Minority subgroup-Female","MCA reading score","Yes",
      dv2g(0.019, 0.039^2, 651-1),
      NA,NA),
    c("Borman","2012","No","Minority subgroup-Female","MCA math score","Yes",
      dv2g(0.104, 0.035^2, 654-1),
      NA,NA),
    # Majority subgroup
    c("Borman","2012","No","Majority subgroup-Asian","MCA reading score","Yes",
      dv2g(-0.066, 0.050^2, 341-1),
      NA,NA),
    c("Borman","2012","No","Majority subgroup-Asian","MCA math score","Yes",
      dv2g(-0.075, 0.046^2, 342-1),
      NA,NA),
    c("Borman","2012","No","Majority subgroup-Male","MCA reading score","Yes",
      dv2g(-0.022, 0.042^2, 667-1),
      NA,NA),
    c("Borman","2012","No","Majority subgroup-Male","MCA math score","Yes",
      dv2g(-0.005, 0.039^2, 668-1),
      NA,NA)
    
  )
  
}

# 6. Borman 2015 --------------------------------------------------------------

Borman2015 <- function(){
  
  # leave empty to avoid double count with Borman 2018
  c("Borman", "2015", rep(NA,10))
  
}

# 7. Borman 2016 --------------------------------------------------------------

Borman2016 <- function(){
  
  # Table A5
  rbind(
    # Borman 2018 reported 9th grade cumulative GPA
    #c("Borman","2016","No","Interaction-URM","7th GPA","Yes",
    #  d2g(0.09, 506, 506),
    #  (3.43-2.57)/(0.082/0.11), NA),
    c("Borman","2016","No","Interaction-URM","Fall Reading test","Yes",
      d2g(-0.03, 463, 463),
      50.27/54.45,
      2.928/(1.939/0.03)),
    c("Borman","2016","No","Interaction-URM","Fall Math test","Yes",
      d2g(0.06, 463, 463),
      52.34/56.49,
      5.311/(2.948/0.06)),
    c("Borman","2016","No","Interaction-URM","Spring Reading test","Yes",
      d2g(0.00, 463, 463),
      50.27/54.45,
      2.234/(0.023/0.001)), # assuming rounded (0.00) = 0.001
    c("Borman","2016","No","Interaction-URM","Spring Math test","Yes",
      d2g(0.08, 463, 463),
      52.34/56.49,
      2.927/(1.578/0.08)),
    c("Borman","2016","No","Interaction-URM","Spring Language Usage test","Yes",
      d2g(0.10, 463, 463),
      NA,
      1.167/(1.386/0.1))
  )

}

# 8. Borman 2018 ----------------------------------------------------------

Borman2018 <- function(){
  
  # different outcomes from Borman 2016
  rbind(
    
  c("Borman","2018","No","Interaction-URM","9th GPA","Yes",
    extract_g(esc_B(0.177+0.036,1.02,920/2,920/2, es.type = "g")),
    (3.3137931034482757-2.232758620689655)/(0.177/0.174), # or grade 6 achievement gap = 0.66/0.65
    0.331/1.02), # webplotdigitizer
  c("Borman","2018","No","Interaction-White and Asian","9th GPA","Yes",
    extract_g(esc_B(0.036,1.02,920/2,920/2, es.type = "g")),
    NA,NA)
  #c("Borman","2018","No","Minority subgroup-URM","7th-9th GPA","Yes",
  #  extract_g(esc_B(0.206,1.02,324/2,324/2, es.type = "g"))) # duplicative analysis
  
  )
}


# 9. Bowen 2013 --------------------------------------------------------------

Bowen2013 <- function(){
  
  rbind(
    c("Bowen","2013","No","Minority subgroup-Black","Social Studies grades","Yes",
      d2g(0.57 / (1- 3/ (4*(58+74)-1)), 58,74),
      NA,NA) # majority is black
  )
}


# 10. Bratter 2016 --------------------------------------------------------

Bratter2016 <- function(){
  rbind(
    
    c("Bratter","2016","No","Minority subgroup-Black","Semester English grades","No",
      extract_g(esc_B(2.48, sqrt(((430*0.26-1) * 11.78^2 + (456*0.25-1) * 12.46^2)/(430*0.26+456*0.25-2)),430*0.26,456*0.25, es.type = "g")),
      NA,6.81/12.12811),
    c("Bratter","2016","No","Minority subgroup-Black","STAAR Reading","No",
      extract_g(esc_B(-22.34, sqrt(((430*0.26-1) * 228.73^2 + (456*0.25-1) * 248.34^2)/(430*0.26+456*0.25-2)),430*0.26,456*0.25, es.type = "g")),
      NA,79.61/238.8327),
    c("Bratter","2016","No","Minority subgroup-Black","STAAR Algebra","No",
      extract_g(esc_B(-84.7, sqrt(((430*0.26-1) * 429.01^2 + (456*0.25-1) * 420.19^2)/(430*0.26+456*0.25-2)),430*0.26,456*0.25, es.type = "g")),
      NA,169.98/424.5795),
    c("Bratter","2016","No","Minority subgroup-Hispanic","Semester English grades","No",
      extract_g(esc_B(-0.28, sqrt(((430*0.63-1) * 11.78^2 + (456*0.61-1) * 12.46^2)/(430*0.63+456*0.61-2)),430*0.63,456*0.61, es.type = "g")),
      NA,5.87/12.12928),
    c("Bratter","2016","No","Minority subgroup-Hispanic","STAAR Reading","No",
      extract_g(esc_B(-21.51, sqrt(((430*0.63-1) * 228.73^2 + (456*0.61-1) * 248.34^2)/(430*0.63+456*0.61-2)),430*0.63,456*0.61, es.type = "g")),
      NA,101.15/238.8664),
    c("Bratter","2016","No","Minority subgroup-Hispanic","STAAR Algebra","No",
      extract_g(esc_B(-9.15, sqrt(((430*0.63-1) * 429.01^2 + (456*0.61-1) * 420.19^2)/(430*0.63+456*0.61-2)),430*0.63,456*0.61, es.type = "g")),
      NA,173.89/424.5644),
    
    
    c("Bratter","2016","No","Minority subgroup-Black","Semester English grades","Yes",
      extract_g(esc_B(1.46, sqrt(((430*0.26-1) * 11.78^2 + (456*0.25-1) * 12.46^2)/(430*0.26+456*0.25-2)),430*0.26,456*0.25, es.type = "g")),
      NA,6.81/12.12811),
    c("Bratter","2016","No","Minority subgroup-Black","STAAR Reading","Yes",
      extract_g(esc_B(-54.62, sqrt(((430*0.26-1) * 228.73^2 + (456*0.25-1) * 248.34^2)/(430*0.26+456*0.25-2)),430*0.26,456*0.25, es.type = "g")),
      NA,79.61/238.8327),
    c("Bratter","2016","No","Minority subgroup-Black","STAAR Algebra","Yes",
      extract_g(esc_B(-122.71, sqrt(((430*0.26-1) * 429.01^2 + (456*0.25-1) * 420.19^2)/(430*0.26+456*0.25-2)),430*0.26,456*0.25, es.type = "g")),
      NA,169.98/424.5795),
    c("Bratter","2016","No","Minority subgroup-Hispanic","Semester English grades","Yes",
      extract_g(esc_B(-0.38, sqrt(((430*0.63-1) * 11.78^2 + (456*0.61-1) * 12.46^2)/(430*0.63+456*0.61-2)),430*0.63,456*0.61, es.type = "g")),
      NA,5.87/12.12928),
    c("Bratter","2016","No","Minority subgroup-Hispanic","STAAR Reading","Yes",
      extract_g(esc_B(-19.10, sqrt(((430*0.63-1) * 228.73^2 + (456*0.61-1) * 248.34^2)/(430*0.63+456*0.61-2)),430*0.63,456*0.61, es.type = "g")),
      NA,101.15/238.8664),
    c("Bratter","2016","No","Minority subgroup-Hispanic","STAAR Algebra","Yes",
      extract_g(esc_B(-11.01, sqrt(((430*0.63-1) * 429.01^2 + (456*0.61-1) * 420.19^2)/(430*0.63+456*0.61-2)),430*0.63,456*0.61, es.type = "g")),
      NA,173.89/424.5644)
    
  )
  
}


# 11. Churchill 2018 ------------------------------------------------------

Churchill2018 <- function(){
  
  rbind(
    c("Churchill","2018","No","Main","Music examination grade","No",
      extract_g(esc_mean_sd(62.18, 5.63, 35, 60.97, 5.98, 32, es.type = "g")),
      NA,NA)
    
  )
  
  
}



# 12. Cohen 2006 ----------------------------------------------------------

Cohen2006 <- function(){
  
  # some calculations are commented out in order not to double count different analysis using the same data
  # GPA outcome is kept because Cohen 2009 combined cohorts rather than just analyzing the exact same cohort
  rbind(
  c("Cohen (Study 1)","2006","No","Minority subgroup-Black","GPA in targeted course","Yes",
    extract_g(esc_B(0.26, 1, 25, 25,es.type = "g")),
    0.68/1, 
    0.44/1),
  #c("Cohen (Study 1)","2006","No","Interaction-Black","GPA in targeted course","Yes",
  #  extract_g(esc_B(0.09+0.29, 1, 55.5, 55.5,es.type = "g"))),
  c("Cohen (Study 1)","2006","No","Interaction-White","GPA in targeted course","Yes",
    extract_g(esc_B(0.09, 1, 55.5, 55.5,es.type = "g")),
    NA, NA),
  c("Cohen (Study 1)","2006","No","Minority subgroup-Black","GPA in non-targeted course","Yes",
    extract_g(esc_B(0.31, 1, 25, 25,es.type = "g")),
    NA, NA),
  #c("Cohen (Study 1)","2006","No","Interaction-Black","GPA in non-targeted course","Yes",
  #  extract_g(esc_B(0.45, 1, 55.5, 55.5,es.type = "g"))),
  c("Cohen (Study 2)","2006","No","Minority subgroup-Black","GPA in targeted course","Yes",
    extract_g(esc_B(0.34, 1, 34.5, 34.5,es.type = "g")),
    0.82/1, 
    0.11/1),
  #c("Cohen (Study 2)","2006","No","Interaction-Black","GPA in targeted course","Yes",
  ## extract_g(esc_B(0.52, 1, 66, 66,es.type = "g"))),
  c("Cohen (Study 2)","2006","No","Interaction-White","GPA in targeted course","Yes",
    extract_g(esc_B(0.03, 1, 66, 66,es.type = "g")),
    NA, NA),
  c("Cohen (Study 2)","2006","No","Minority subgroup-Black","GPA in non-targeted course","Yes",
    extract_g(esc_B(0.21, 1, 34.5, 34.5,es.type = "g")),
    NA, NA)
  
  )
}


# 13. Cohen 2009 ----------------------------------------------------------

Cohen2009 <- function(){
  # some calculations are commented out in order not to double count different analysis using the same data
  rbind(
  c("Cohen","2009","No","Minority subgroup-Black","GPA","Yes",
    extract_g(esc_B(0.24, 1, 87.5, 87.5,es.type = "g")),
    NA, 
    0.08/1),
  c("Cohen","2009","No","Majority subgroup-White","GPA","Yes",
    extract_g(esc_B(-0.07, 1, 94, 94,es.type = "g")),
    NA, NA),
  c("Cohen","2009","No","Minority subgroup-Black","Non-placement in remediation","No",
    extract_g(esc_bin_prop(0.97,87.5,0.91,87.5, es.type = "g")),
    extract_g(esc_bin_prop(0.97,94,0.91,87.5, es.type = "g"))[1], NA)

  # c("Cohen","2009","No","Majority subgroup-White","GPA","Yes",
  #  extract_g(esc_bin_prop(1,94,0.97,94, es.type = "g"))), # this returns Inf
  #c("Cohen","2009","No","Interaction-Black","GPA","Yes",
  #  extract_g(esc_B(0.33, 1, 181.5, 181.5,es.type = "g")))
  #c("Cohen","2009","No","Interaction-White","GPA","Yes",
  #  extract_g(esc_B(-0.02, 1, 181.5, 181.5,es.type = "g")))
  )
}


# 14. Cook 2012 -----------------------------------------------------------

Cook2012 <- function(){
  
  rbind(
  # leave empty to avoid double count with Cohen 2006, 2009
  c("Cook (Study 1)", "2012", rep(NA,10))
  )
}



# 15. De Clercq 2019 ------------------------------------------------------

DeClercq2019 <- function(){
  
  rbind(
  # Author shared data
  c("De Clercq","2019","No","Main","Test score","No",
    extract_g(esc_mean_sd(2.664, 1.5642, 125, 2.737, 1.5112, 112, es.type = "g")),
    NA,NA),
  c("De Clercq","2019","No","Main","Study time","No",
    extract_g(esc_mean_sd(3.6488, 1.09978, 84, 3.7821, 1.09482, 78, es.type = "g")),
    NA,NA)
  )
}


# 16. de Jong 2016 --------------------------------------------------------

deJong2016 <- function(){
  
  rbind(
  c("de Jong (Study 1)","2016","No","Minority subgroup-URM","Dutch","No",
    extract_g(esc_mean_sd(6.68, 0.78, 144, 6.76, 0.83, 133, es.type = "g")),
    extract_g(esc_mean_sd(7.09, 0.93, 31, 6.76, 0.83, 133, es.type = "g"))[1], NA),
  c("de Jong (Study 1)","2016","No","Minority subgroup-URM","English","No",
    extract_g(esc_mean_sd(6.96, 1.05, 144, 6.95, 1.28, 133, es.type = "g")),
    extract_g(esc_mean_sd(6.90, 1.29, 31, 6.95, 1.28, 133, es.type = "g"))[1], NA),
  c("de Jong (Study 1)","2016","No","Minority subgroup-URM","Mathematics","No",
    extract_g(esc_mean_sd(6.55, 1.12, 144, 6.63, 1.01, 133, es.type = "g")),
    extract_g(esc_mean_sd(7.07, 1.10, 31, 6.63, 1.01, 133, es.type = "g"))[1], NA),
  
  c("de Jong (Study 1)","2016","No","Majority subgroup-URM","Dutch","No",
    extract_g(esc_mean_sd(6.94, 0.95, 28, 7.09, 0.93, 31, es.type = "g")),
    NA,NA),
  c("de Jong (Study 1)","2016","No","Majority subgroup-URM","English","No",
    extract_g(esc_mean_sd(7, 1.22, 28, 6.9, 1.29, 31, es.type = "g")),
    NA,NA),
  c("de Jong (Study 1)","2016","No","Majority subgroup-URM","Mathematics","No",
    extract_g(esc_mean_sd(6.93, 1.16, 28, 7.07, 1.1, 31, es.type = "g")),
    NA,NA),
  
  c("de Jong (Study 2)","2016","Yes","Minority subgroup-URM","Cito scores","No",
    extract_g(esc_mean_sd(530.21, 11.24, 88, 530.43, 11.71, 84, es.type = "g")),
    extract_g(esc_mean_sd(531.33, 8.08, 3, 530.20, 11.71, 84, es.type = "g"))[1], NA),
  c("de Jong (Study 2)","2016","No","Minority subgroup-URM","Cito scores","No",
    extract_g(esc_mean_sd(529.80, 10.26, 94, 530.43, 11.71, 84, es.type = "g")),
    extract_g(esc_mean_sd(531.33, 8.08, 3, 530.20, 11.71, 84, es.type = "g"))[1], NA),
  c("de Jong (Study 2)","2016","Yes","Majority subgroup-White","Cito scores","No",
    extract_g(esc_mean_sd(530.50, 11.36, 6, 531.33, 8.08, 3, es.type = "g")),
    NA,NA),
  c("de Jong (Study 2)","2016","No","Majority subgroup-White","Cito scores","No",
    extract_g(esc_mean_sd(527.33, 5.69, 3, 531.33, 8.08, 3, es.type = "g")),
    NA,NA)
  )
  
  
}



# 17. Dee 2015 ----------------------------------------------------------------

Dee2015 <- function(){
  
  rbind(
    c("Dee","2015","No","Interaction-Black","Final grades in the treated subject","No",
      extract_g(esc_B(0.228, 10.8, 2348/2, 2348/2,es.type = "g")),
      7.136/10.8,NA),
    c("Dee","2015","No","Interaction-Hispanic","Final grades in the treated subject","No",
      extract_g(esc_B(0.582, 10.8, 2348/2, 2348/2,es.type = "g")),
      9.14/10.8,NA),
    c("Dee","2015","No","Interaction-Female","Final grades in the treated subject","No",
      extract_g(esc_B(-0.28, 10.8, 2348/2, 2348/2,es.type = "g")),
      3.785/10.8,NA),
    c("Dee","2015","No","Interaction-White","Final grades in the treated subject","No",
      extract_g(esc_B(-0.028, 10.8, 2348/2, 2348/2,es.type = "g")),
      NA,NA),
    c("Dee","2015","No","Interaction-Black","Final grades in the treated subject","Yes",
      extract_g(esc_B(-0.009, 10.8, 2348/2, 2348/2,es.type = "g")),
      NA,
      (2.177+(4.626-0.957)/2)/10.8),
    c("Dee","2015","No","Interaction-Hispanic","Final grades in the treated subject","Yes",
      extract_g(esc_B(0.591, 10.8, 2348/2, 2348/2,es.type = "g")),
      NA,
      (2.177+(4.84-2.371)/2)/10.8),
    c("Dee","2015","No","Interaction-Female","Final grades in the treated subject","Yes",
      extract_g(esc_B(-0.431, 10.8, 2348/2, 2348/2,es.type = "g")),
      NA,NA),
    c("Dee","2015","No","Interaction-White","Final grades in the treated subject","Yes",
      extract_g(esc_B(0.063, 10.8, 2348/2, 2348/2,es.type = "g")),
      NA,NA)
  )
  
}


# 18. Goyer 2017 --------------------------------------------------------------

Goyer2017 <- function(goyer){
  

  rbind(
    # Study 1 uses Sherman 2013, effect on GPA is not recalculated to avoid double dipping
    
    # course difficulty score
    c("Goyer (Study 1)","2017","No","Minority subgroup-Hispanic","Course Difficulty Scores","No",
      extract_g(esc_mean_se(goyer[1,3], goyer[1,4], goyer[1,5], goyer[1,6], goyer[1,7], goyer[1,8], es.type = "g")),
      extract_g(esc_mean_se(goyer[1,12], goyer[1,13], goyer[1,14], goyer[1,6], goyer[1,7], goyer[1,8], es.type = "g"))[1], NA
      ),
    
    c("Goyer (Study 1)","2017","No","Majority subgroup-White","Course Difficulty Scores","No",
      extract_g(esc_mean_se(goyer[1,9], goyer[1,10], goyer[1,11], goyer[1,12], goyer[1,13], goyer[1,14], es.type = "g")),
    NA,NA  
    ),
    c("Goyer (Study 1)","2017","No","Minority subgroup-Hispanic","Course Difficulty Scores","Yes",
      extract_g(esc_mean_se(goyer[2,3], goyer[2,4], goyer[2,5], goyer[2,6], goyer[2,7], goyer[2,8], es.type = "g")),
      NA,extract_g(esc_mean_se(goyer[2,12], goyer[2,13], goyer[2,14], goyer[2,6], goyer[2,7], goyer[2,8], es.type = "g"))[1]
    ),
    
    c("Goyer (Study 1)","2017","No","Majority subgroup-White","Course Difficulty Scores","Yes",
      extract_g(esc_mean_se(goyer[2,9], goyer[2,10], goyer[2,11], goyer[2,12], goyer[2,13], goyer[2,14], es.type = "g")),
      NA,NA  
    ),
    
    # enrollment in remedial clinics (reverse code)

    c("Goyer (Study 1)","2017","No","Minority subgroup-Hispanic","Non-enrollment in remedial clinics","No",
      -extract_g(esc_mean_se(goyer[3,3], goyer[3,4], goyer[3,5], goyer[3,6], goyer[3,7], goyer[3,8], es.type = "g")),
      -extract_g(esc_mean_se(goyer[3,12], goyer[3,13], goyer[3,14], goyer[3,6], goyer[3,7], goyer[3,8], es.type = "g"))[1], NA
    ),
    
    c("Goyer (Study 1)","2017","No","Majority subgroup-White","Non-enrollment in remedial clinics","No",
      -extract_g(esc_mean_se(goyer[3,9], goyer[3,10], goyer[3,11], goyer[3,12], goyer[3,13], goyer[3,14], es.type = "g")),
      NA,NA  
    ),
    c("Goyer (Study 1)","2017","No","Minority subgroup-Hispanic","Non-enrollment in remedial clinics","Yes",
      -extract_g(esc_mean_se(goyer[4,3], goyer[4,4], goyer[4,5], goyer[4,6], goyer[4,7], goyer[4,8], es.type = "g")),
      NA,-extract_g(esc_mean_se(goyer[4,12], goyer[4,13], goyer[4,14], goyer[4,6], goyer[4,7], goyer[4,8], es.type = "g"))[1]
    ),
    
    c("Goyer (Study 1)","2017","No","Majority subgroup-White","Non-enrollment in remedial clinics","Yes",
      -extract_g(esc_mean_se(goyer[4,9], goyer[4,10], goyer[4,11], goyer[4,12], goyer[4,13], goyer[4,14], es.type = "g")),
      NA,NA  
    ), 
    
    # enrollment in AVID (only for Latino students)
    
    c("Goyer (Study 1)","2017","No","Minority subgroup-Hispanic","Enrollment in AVID","No",
      extract_g(esc_mean_se(goyer[5,3], goyer[5,4], goyer[5,5], goyer[5,6], goyer[5,7], goyer[5,8], es.type = "g")),
      extract_g(esc_mean_se(goyer[5,12], goyer[5,13], goyer[5,14], goyer[5,6], goyer[5,7], goyer[5,8], es.type = "g"))[1], NA
    ),
    
    #c("Goyer (Study 1)","2017","No","Majority subgroup-White","Enrollment in AVID","No",
    #  extract_g(esc_mean_se(goyer[5,9], goyer[5,10], goyer[5,11], goyer[5,12], goyer[5,13], goyer[5,14], es.type = "g")),
    #  NA,NA  
    #),
    c("Goyer (Study 1)","2017","No","Minority subgroup-Hispanic","Enrollment in AVID","Yes",
      extract_g(esc_mean_se(goyer[6,3], goyer[6,4], goyer[6,5], goyer[6,6], goyer[6,7], goyer[6,8], es.type = "g")),
      NA,extract_g(esc_mean_se(goyer[6,12], goyer[6,13], goyer[6,14], goyer[6,6], goyer[6,7], goyer[6,8], es.type = "g"))[1]
    ),
    
    #c("Goyer (Study 1)","2017","No","Majority subgroup-White","Enrollment in AVID","Yes",
    #  extract_g(esc_mean_se(goyer[6,9], goyer[6,10], goyer[6,11], goyer[6,12], goyer[6,13], goyer[6,14], es.type = "g")),
    #  NA,NA  
    #), 
    
    # enrollment in main high school
    
    c("Goyer (Study 1)","2017","No","Minority subgroup-Hispanic","Enrollment in main high school","No",
      extract_g(esc_mean_se(goyer[7,3], goyer[7,4], goyer[7,5], goyer[7,6], goyer[7,7], goyer[7,8], es.type = "g")),
      extract_g(esc_mean_se(goyer[7,12], goyer[7,13], goyer[7,14], goyer[7,6], goyer[7,7], goyer[7,8], es.type = "g"))[1], NA
    ),
    
    c("Goyer (Study 1)","2017","No","Majority subgroup-White","Enrollment in main high school","No",
      extract_g(esc_mean_se(goyer[7,9], goyer[7,10], goyer[7,11], goyer[7,12], goyer[7,13], goyer[7,14], es.type = "g")),
      NA,NA  
    ),
    c("Goyer (Study 1)","2017","No","Minority subgroup-Hispanic","Enrollment in main high school","Yes",
      extract_g(esc_mean_se(goyer[8,3], goyer[8,4], goyer[8,5], goyer[8,6], goyer[8,7], goyer[8,8], es.type = "g")),
      NA,extract_g(esc_mean_se(goyer[8,12], goyer[8,13], goyer[8,14], goyer[8,6], goyer[8,7], goyer[8,8], es.type = "g"))[1]
    ),
    
    c("Goyer (Study 1)","2017","No","Majority subgroup-White","Enrollment in main high school","Yes",
      extract_g(esc_mean_se(goyer[8,9], goyer[8,10], goyer[8,11], goyer[8,12], goyer[8,13], goyer[8,14], es.type = "g")),
      NA,NA  
    ), 
    
    
    # study 2 uses Cohen 2009, three addition outcomes are calculated here
    
    # enrollment in college
    
    c("Goyer (Study 2)","2017","No","Minority subgroup-Black","Enrollment in college","No",
      extract_g(esc_mean_se(goyer[9,3], goyer[9,4], goyer[9,5], goyer[9,6], goyer[9,7], goyer[9,8], es.type = "g")),
      extract_g(esc_mean_se(goyer[9,12], goyer[9,13], goyer[9,14], goyer[9,6], goyer[9,7], goyer[9,8], es.type = "g"))[1], NA
    ),
    
    c("Goyer (Study 2)","2017","No","Majority subgroup-White","Enrollment in college","No",
      extract_g(esc_mean_se(goyer[9,9], goyer[9,10], goyer[9,11], goyer[9,12], goyer[9,13], goyer[9,14], es.type = "g")),
      NA,NA  
    ),
    c("Goyer (Study 2)","2017","No","Minority subgroup-Black","Enrollment in college","Yes",
      extract_g(esc_mean_se(goyer[10,3], goyer[10,4], goyer[10,5], goyer[10,6], goyer[10,7], goyer[10,8], es.type = "g")),
      NA,extract_g(esc_mean_se(goyer[10,12], goyer[10,13], goyer[10,14], goyer[10,6], goyer[10,7], goyer[10,8], es.type = "g"))[1]
    ),
    
    c("Goyer (Study 2)","2017","No","Majority subgroup-White","Enrollment in college","Yes",
      extract_g(esc_mean_se(goyer[10,9], goyer[10,10], goyer[10,11], goyer[10,12], goyer[10,13], goyer[10,14], es.type = "g")),
      NA,NA  
    ), 
    
    # enrollment in a 4-year college
    
    c("Goyer (Study 2)","2017","No","Minority subgroup-Black","Enrollment in a 4-year college","No",
      extract_g(esc_mean_se(goyer[11,3], goyer[11,4], goyer[11,5], goyer[11,6], goyer[11,7], goyer[11,8], es.type = "g")),
      extract_g(esc_mean_se(goyer[11,12], goyer[11,13], goyer[11,14], goyer[11,6], goyer[11,7], goyer[11,8], es.type = "g"))[1], NA
    ),
    
    c("Goyer (Study 2)","2017","No","Majority subgroup-White","Enrollment in a 4-year college","No",
      extract_g(esc_mean_se(goyer[11,9], goyer[11,10], goyer[11,11], goyer[11,12], goyer[11,13], goyer[11,14], es.type = "g")),
      NA,NA  
    ),
    c("Goyer (Study 2)","2017","No","Minority subgroup-Black","Enrollment in a 4-year college","Yes",
      extract_g(esc_mean_se(goyer[12,3], goyer[12,4], goyer[12,5], goyer[12,6], goyer[12,7], goyer[12,8], es.type = "g")),
      NA,extract_g(esc_mean_se(goyer[12,12], goyer[12,13], goyer[12,14], goyer[12,6], goyer[12,7], goyer[12,8], es.type = "g"))[1]
    ),
    
    c("Goyer (Study 2)","2017","No","Majority subgroup-White","Enrollment in a 4-year college","Yes",
      extract_g(esc_mean_se(goyer[12,9], goyer[12,10], goyer[12,11], goyer[12,12], goyer[12,13], goyer[12,14], es.type = "g")),
      NA,NA  
    ), 
    
    # 4-year college selectivity
    
    c("Goyer (Study 2)","2017","No","Minority subgroup-Black","4-year college selectivity","No",
      extract_g(esc_mean_se(goyer[13,3], goyer[13,4], goyer[13,5], goyer[13,6], goyer[13,7], goyer[13,8], es.type = "g")),
      extract_g(esc_mean_se(goyer[13,12], goyer[13,13], goyer[13,14], goyer[13,6], goyer[13,7], goyer[13,8], es.type = "g"))[1], NA
    ),
    
    c("Goyer (Study 2)","2017","No","Majority subgroup-White","4-year college selectivity","No",
      extract_g(esc_mean_se(goyer[13,9], goyer[13,10], goyer[13,11], goyer[13,12], goyer[13,13], goyer[13,14], es.type = "g")),
      NA,NA  
    ),
    c("Goyer (Study 2)","2017","No","Minority subgroup-Black","4-year college selectivity","Yes",
      extract_g(esc_mean_se(goyer[14,3], goyer[14,4], goyer[14,5], goyer[14,6], goyer[14,7], goyer[14,8], es.type = "g")),
      NA,extract_g(esc_mean_se(goyer[14,12], goyer[14,13], goyer[14,14], goyer[14,6], goyer[14,7], goyer[14,8], es.type = "g"))[1]
    ),
    
    c("Goyer (Study 2)","2017","No","Majority subgroup-White","4-year college selectivity","Yes",
      extract_g(esc_mean_se(goyer[14,9], goyer[14,10], goyer[14,11], goyer[14,12], goyer[14,13], goyer[14,14], es.type = "g")),
      NA,NA  
    )
    
  )
  
}


# 19. Gutmann 2019 --------------------------------------------------------

Gutmann2019 <- function(){
  # Numbers extracted from plots using WebPlotDigitizer
  
  # only final grades are kept, as midterm and exam scores build up to the final grades
  rbind(
    #c("Gutmann","2019","No","Minority subgroup-Female","Physics 100 midterm exam score","No",
    #  extract_g(esc_mean_se(0.668485523, 0.687193764-0.668485523, 75, 0.673385301, 0.687639198-0.673385301, 73, es.type = "g"))),
    #c("Gutmann","2019","No","Majority subgroup-Male","Physics 100 midterm exam score","No",
    #  extract_g(esc_mean_se(0.672494432, 0.691648107-0.672494432, 121, 0.71481069, 0.729064588-0.71481069, 120, es.type = "g"))),
    #c("Gutmann","2019","No","Minority subgroup-Female","Physics 212 hour exam 1 score","No",
    #  extract_g(esc_mean_se(0.680530973, 0.696570796-0.680530973, 143, 0.710951327, 0.720353982-0.710951327, 122, es.type = "g"))),
    #c("Gutmann","2019","No","Majority subgroup-Male","Physics 212 hour exam 1 score","No",
    #  extract_g(esc_mean_se(0.748561947, 0.765707965-0.748561947, 366, 0.734734513, 0.743584071-0.734734513, 381, es.type = "g")),
    
    # using the Cochrane calculator for ordered final grades (A-D)
    c("Gutmann","2019","No","Minority subgroup-Female","Physics 100 final grades","No",
      dv2g(-0.2212, 0.0273, 75+73),
      -0.0058, NA),
    c("Gutmann","2019","No","Majority subgroup-Male","Physics 100 final grades","No",
      dv2g(-0.328, 0.0162, 121+120),
      NA,NA),
    c("Gutmann","2019","No","Minority subgroup-Female","Physics 212 final grades","No",
      dv2g(-0.1159, 0.0152, 143+122),
      0.0809, NA),
    c("Gutmann","2019","No","Majority subgroup-Male","Physics 212 final grades","No",
      dv2g(0.0852, 0.0054, 366+381),
      NA,NA)
  )
  
}


# 20. Hadden 2019 -------------------------------------------------------------------

Hadden2019 <- function(){
  
  rbind(
    c("Hadden","2019","No","Minority subgroup-FSM","GCSE Mathematics","No",
      extract_g(esc_mean_se(-0.084,(-0.084+0.303)/1.96,69,-0.438,(-0.438+0.588)/1.96,59,es.type = "g")),
      0.527/0.996, NA
      ),
    c("Hadden","2019","No","Majority subgroup-nonFSM","GCSE Mathematics","No",
      extract_g(esc_mean_se(0.088,(0.088+0.039)/1.96,224,0.048,(0.048+0.073)/1.96,210,es.type = "g")),
      NA,NA),
    c("Hadden","2019","No","Minority subgroup-FSM","GCSE Mathematics","Yes",
      extract_g(esc_mean_se(-0.099,(-0.099+0.280)/1.96,69,-0.478,(-0.478+0.645)/1.96,59,es.type = "g")),
      NA, 0.517/0.996),
    c("Hadden","2019","No","Majority subgroup-nonFSM","GCSE Mathematics","Yes",
      extract_g(esc_mean_se(0.1,(0.1+0.037)/1.96,224,0.049,(0.049+0.071)/1.96,210,es.type = "g")),
      NA,NA)
  )
  
}


# 21. Hanselman 2014 ------------------------------------------------------

Hanselman2014 <- function(){
  
  # Similar to Borman 2016, but they reported multilevel model with different sets of covariates, drop to avoid reanalysis
  rbind(
    #c("Hanselman","2014","No","Minority subgroup-URM","GPA","Yes",
    #  extract_g(esc_B(0.068,0.68,310/2,310/2,es.type = "g"))),
    #c("Hanselman","2014","No","Majority subgroup-White","GPA","Yes",
    #  extract_g(esc_B(-0.012,0.68,600/2,600/2,es.type = "g")))
    
    c("Hanselman","2014", rep(NA, 10))
    
  )
  
}


# 22. Hanselman 2017 ------------------------------------------------------

Hanselman2017 <- function(){

  rbind(
  # Study 1 refers to Cohort 1, and that uses the same data as Borman 2016, 2018. 
  # However, Math and Reading score for Grade 8 of the 2011 Cohort was not previously reported.
    
 #c("Hanselman (Study 1)","2017","No","Minority subgroup-Black and Hispanic","7th GPA","Yes",
 #  dv2g(0.062,	0.057^2, 331)),
 #c("Hanselman (Study 1)","2017","No","Minority subgroup-Black and Hispanic","8th GPA","Yes",
 #   dv2g(0.152,	0.07^2, 331)),
  #c("Hanselman (Study 1)","2017","No","Minority subgroup-Black and Hispanic","WKCE Mathematcis, Grade 7","Yes",
  #  dv2g(0.072,	0.059^2, 331)),
  c("Hanselman (Study 1)","2017","No","Minority subgroup-Black and Hispanic","WKCE Mathematcis, Grade 8","Yes",
    dv2g(0.101,	0.07^2, 331),
    NA,NA),
  #c("Hanselman (Study 1)","2017","No","Minority subgroup-Black and Hispanic","WKCE Reading, Grade 7","Yes",
  # dv2g(-0.034,	0.069^2, 331)),
  c("Hanselman (Study 1)","2017","No","Minority subgroup-Black and Hispanic","WKCE Reading, Grade 8","Yes",
    dv2g(-0.03,	0.071^2, 331),
    NA,NA),
  
  # Study 2 refers to Cohort 2
  #c("Hanselman (Study 2)","2017","No","Minority subgroup-Black and Hispanic","7th GPA","Yes",
  #  dv2g(-0.002, 0.042^2, 449)),
  c("Hanselman (Study 2)","2017","No","Minority subgroup-Black and Hispanic","8th GPA","Yes",
    dv2g(-0.072, 0.058^2, 449),
    (3.3986486486486482-2.668918918918919)/(0.058*sqrt(449)), NA),# webplotdigitizer
  c("Hanselman (Study 2)","2017","No","Minority subgroup-Black and Hispanic","WKCE Mathematcis, Grade 7","Yes",
    dv2g(-0.085,	0.047^2, 449),
    NA,NA),
  c("Hanselman (Study 2)","2017","No","Minority subgroup-Black and Hispanic","WKCE Mathematcis, Grade 8","Yes",
    dv2g(-0.08,	0.044^2, 449),
    NA,NA),
  c("Hanselman (Study 2)","2017","No","Minority subgroup-Black and Hispanic","WKCE Reading, Grade 7","Yes",
    dv2g(-0.005,	0.055^2, 449),
    NA,NA),
  c("Hanselman (Study 2)","2017","No","Minority subgroup-Black and Hispanic","WKCE Reading, Grade 8","Yes",
    dv2g(-0.005,	0.056^2, 449),
    NA,NA)
  ) 
}

# 23. Harackiewicz 2014 ---------------------------------------------------

Harackiewicz2014<- function(){
  
  rbind(
    
    c("Harackiewicz","2014","No","Majority subgroup-Continuing generation","Biology Course Grade","No",
      extract_g(esc_mean_sd(2.82, 0.69, 325, 2.86, 0.69, 319, es.type = "g")),
      NA,NA),
    c("Harackiewicz","2014","No","Majority subgroup-Continuing generation","GPA","No",
      extract_g(esc_mean_sd(3.17, 0.62, 325, 3.2, 0.63, 319, es.type = "g")),
      NA,NA),
    c("Harackiewicz","2014","No","Majority subgroup-Continuing generation","Continuation","No",
      extract_g(esc_bin_prop(0.748,325,0.777,319, es.type = "g")),
      NA,NA),
    
    c("Harackiewicz","2014","No","Minority subgroup-First generation","Biology Course Grade","No",
      extract_g(esc_mean_sd(2.62, 0.78, 77, 2.38, 0.85, 77, es.type = "g")),
      extract_g(esc_mean_sd(2.86, 0.69, 319, 2.38, 0.85, 77, es.type = "g"))[1], NA),
    c("Harackiewicz","2014","No","Minority subgroup-First generation","GPA","No",
      extract_g(esc_mean_sd(3.05, 0.64, 77, 2.81, 0.81, 77, es.type = "g")),
      extract_g(esc_mean_sd(3.20, 0.63, 319, 2.81, 0.81, 77, es.type = "g"))[1], NA),
    c("Harackiewicz","2014","No","Minority subgroup-First generation","Continuation","No",
      extract_g(esc_bin_prop(0.857,77,0.662,77, es.type = "g")),
      extract_g(esc_bin_prop(0.777,319,0.662,77, es.type = "g"))[1], NA),
    
    # this is an approximation of the marginal effect (no other stats were reported)
    c("Harackiewicz","2014","No","Interaction-First generation","Biology Course Grade","Yes",
      extract_g(esc_beta(0.09, 0.73, 325+77, 319+77, es.type = "g")),
      NA,NA),
    c("Harackiewicz","2014","No","Interaction-First generation","GPA","Yes",
      extract_g(esc_beta(0.1, 0.65, 325+77, 319+77, es.type = "g")),
      NA,NA),
    c("Harackiewicz","2014","No","Interaction-First generation","Continuation","Yes",
      extract_g(esc_chisq(8.8, 0.01, 798, es.type = "g")),
      NA,NA)

  )
  
}

# 24. Harackiewicz 2016 ---------------------------------------------------

# ignoring the UV condition

Harackiewicz2016 <- function(){
  
  rbind(
    
    c("Harackiewicz","2016","No","Interaction-URM","Biology course grade","Yes",
      extract_g(esc_B(2*(-0.01) + 2*(-0.02), 0.81, 1040/4, 1040/4, es.type = "g")),
      NA,NA),
    c("Harackiewicz","2016","No","Interaction-White and Asian","Biology course grade","Yes",
      extract_g(esc_B(2*(-0.01) - 2*(-0.02), 0.81, 1040/4, 1040/4, es.type = "g")),
      NA,NA),
    c("Harackiewicz","2016","No","Interaction-First generation","Biology course grade","Yes",
      extract_g(esc_B(2*(-0.01) + 2*(-0.05), 0.81, 1040/4, 1040/4, es.type = "g")),
      NA,NA),
    c("Harackiewicz","2016","No","Interaction-Continuing generation","Biology course grade","Yes",
      extract_g(esc_B(2*(-0.01) - 2*(-0.05), 0.81, 1040/4, 1040/4, es.type = "g")),
      NA,NA)
    
  )
  
}


# 25. Hayes 2019 ----------------------------------------------------------

Hayes2019 <- function(){
  
  rbind(
    
    # Study 1: almost entire sample is Latino
    # there are three conditions, one of which is not relevant
    c("Hayes (Study 1)","2019","No","Minority subgroup-Hispanic","Semester Grade","No",
      extract_g(esc_mean_sd(84.21, 4.97, 116/3, 85.77, 5.45, 116/3, es.type = "g")),
      NA,NA),
    
    # Study 2: # only 1st year student in the sample
    c("Hayes (Study 2)","2019","No","Minority subgroup-First generation","GPA","No",
      extract_g(esc_mean_se(2.86,0.1,39,2.93,0.1,39,es.type = "g")),
      NA,NA)
  )
  
}


# 26. Jordt 2017 ----------------------------------------------------------

Jordt2017 <- function(){
  
  # because they also have urm*treatment and gender*treatment in the regression model,
  # the effect for URM differs for male URM and female URM
  # since this is what they only reported, I am comparing URM female with White male to avoid double dipping from the same regression model
  
  rbind(
    c("Jordt","2017","No","Interaction-URM and Female","Exam grades","Yes",
      extract_g(esc_B(6.45-8.1+10.29, (10.29+6.45)*2 , 963, 970, es.type = "g")),
      NA,extract_g(esc_B(16.01+2.52, (10.29+6.45)*2 , 963, 970, es.type = "g"))[1]),
    c("Jordt","2017","No","Interaction-White and Male","Exam grades","Yes",
      extract_g(esc_B(6.45, (10.29+6.45)*2 , 963, 970, es.type = "g")),
      NA,NA),
    
    c("Jordt","2017","No","Interaction-URM","Exam grades","No",
      extract_g(esc_B(3.7+8.2, (3.7+8.2)*2 , 963, 970, es.type = "g")),
      extract_g(esc_B(25.6, (3.7+8.2)*2, 963, 970, es.type = "g"))[1],NA),
    c("Jordt","2017","No","Interaction-White","Exam grades","No",
      extract_g(esc_B(3.7, (3.7+8.2)*2, 963, 970, es.type = "g")),
      NA,NA)
  )
  
}


# 27. Kim 2019 ------------------------------------------------------------

Kim2019 <- function(){
  
  rbind(
    c("Kim","2019","Yes","Interaction-Female","GPA","No",
      extract_g(esc_B(3.06-3.18, 0.47, 110, 110, es.type = "g")),
      (3.35-3.06)/0.47, NA),
    
    # combined control condition
    c("Kim","2019","No","Interaction-Female","GPA","No",
      extract_g(esc_B(3.31-3.13, 0.47, 110, 110, es.type = "g")),
      (3.36-3.18)/0.47, NA),
    
    c("Kim","2019","Yes","Interaction-Female","GPA","Yes",
      f2g(0.15, 110, 110), # male was coded as 1
      NA,NA)
  )
  
}


# 28. Kinias 2016 ---------------------------------------------------------

Kinias2016 <- function(){
  
  rbind(
    c("Kinias (Study 2)","2016","No","Majority subgroup-Male","MBA core course grade","No",
      extract_g(esc_mean_sd(3.109, 0.688, 266/2, 3.205, 0.694, 266/2, es.type = "g")),
      NA,NA),
    c("Kinias (Study 2)","2016","No","Minority subgroup-Female","MBA core course grade","No",
      extract_g(esc_mean_sd(2.948, 0.654, 130/2, 2.739, 0.763, 130/2, es.type = "g")),
      extract_g(esc_mean_sd(3.205, 0.694, 266/2, 2.739, 0.763, 130/2, es.type = "g"))[1], NA),
    
    # ignoring the interactions with campus location
    # they have +1 -1 coding
    c("Kinias (Study 2)","2016","No","Interaction-Female","MBA core course grade","Yes",
      extract_g(esc_B(2*0.028 + 2*0.076, sqrt(( (198-1) * 0.694^2 + (198-1) * 0.763^2 ) / (198 + 198 - 2)), 198, 198, es.type = "g")),
      NA,2*0.157/0.7293165),
    c("Kinias (Study 2)","2016","No","Interaction-Male","MBA core course grade","Yes",
      extract_g(esc_B(2*0.028 - 2*0.076, sqrt(( (198-1) * 0.694^2 + (198-1) * 0.763^2 ) / (198 + 198 - 2)), 198, 198, es.type = "g")),
      NA,NA)
  )
  
}


# 29. Kost-Smith 2010 -----------------------------------------------------

Kostsmith2010 <- function(){
  
  # the adjusted effect sizes on FMCE grades are not reported in Miyake 2013
  
  # using Miyake 2010 to get pooled SD
  sd_ks <- sqrt(((137-1) * 26.3^2 + (55-1) * 30.6^2 + (75-1) * 27.3^2 + (41-1) * 25.1^2) /(137 + 55 + 75 + 41 - 4))
  
  rbind(
    c("Kost-Smith","2010","No","Minority subgroup-Female","FMCE grade","Yes",
      extract_g(esc_B(12.9-0.6, sd_ks, 55,41, es.type = "g")),
      NA, 12.7/sd_ks),
    c("Kost-Smith","2010","No","Majority subgroup-Male","FMCE grade","Yes",
      extract_g(esc_B(-0.6, sd_ks, 137,75, es.type = "g")),
      NA,NA)
  )
  
}

# 30. Kost-Smith 2012 -----------------------------------------------------

Kostsmith2012 <- function(kost){
  

  # unadjusted
  kost2012 <- kost %>% filter(Cohort == "Spring 2011") %>%
    mutate(AFFIRM = if_else(AFFIRM == 1, "Affirm", "Control")) %>%
    mutate(FEMALE = if_else(FEMALE == 1, "Female", "Male"))
  
  
  temp1 <- kost2012 %>% group_by(FEMALE, AFFIRM) %>% summarise(
    m_course_grade = mean(COURSE_GRADE, na.rm = T),
    sd_course_grade = sd(COURSE_GRADE, na.rm = T),
    m_course_score = mean(COURSE_SCORE, na.rm = T),
    sd_course_score = sd(COURSE_SCORE, na.rm = T),
    m_exam_score = mean(RAW_ALL_EXAMS, na.rm = T),
    sd_exam_score = sd(RAW_ALL_EXAMS, na.rm = T),
    .groups = "drop_last"
  )
  
  
  # drake is incompatible with emmeans: 
  #diagnose(kost_extracted)$error$message:
  #cannot add bindings to a locked environment
  # therefore I am manually inputting values here
  
  # adjusted
  
  # COURSE_GRADE
  #fit <- lm(COURSE_GRADE ~ as.factor(FEMALE) * as.factor(AFFIRM) + MATH, kost2012)
  #fit.emm <-  fit %>% emmeans("AFFIRM", "FEMALE")
  #temp2 <- as.data.frame(eff_size(fit.emm, sigma = sigma(fit), edf = 526))
 
  
  # COURSE_SCORE
  #fit <- lm(COURSE_SCORE ~ as.factor(FEMALE) * as.factor(AFFIRM) + MATH, kost2012)
  #fit.emm <- emmeans(fit, "AFFIRM", "FEMALE")
  #temp3 <- as.data.frame(eff_size(fit.emm, sigma = sigma(fit), edf = 526))
  
  # RAW_ALL_EXAMS
  #fit <- lm(RAW_ALL_EXAMS ~ as.factor(FEMALE) * as.factor(AFFIRM) + MATH, kost2012)
  #fit.emm <- emmeans(fit, "AFFIRM", "FEMALE")
  #temp4 <- as.data.frame(eff_size(fit.emm, sigma = sigma(fit), edf = 526))
  
  # unadjusted
  unadjusted <- rbind(
    c("Kost-Smith","2012","No","Minority subgroup-Female","Physics course grade","No",
      extract_g(esc_mean_sd(temp1[1,3], temp1[1,4], 89, temp1[2,3], temp1[2,4], 60, es.type = "g")),
      extract_g(esc_mean_sd(temp1[4,3], temp1[4,4], 89, temp1[2,3], temp1[2,4], 60, es.type = "g"))[1],NA),
    c("Kost-Smith","2012","No","Majority subgroup-Male","Physics course grade","No",
      extract_g(esc_mean_sd(temp1[3,3], temp1[3,4], 232, temp1[4,3], temp1[4,4], 150, es.type = "g")),
      NA,NA),
    
    c("Kost-Smith","2012","No","Minority subgroup-Female","Physics course score","No",
      extract_g(esc_mean_sd(temp1[1,5], temp1[1,6], 89, temp1[2,5], temp1[2,6], 60, es.type = "g")),
      extract_g(esc_mean_sd(temp1[4,5], temp1[4,6], 89, temp1[2,5], temp1[2,6], 60, es.type = "g"))[1],NA),
    c("Kost-Smith","2012","No","Majority subgroup-Male","Physics course score","No",
      extract_g(esc_mean_sd(temp1[3,5], temp1[3,6], 232, temp1[4,5], temp1[4,6], 150, es.type = "g")),
      NA,NA),
    
    c("Kost-Smith","2012","No","Minority subgroup-Female","Physics exam score","No",
      extract_g(esc_mean_sd(temp1[1,7], temp1[1,8], 89, temp1[2,7], temp1[2,8], 60, es.type = "g")),
      extract_g(esc_mean_sd(temp1[4,7], temp1[4,8], 89, temp1[2,7], temp1[2,8], 60, es.type = "g"))[1],NA),
    c("Kost-Smith","2012","No","Majority subgroup-Male","Physics exam score","No",
      extract_g(esc_mean_sd(temp1[3,7], temp1[3,8], 232, temp1[4,7], temp1[4,8], 150, es.type = "g")),
      NA,NA)
  )
  
  # adjusted
  
  
  adjusted <- rbind(
    c("Kost-Smith","2012","No","Minority subgroup-Female","Physics course grade","Yes",
      dv2g(0.20004813, (0.1671775)^2, 531),
      NA, (0.200708-0.009145)/0.9416941),
    c("Kost-Smith","2012","No","Majority subgroup-Male","Physics course grade","Yes",
      dv2g(-0.04042167, (0.1047923)^2, 531),
      NA,NA),
    c("Kost-Smith","2012","No","Minority subgroup-Female","Physics course score","Yes",
      dv2g(0.1444122, (0.1671230)^2, 531),
      NA, (2.1180-0.3280)/13.09162),
    c("Kost-Smith","2012","No","Majority subgroup-Male","Physics course score","Yes",
      dv2g(0.0374861, (0.1047912)^2, 531),
      NA,NA),
    c("Kost-Smith","2012","No","Minority subgroup-Female","Physics exam score","Yes",
      dv2g(0.11952118, (0.1671043)^2, 531),
      NA, (2.4267-1.4207)/14.12272),
    c("Kost-Smith","2012","No","Majority subgroup-Male","Physics exam score","Yes",
      dv2g(-0.08462973, (0.1048173)^2, 531),
      NA,NA)
  )
  
  return(rbind(unadjusted, adjusted))
  
}



# 31. Lauer 2013 --------------------------------------------------------------
Lauer2013 <- function(){
  
  rbind(
    c("Lauer","2013","No","Main-Main","Introductory biology learning gains","Yes",
      extract_g(esc_B(0.07,sqrt(((131-1) * 15.1^2 + (138-1) * 14.4^2) /(131 + 138 - 2)), 132/2,132/2, es.type = "g")),
      NA,NA),
    c("Lauer","2013","No","Main-Main","Biochemistry learning gains","Yes",
      extract_g(esc_B(0.06,sqrt(((122-1) * 8.8^2 + (97-1) * 12.7^2) /(122 + 97 - 2)), 185/2,185/2, es.type = "g")),
      NA,NA),
    c("Lauer","2013","No","Main-Main","Physics 1 learning gains","Yes",
      extract_g(esc_B(0.25,sqrt(((13-1) * 10.8^2 + (52-1) * 20.7^2) /(13 + 52 - 2)), 44/2,44/2, es.type = "g")),
      NA,NA),
    c("Lauer","2013","No","Main-Main","Physics 2 learning gains","Yes",
      extract_g(esc_B(-0.04,sqrt(((15-1) * 11.1^2 + (111-1) * 15.5^2) /(15 + 111 - 2)), 89/2,89/2, es.type = "g")),
      NA,NA),
    
    c("Lauer","2013","No","Main-Main","Introductory biology learning gains","No",
      extract_g(esc_B(-1.7,sqrt(((131-1) * 15.1^2 + (138-1) * 14.4^2) /(131 + 138 - 2)), 260/2,260/2, es.type = "g")),
      NA,NA),
    c("Lauer","2013","No","Main-Main","Biochemistry learning gains","No",
      extract_g(esc_B(-1.5,sqrt(((122-1) * 8.8^2 + (97-1) * 12.7^2) /(122 + 97 - 2)), 212/2,12/2, es.type = "g")),
      NA,NA),
    c("Lauer","2013","No","Main-Main","Physics 1 learning gains","No",
      extract_g(esc_B(3.96,sqrt(((13-1) * 10.8^2 + (52-1) * 20.7^2) /(13 + 52 - 2)), 65/2,65/2, es.type = "g")),
      NA,NA),
    c("Lauer","2013","No","Main-Main","Physics 2 learning gains","No",
      extract_g(esc_B(-5.44,sqrt(((15-1) * 11.1^2 + (111-1) * 15.5^2) /(15 + 111 - 2)), 124/2,124/2, es.type = "g")),
      NA,NA)
  )
  
}


# 32. Lokhande 2019------------------------------------------------------------

Lokhande2019 <- function(){
  
  rbind(
    #c("Lokhande","2019","No","Main-Main","Wave 1 Math scores","No",
    #  extract_g(esc_mean_sd(5.23, 3.34, 374, 4.86, 3.46, 294, es.type = "g"))),
    #c("Lokhande","2019","No","Main-Main","Wave 2 Math scores","No",
    #  extract_g(esc_mean_sd(6.02, 3.66, 374, 5.42, 3.54, 294, es.type = "g"))),
    
    c("Lokhande","2019","No","Minority subgroup-Female","Wave 1 Math scores","No",
      d2g(0.2, 294*0.488,374*0.488),
      NA,NA),
    c("Lokhande","2019","No","Majority subgroup-Male","Wave 1 Math scores","No",
      d2g(-0.05, 294*(1-0.488),374*(1-0.488)),
      NA,NA),
    c("Lokhande","2019","No","Minority subgroup-Turkish","Wave 1 Math scores","No",
      d2g(0.33, 294*0.198,374*0.198),
      NA,NA),
    c("Lokhande","2019","No","Minority subgroup-Arabic","Wave 1 Math scores","No",
      d2g(0.24, 294*0.135,374*0.135),
      NA,NA),
    c("Lokhande","2019","No","Minority subgroup-Eastern European","Wave 1 Math scores","No",
      d2g(-0.07, 294*0.174,374*0.174),
      NA,NA),
    
    c("Lokhande","2019","No","Interaction-Female","Wave 1 Math scores","Yes",
      extract_g(esc_B(0.63-0.55,3.39,374,294,es.type = "g")),
      NA,1.57/3.39),
    c("Lokhande","2019","No","Interaction-Turkish","Wave 1 Math scores","Yes",
      extract_g(esc_B(1.2-0.55,3.39,374,294,es.type = "g")),
      NA,1.39/3.39),
    c("Lokhande","2019","No","Interaction-Arabic","Wave 1 Math scores","Yes",
      extract_g(esc_B(0.75-0.55,3.39,374,294,es.type = "g")),
      NA,1.01/3.39),
    c("Lokhande","2019","No","Interaction-Eastern European","Wave 1 Math scores","Yes",
      extract_g(esc_B(0.08-0.55,3.39,374,294,es.type = "g")),
      NA,0.19/3.39),
    c("Lokhande","2019","No","Interaction-White","Wave 1 Math scores","Yes",
      extract_g(esc_B(-0.55,3.39,374,294,es.type = "g")),
      NA,NA),
    
    c("Lokhande","2019","No","Minority subgroup-Female","Wave 2 Math scores","No",
      d2g(0.18, 294*0.488,374*0.488),
      NA,NA),
    c("Lokhande","2019","No","Majority subgroup-Male","Wave 2 Math scores","No",
      d2g(0.18, 294*(1-0.488),374*(1-0.488)),
      NA,NA),
    c("Lokhande","2019","No","Minority subgroup-Turkish","Wave 2 Math scores","No",
      d2g(0.35, 294*0.198,374*0.198),
      NA,NA),
    c("Lokhande","2019","No","Minority subgroup-Arabic","Wave 2 Math scores","No",
      d2g(0.35, 294*0.135,374*0.135),
      NA,NA),
    c("Lokhande","2019","No","Minority subgroup-Eastern European","Wave 2 Math scores","No",
      d2g(0.2, 294*0.174,374*0.174),
      NA,NA),
    
    c("Lokhande","2019","No","Interaction-Female","Wave 2 Math scores","Yes",
      extract_g(esc_B(0.27-0.14,3.61,374,294,es.type = "g")),
      NA, 1.83/3.61),
    c("Lokhande","2019","No","Interaction-Turkish","Wave 2 Math scores","Yes",
      extract_g(esc_B(0.97-0.14,3.61,374,294,es.type = "g")),
      NA, 0.83/3.61),
    c("Lokhande","2019","No","Interaction-Arabic","Wave 2 Math scores","Yes",
      extract_g(esc_B(0.78-0.14,3.61,374,294,es.type = "g")),
      NA, 0.29/3.61),
    c("Lokhande","2019","No","Interaction-Eastern European","Wave 2 Math scores","Yes",
      extract_g(esc_B(0.65-0.14,3.61,374,294,es.type = "g")),
      NA, 0.02/3.61),
    c("Lokhande","2019","No","Interaction-White","Wave 2 Math scores","Yes",
      extract_g(esc_B(-0.14,3.61,374,294,es.type = "g")),
      NA,NA)
  )
  
}


# 33. Miyake 2010 ---------------------------------------------------------

Miyake2010 <- function(){
  
  rbind(
    
        c("Miyake","2010","No","Majority subgroup-Male","Biology mean exam score","No",
          extract_g(esc_mean_sd(69.4, 13.2, 178, 72.7, 12.5, 105, es.type = "g")),
          NA,NA),
        c("Miyake","2010","No","Minority subgroup-Female","Biology mean exam score","No",
          extract_g(esc_mean_sd(65.2, 13.8, 69, 62.7, 11.9, 47, es.type = "g")),
          extract_g(esc_mean_sd(72.7, 12.5, 105, 62.7, 11.9, 47, es.type = "g"))[1],NA),
        
        c("Miyake","2010","No","Majority subgroup-Male","Biology final exam score","No",
          extract_g(esc_mean_sd(70.4, 14.2, 178, 73.3, 12.8, 105, es.type = "g")),
          NA,NA),
        c("Miyake","2010","No","Minority subgroup-Female","Biology final exam score","No",
          extract_g(esc_mean_sd(66.7, 15.6, 69, 61.3, 13.6, 47, es.type = "g")),
          extract_g(esc_mean_sd(73.3, 12.8, 105, 61.3, 13.6, 47, es.type = "g"))[1],NA),
        
        c("Miyake","2010","No","Majority subgroup-Male","Biology final course grade","No",
          extract_g(esc_mean_sd(73.9, 10.8, 178, 76.0, 10.5, 105, es.type = "g")),
          NA,NA),
        c("Miyake","2010","No","Minority subgroup-Female","Biology final course grade","No",
          extract_g(esc_mean_sd(70.5, 12.1, 69, 69.3, 9.9, 47, es.type = "g")),
          extract_g(esc_mean_sd(76.0, 10.5, 105, 69.3, 9.9, 47, es.type = "g"))[1],NA),
        
        c("Miyake","2010","No","Majority subgroup-Male","End-of-semester FMCE","No",
          extract_g(esc_mean_sd(72.7, 26.3, 137, 74.7, 27.3, 75, es.type = "g")),
          NA,NA),
        c("Miyake","2010","No","Minority subgroup-Female","End-of-semester FMCE","No",
          extract_g(esc_mean_sd(63.6, 30.6, 55, 56.2, 25.1, 41, es.type = "g")),
          extract_g(esc_mean_sd(74.7, 27.3, 105, 56.2, 25.1, 47, es.type = "g"))[1],NA),
        
        c("Miyake","2010","No","Majority subgroup-Male","Biology final exam score","Yes",
          extract_g(esc_mean_sd(70.4, 12.9, 178, 73.2, 12.8, 105, es.type = "g")),
          NA, extract_g(esc_mean_sd(73.2, 12.8, 105, 60.2, 14.7, 47, es.type = "g"))[1]),
        c("Miyake","2010","No","Minority subgroup-Female","Biology final exam score","Yes",
          extract_g(esc_mean_sd(68.5, 14.9, 69, 60.2, 14.7, 47, es.type = "g")),
          NA,NA),
        c("Miyake","2010","No","Majority subgroup-Male","Biology final course grade","Yes",
          extract_g(esc_mean_sd(73.7, 9.7, 178, 75.7, 9.7, 105, es.type = "g")),
          NA,extract_g(esc_mean_sd(75.7, 9.7, 105, 68.2, 11.1, 47, es.type = "g"))[1]),
        c("Miyake","2010","No","Minority subgroup-Female","Biology final course grade","Yes",
          extract_g(esc_mean_sd(72.3, 11.2, 69, 68.2, 11.1, 47, es.type = "g")),
          NA,NA)
        
        )
  
}


# 34. Peters 2017 ---------------------------------------------------------


Peters2017 <- function(){
  
  rbind(
    
    c("Peters","2017","No","Main","Final grades","No",
      extract_g(esc_mean_sd(80.03, 13.55, 221/2, 80.03, 12.92, 221/2, es.type = "g")),
      NA,NA)
    
  )
  
}


# 35. Powers 2016 ---------------------------------------------------------

Powers2016 <- function(){
  
  rbind(
    
    c("Powers (Study 1)", "2016" , rep(NA,10)),
    c("Powers (Study 2)", "2016" , rep(NA,10))
    
  )
  
}


# 36. Protzko 2016 -------------------------------------------------------

Protzko2016 <- function(){
  
  rbind(
    c("Protzko","2016","No","Majority subgroup-White and Asian","GPA","No",
      extract_g(esc_mean_sd(3.316, 0.586, 124/2, 3.262, 0.603, 124/2, es.type = "g")),
      NA,NA),
    c("Protzko","2016","No","Minority subgroup-Black and Hispanic","GPA","No",
      extract_g(esc_mean_sd(1.991, 1.022, 119/2, 2.051, 1.073, 119/2, es.type = "g")),
      extract_g(esc_mean_sd(3.262, 0.603, 124/2, 2.051, 1.073, 119/2, es.type = "g"))[1],NA),
    
    c("Protzko","2016","No","Interaction-Black and Hispanic","GPA","Yes",
      extract_g(esc_B(-0.07-0.001, sqrt(( (152-1) * 1.043 ^2 + (223-1) * 0.593 ^2  ) / (328-33+134-54-2)), (328-33+134-54)/2, (328-33+134-54)/2, es.type = "g"))),
    c("Protzko","2016","No","Interaction-White","GPA","Yes",
      extract_g(esc_B(-0.001, sqrt(( (152-1) * 1.043 ^2 + (223-1) * 0.593 ^2  ) / (328-33+134-54-2)), (328-33+134-54)/2, (328-33+134-54)/2, es.type = "g")))
      )
  
}


# 37. Purdie-Greenaway under review ---------------------------------------

PurdieGreenawayUR <- function(purdie){
  
  purdie <- purdie[is.na(purdie$Condition) == F,] # 74 in affirmation & control
  purdie$Condition <- ifelse(purdie$Condition == 1, "Affirm", "Control")
  purdie$Gender <- ifelse(purdie$Gender == 1, "Female", "Male")
  purdie$Race <- ifelse(purdie$Race == 1, "White", "URM")
  
  temp1 <- purdie %>% group_by(Gender, Condition) %>% summarise(
    m = mean(gpaq4sixth, na.rm = T),
    sd = sd(gpaq4sixth, na.rm = T),
    .groups = "drop_last"
  )
  
  temp2 <- purdie %>% group_by(Race, Condition) %>% summarise(
    m = mean(gpaq4sixth, na.rm = T),
    sd = sd(gpaq4sixth, na.rm = T),
    .groups = "drop_last"
  )
  
  # unadjusted
  
  unadjusted <- rbind(
    c("Purdie-Greenaway","under review","No","Minority subgroup-Female","GPA","No",
      extract_g(esc_mean_sd(temp1[1,3], temp1[1,4], 21, temp1[2,3], temp1[2,4], 19, es.type = "g"))),
    c("Purdie-Greenaway","under review","No","Majority subgroup-Male","GPA","No",
      extract_g(esc_mean_sd(temp1[3,3], temp1[3,4], 17, temp1[4,3], temp1[4,4], 17, es.type = "g"))),
    c("Purdie-Greenaway","under review","No","Minority subgroup-Black and Hispanic","GPA","No",
      extract_g(esc_mean_sd(temp2[1,3], temp2[1,4], 26, temp2[2,3], temp2[2,4], 26, es.type = "g"))),
    c("Purdie-Greenaway","under review","No","Majority subgroup-White","GPA","No",
      extract_g(esc_mean_sd(temp2[3,3], temp2[3,4], 12, temp2[4,3], temp2[4,4], 10, es.type = "g")))
  )
  
  # adjusted
  
  # manually entering values here because incompatibility between emmeans and drake
  
  #fit <- lm(gpaq4sixth ~ as.factor(Gender) * as.factor(Condition) + Race + CMTPRELEVELMN + gpaq12sixth, purdie)
  #fit.emm <- emmeans(fit, "Condition", "Gender")
  #temp3 <- as.data.frame(eff_size(fit.emm, sigma = sigma(fit), edf = 67))
  #
  #fit <- lm(gpaq4sixth ~ as.factor(Race) * as.factor(Condition) + Gender +CMTPRELEVELMN + gpaq12sixth, purdie)
  #fit.emm <- emmeans(fit, "Condition", "Race")
  #temp4 <- as.data.frame(eff_size(fit.emm, sigma = sigma(fit), edf = 67))
  
  adjusted <- rbind(
    
    c("Purdie-Greenaway","under review","No","Minority subgroup-Female","GPA","Yes",
      dv2g(0.2187767, (0.3269437)^2, 74)),
    c("Purdie-Greenaway","under review","No","Majority subgroup-Male","GPA","Yes",
      dv2g(1.2088276, (0.3594391)^2, 74)),
    c("Purdie-Greenaway","under review","No","Minority subgroup-Black and Hispanic","GPA","Yes",
      dv2g(0.95608926, (0.2951726)^2, 74)),
    c("Purdie-Greenaway","under review","No","Majority subgroup-White","GPA","Yes",
      dv2g(0.01199424, (0.4475243)^2, 74))
    
  )
  
  return(rbind(unadjusted, adjusted))
  
}


# 38. Rapa 2016  ---------------------------------------------------------------------

Rapa2016 <- function(){
  
  rbind(
    c("Rapa","2016","Yes","Main-Main","GPA","No",
      extract_g(esc_mean_sd(3.64, 0.51, 25, 3.33, 0.63, 28, es.type = "g")))
  )
  
}


# 39. Rozek 2015 --------------------------------------------------------------------

Rozek2015 <- function(){
  
  rbind(
    
  c("Rozek", "2015", rep(NA, 8))
  
  ) 
}


# 40. Schwalbe 2018 --------------------------------------------------------------------

Schwalbe2018 <- function(){
  
  rbind(
    #c("Schwalbe","2018","Yes","Main-Main","Pass rates","Yes",
    #  extract_g(esc_B(0.042, 0.4, 2719, 1744, es.type = "g"))),
    c("Schwalbe","2018","Yes","Minority subgroup-Functional skills","Pass rates","Yes",
      extract_g(esc_B(0.041, 0.4, 2719*0.431, 1744*0.426, es.type = "g"))),
    c("Schwalbe","2018","Yes","Minority subgroup-Black Caribbeans","Pass rates","Yes",
      extract_g(esc_B(0.07, 0.4, 2719*0.027, 1744*0.028, es.type = "g"))),
    
    #c("Schwalbe","2018","Yes","Main-Main","Pass rates","No",
    #  extract_g(esc_B(0.041, 0.4, 2719, 1744, es.type = "g"))),
    c("Schwalbe","2018","Yes","Minority subgroup-Functional skills","Pass rates","No",
      extract_g(esc_B(0.038, 0.4, 2719*0.431, 1744*0.426, es.type = "g"))),
    c("Schwalbe","2018","Yes","Minority subgroup-Black Caribbeans","Pass rates","No",
      extract_g(esc_B(0.071, 0.4, 2719*0.027, 1744*0.028, es.type = "g"))),
    
    #c("Schwalbe","2018","Yes","Main-Main","Attendance","Yes",
    #  extract_g(esc_B(0.023, 0.39, 2459, 2042, es.type = "g"))),
    c("Schwalbe","2018","Yes","Minority subgroup-Functional skills","Attendance","Yes",
      extract_g(esc_B(0.044, 0.39, 2459*0.431, 2042*0.426, es.type = "g"))),
    c("Schwalbe","2018","Yes","Minority subgroup-Black Caribbeans","Attendance","Yes",
      extract_g(esc_B(0.160, 0.39, 2459*0.027, 2042*0.028, es.type = "g"))),
    
   #c("Schwalbe","2018","Yes","Main-Main","Attendance","No",
   #  extract_g(esc_B(0.018, 0.39, 2459, 2042, es.type = "g"))),
    c("Schwalbe","2018","Yes","Minority subgroup-Functional skills","Attendance","No",
      extract_g(esc_B(0.047, 0.39, 2459*0.431, 2042*0.426, es.type = "g"))),
    c("Schwalbe","2018","Yes","Minority subgroup-Black Caribbeans","Attendance","No",
      extract_g(esc_B(0.188, 0.39, 2459*0.027, 2042*0.028, es.type = "g")))
  )
  
}


# 41. Serra-Garcia under review --------------------------------------------------------------------

SerraGarciaUR <- function(){
  
  rbind(
    
    # Study 1 reanalyzed Miyake 2013
    c("Serra-Garcia (Study 1)", "under review", rep(NA, 8)),
    
    # Study 2 is an independent replication study
    
    c("Serra-Garcia (Study 2)","under review","No","Minority subgroup-Female","Physics quiz grade","No",
      extract_g(esc_mean_sd(66.95299, 16.23619, 39, 63.25759, 16.39845, 22, es.type = "g"))),
    c("Serra-Garcia (Study 2)","under review","No","Minority subgroup-Female","Physics final exam score","No",
      extract_g(esc_mean_sd(69.38462, 19.58331, 39, 75.27273, 21.20238, 22, es.type = "g"))),
    c("Serra-Garcia (Study 2)","under review","No","Minority subgroup-Female","Physics final grade","No",
      extract_g(esc_mean_sd(68.53846, 15.03128, 39, 68.63636, 14.8566, 22, es.type = "g"))),
    
    c("Serra-Garcia (Study 2)","under review","No","Majority subgroup-Male","Physics quiz grade","No",
      extract_g(esc_mean_sd(70.71377, 15.95548, 46, 76.64394, 12.67724, 22, es.type = "g"))),
    c("Serra-Garcia (Study 2)","under review","No","Majority subgroup-Male","Physics final exam score","No",
      extract_g(esc_mean_sd(78.41304, 16.42299, 46, 73.40909, 18.50266, 22, es.type = "g"))),
    c("Serra-Garcia (Study 2)","under review","No","Majority subgroup-Male","Physics final grade","No",
      extract_g(esc_mean_sd(73.84783, 14.1625, 46, 75.68182, 12.16312, 22, es.type = "g")))
    
  )
  
}


# 42. Sherman 2013 --------------------------------------------------------------------

Sherman2013 <- function(){
  
  rbind(
    c("Sherman (Study 1)","2013","No","Minority subgroup-Hispanic","Year 1 GPA","No",
      extract_g(esc_mean_se(2.62,0.06,41,2.4,0.06,40, es.type = "g"))),
    c("Sherman (Study 1)","2013","No","Majority subgroup-White","Year 1 GPA","No",
      extract_g(esc_mean_se(3.4,0.05,51,3.44,0.05,52, es.type = "g"))),
    
    c("Sherman (Study 1)","2013","No","Minority subgroup-Hispanic","Year 2 GPA","No",
      extract_g(esc_mean_se(2.09,0.09,41,1.7,0.09,40, es.type = "g"))),
    c("Sherman (Study 1)","2013","No","Majority subgroup-White","Year 2 GPA","No",
      extract_g(esc_mean_se(3.11,0.08,51,3.09,0.08,52, es.type = "g"))),
    
    c("Sherman (Study 1)","2013","No","Minority subgroup-Hispanic","Year 3 GPA","No",
      extract_g(esc_mean_se(1.92,0.1,41,1.67,0.11,40, es.type = "g"))),
    c("Sherman (Study 1)","2013","No","Majority subgroup-White","Year 3 GPA","No",
      extract_g(esc_mean_se(3.12,0.1,51,3.27,0.09,52, es.type = "g"))),
    
    c("Sherman (Study 1)","2013","No","Interaction-Hispanic","Year 1 GPA","Yes",
      d2g(0.29,92,92)),
    c("Sherman (Study 1)","2013","No","Interaction-Hispanic","Year 2 GPA","Yes",
      d2g(0.43,92,92)),
    c("Sherman (Study 1)","2013","No","Interaction-Hispanic","Year 3 GPA","Yes",
      d2g(0.24,92,92)),
    
    c("Sherman (Study 2)","2013","No","Minority subgroup-Hispanic","GPA","No",
      extract_g(esc_mean_se(2.84,0.12,26,2.46,0.11,29, es.type = "g"))),
    c("Sherman (Study 2)","2013","No","Majority subgroup-White","GPA","No",
      extract_g(esc_mean_se(3.58,0.09,46,3.69,0.09,50, es.type = "g"))),
    
    c("Sherman (Study 2)","2013","No","Interaction-Hispanic","GPA","Yes",
      d2g(0.45,75.5,75.5)),
    c("Sherman (Study 2)","2013","No","Interaction-White","GPA","Yes",
      d2g(0.13,75.5,75.5))
  )
  
}



# 43. Shnabel 2013 --------------------------------------------------------------------

Shnabel2013 <- function(){
  
  # uses Cohen 2006 & 2009
  rbind(
    
    c("Shnabel (Study 1)", "2013", rep(NA,8))
    
  )
  
}



# 44. Silverman 2014 ------------------------------------------------------

Silverman2014 <- function(){
  
  rbind(
    
    c("Silverman (Study 2)","2014","No","Minority subgroup-Blind","Instructor-graded student progress","Yes",
      d2g(0.64,35/2,35/2))
    
  )
  
}

# 45. Simmons 2011 ------------------------------------------------------
Simmons2011 <- function(){
  rbind(
    
    c("Simmons","2011","No","Minority subgroup-Black","Fall zscored GPA","No",
      extract_g(esc_mean_sd(-0.11, 1.02, 24, 0.13, 0.94, 23, es.type = "g"))),
    c("Simmons","2011","No","Minority subgroup-Black","Winter zscored GPA","No",
      extract_g(esc_mean_sd(-0.07, 0.87, 24, 0.17, 1.01, 23, es.type = "g")))  
    
  )
  
}
# 46. Tibbetts 2016 -------------------------------------------------------

Tibbetts2016 <- function(){
  
  rbind(
    c("Tibbetts (Study 1a)","2016","No","Minority subgroup-First generation","S1 Semester Grade","No",
      extract_g(esc_mean_sd(3.11, 0.5, 72, 3.07, 0.52, 69, es.type = "g"))),
    c("Tibbetts (Study 1a)","2016","No","Minority subgroup-First generation","S2 Semester Grade","No",
      extract_g(esc_mean_sd(3.15, 0.6, 72, 2.99, 0.74, 69, es.type = "g"))),
    c("Tibbetts (Study 1a)","2016","No","Minority subgroup-First generation","S3 Semester Grade","No",
      extract_g(esc_mean_sd(3.27, 0.53, 72, 3.04, 0.7, 69, es.type = "g"))),
    
    c("Tibbetts (Study 1a)","2016","No","Majority subgroup-Continuing generation","S1 Semester Grade","No",
      extract_g(esc_mean_sd(3.21, 0.58, 304, 3.24, 0.5, 304, es.type = "g"))),
    c("Tibbetts (Study 1a)","2016","No","Majority subgroup-Continuing generation","S2 Semester Grade","No",
      extract_g(esc_mean_sd(3.27, 0.64, 304, 3.28, 0.59, 304, es.type = "g"))),
    c("Tibbetts (Study 1a)","2016","No","Majority subgroup-Continuing generation","S3 Semester Grade","No",
      extract_g(esc_mean_sd(3.35, 0.56, 304, 3.37, 0.56, 304, es.type = "g"))),
    
    c("Tibbetts (Study 1a)","2016","No","Minority subgroup-First generation","GPA","No",
      extract_g(esc_mean_sd(3.16, 0.58, 76, 2.98, 0.7, 74, es.type = "g"))),
    c("Tibbetts (Study 1a)","2016","No","Majority subgroup-Continuing generation","GPA","No",
      extract_g(esc_mean_sd(3.25, 0.57, 323, 3.28, 0.49, 315, es.type = "g"))),
    c("Tibbetts (Study 1a)","2016","No","Interaction-First generation","GPA","Yes",
      extract_g(esc_beta(0.09+0.08, sqrt(((399-1) * 0.54 ^2 + (389-1) * 0.65 ^2) / (399 + 389- 2)), 399, 389, es.type = "g"))),
    c("Tibbetts (Study 1a)","2016","No","Interaction-Continuing generation","GPA","Yes",
      extract_g(esc_beta(0.08, sqrt(((399-1) * 0.54 ^2 + (389-1) * 0.65 ^2) / (399 + 389- 2)), 399, 389, es.type = "g")))
  )
  
}


# 47. Tibbetts 2018 --------------------------------------------------------------------


Tibbetts2018 <- function(){
  
  rbind(
    
    c("Tibbetts (Study 1b)","2018","Yes","Minority subgroup-First generation","Course Grade","No",
      extract_g(esc_mean_sd(2.52, 1, 438/2, 2.33, 1.24, 438/4, es.type = "g"))),
    c("Tibbetts (Study 1b)","2018","Yes","Majority subgroup-Continuting generation","Course Grade","No",
      extract_g(esc_mean_sd(2.47, 1.21, 438/2, 2.63, 1.14, 438/4, es.type = "g"))),
    
    c("Tibbetts (Study 1b)","2018","No","Minority subgroup-First generation","Course Grade","No",
      extract_g(esc_mean_sd(2.18, 0.74, 438/4, 1.25, 1.24, 438/4, es.type = "g"))),
    c("Tibbetts (Study 1b)","2018","No","Majority subgroup-Continuting generation","Course Grade","No",
      extract_g(esc_mean_sd(2.34, 1.1, 438/4, 2.54, 1.15, 438/4, es.type = "g")))
    
  )
  
}


# 48. Turetsky under review -----------------------------------------------

TuretskyUR <- function(turetsky){

  temp1 <- turetsky %>% group_by(RaceMarg, Intervention) %>% summarise(
    m_course_score = mean(CoursePointTotal, na.rm = T),
    sd_course_score = sd(CoursePointTotal, na.rm = T),
    m_continuation = mean(TookSpringClass, na.rm = T),
    sd_continuation = sd(TookSpringClass, na.rm = T),
    .groups = "drop_last"
  )
  
  temp2 <- turetsky %>% group_by(Gender, Intervention) %>% summarise(
    m_course_score = mean(CoursePointTotal, na.rm = T),
    sd_course_score = sd(CoursePointTotal, na.rm = T),
    m_continuation = mean(TookSpringClass, na.rm = T),
    sd_continuation = sd(TookSpringClass, na.rm = T),
    .groups = "drop_last"
  )

  
  rbind(
    
    c("Turetsky","under review","No","Minority subgroup-URM","Course point total","No",
      extract_g(esc_mean_sd(temp1[2,3], temp1[2,4], 40, temp1[1,3], temp1[1,4], 42, es.type = "g"))),
    c("Turetsky","under review","No","Majority subgroup-White and Asian","Course point total","No",
      extract_g(esc_mean_sd(temp1[4,3], temp1[4,4], 105, temp1[3,3], temp1[3,4], 103, es.type = "g"))),
    
    c("Turetsky","under review","No","Minority subgroup-Female","Course point total","No",
      extract_g(esc_mean_sd(temp2[2,3], temp2[2,4], 96, temp2[1,3], temp2[1,4], 96, es.type = "g"))),
    c("Turetsky","under review","No","Majority subgroup-Male","Course point total","No",
      extract_g(esc_mean_sd(temp2[4,3], temp2[4,4], 46, temp2[3,3], temp2[3,4], 48, es.type = "g"))),
    
    c("Turetsky","under review","No","Minority subgroup-URM","Biology track persistence","No",
      extract_g(esc_mean_sd(temp1[2,5], temp1[2,6], 40, temp1[1,5], temp1[1,6], 42, es.type = "g"))),
    c("Turetsky","under review","No","Majority subgroup-White and Asian","Biology track persistence","No",
      extract_g(esc_mean_sd(temp1[4,5], temp1[4,6], 105, temp1[3,5], temp1[3,6], 103, es.type = "g"))),
    c("Turetsky","under review","No","Minority subgroup-Female","Biology track persistence","No",
      extract_g(esc_mean_sd(temp2[2,5], temp2[2,6], 96, temp2[1,5], temp2[1,6], 96, es.type = "g"))),
    c("Turetsky","under review","No","Majority subgroup-Male","Biology track persistence","No",
      extract_g(esc_mean_sd(temp2[4,5], temp2[4,6], 46, temp2[3,5], temp2[3,6], 48, es.type = "g")))
    
    
  )
  
}


# 49. Woolf 2009 ----------------------------------------------------------

Woolf2009 <- function(){
  
  rbind(
    
    c("Woolf","2009","No","Majority subgroup-White","Summative written essay score","Yes",
      extract_g(esc_mean_sd(0.063, 0.9, 79, 0.244, 1, 84, es.type = "g"))),
    c("Woolf","2009","No","Minority subgroup-URM","Summative written essay score","Yes",
      extract_g(esc_mean_sd(-0.098, 1.09, 95, -0.175, 0.96, 77, es.type = "g"))),
    c("Woolf","2009","No","Majority subgroup-White","OSCE score","Yes",
      extract_g(esc_mean_sd(0.271, 0.96, 79, -0.002, 0.96, 84, es.type = "g"))),
    c("Woolf","2009","No","Minority subgroup-URM","OSCE score","Yes",
      extract_g(esc_mean_sd(0.001, 1, 95, -0.286, 0.97, 77, es.type = "g")))

  )
  
}


# 50. Wynne 2011---------------------------------------------------------------


Wynne2011 <- function(){
  
  rbind(
    c("Wynne","2011","No","Minority subgroup-Black and Hispanic","Time 1 GPA in History","No",
      extract_g(esc_mean_sd(80.63, 7.20, 100/2, 78.60, 6.77, 100/2, es.type = "g"))),
    c("Wynne","2011","No","Minority subgroup-Black and Hispanic","Time 2 GPA in History","No",
      extract_g(esc_mean_sd(79.43, 9.23, 100/2, 78.40, 7.16, 100/2, es.type = "g"))),
    c("Wynne","2011","No","Minority subgroup-Black","Time 1 GPA in History","No",
      extract_g(esc_mean_sd(80.23, 6.91, 71/2, 78.39, 6.46, 71/2, es.type = "g"))),
    c("Wynne","2011","No","Minority subgroup-Black","Time 2 GPA in History","No",
      extract_g(esc_mean_sd(78.89, 8.66, 71/2, 78.39, 7.21, 71/2, es.type = "g"))),
    c("Wynne","2011","No","Minority subgroup-Hispanic","Time 1 GPA in History","No",
      extract_g(esc_mean_sd(81.69, 8.11, 29/2, 79.06, 7.61, 29/2, es.type = "g"))),
    c("Wynne","2011","No","Minority subgroup-Hispanic","Time 2 GPA in History","No",
      extract_g(esc_mean_sd(80.92, 10.87, 29/2, 78.44, 7.28, 29/2, es.type = "g"))),
    
    #c("Wynne","2011","No","Main-Main","Time 1 GPA Overall","No",
    #  extract_g(esc_mean_sd(79.81, 7.17, 100/2, 78.09, 6.52, 100/2, es.type = "g"))),
    #c("Wynne","2011","No","Main-Main","Time 2 GPA in Overall","No",
    #  extract_g(esc_mean_sd(77.92, 8.48, 100/2, 76.52, 6.94, 100/2, es.type = "g"))),
    
    c("Wynne","2011","No","Minority subgroup-Black","Time 1 GPA in Overall","No",
      extract_g(esc_mean_sd(79.49, 7.07, 71/2, 77.85, 6.52, 71/2, es.type = "g"))),
    c("Wynne","2011","No","Minority subgroup-Black","Time 2 GPA in Overall","No",
      extract_g(esc_mean_sd(77.53, 8.53, 71/2, 76.08, 7.24, 71/2, es.type = "g"))),
    c("Wynne","2011","No","Minority subgroup-Hispanic","Time 1 GPA in Overall","No",
      extract_g(esc_mean_sd(80.66, 7.65, 29/2, 78.64, 6.68, 29/2, es.type = "g"))),
    c("Wynne","2011","No","Minority subgroup-Hispanic","Time 2 GPA in Overall","No",
      extract_g(esc_mean_sd(78.97, 8.57, 29/2, 77.51, 6.34, 29/2, es.type = "g"))),
    
    #c("Wynne","2011","No","Main-Main","MCAS ELA scores","No",
    #  extract_g(esc_mean_sd(249.5, 11.29, 100/2, 246.8, 11.27, 100/2, es.type = "g"))),
    c("Wynne","2011","No","Minority subgroup-Black","MCAS ELA scores","No",
      extract_g(esc_mean_sd(248.7, 11.35, 71/2, 247.3, 11.39, 71/2, es.type = "g"))),
    c("Wynne","2011","No","Minority subgroup-Hispanic","MCAS ELA scores","No",
      extract_g(esc_mean_sd(251.8, 11.21, 29/2, 245.6, 11.25, 29/2, es.type = "g"))),
    
    #c("Wynne","2011","No","Main-Main","MCAS Math scores","No",
    #  extract_g(esc_mean_sd(249.3, 12.14, 100/2, 246.0, 12.37, 100/2, es.type = "g"))),
    c("Wynne","2011","No","Minority subgroup-Black","MCAS Math scores","No",
      extract_g(esc_mean_sd(249.9, 13.34, 71/2, 245.8, 12.33, 71/2, es.type = "g"))),
    c("Wynne","2011","No","Minority subgroup-Hispanic","MCAS Math scores","No",
      extract_g(esc_mean_sd(247.5, 8.29, 29/2, 246.5, 12.87, 29/2, es.type = "g")))
  )
  
}








