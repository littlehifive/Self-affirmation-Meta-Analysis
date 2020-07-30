# calculation script

#  dat <- data.frame(study = NA, author = NA, year = NA, adapted = NA, type = NA,  outcome = NA, adjusted = NA, es = NA, v = NA, lowerCI = NA, upperCI = NA)

# 1. Baker 2019 --------------------------------------------------------------

Baker2019 <- function(){
  
  rbind(
    
    c("Baker", "2019", "No", "Interaction-Black", "GPA", "No", 
      extract_g(esc_B(0.315+0.003, 1.27, 564, 551,es.type = "g"))),
    
    c("Baker", "2019", "No", "Interaction-Hispanic", "GPA", "No", 
      extract_g(esc_B(-0.02+0.003, 1.27, 564, 551,es.type = "g"))),    
    
    c("Baker", "2019", "No", "Interaction-Asian", "GPA", "No", 
      extract_g(esc_B(0.167+0.003, 1.27, 564, 551,es.type = "g"))),   
    
    c("Baker", "2019", "No", "Interaction-Female", "GPA", "No", 
      extract_g(esc_B(-0.114+0.003, 1.27, 564, 551,es.type = "g"))),   
    
    c("Baker", "2019", "No", "Interaction-White", "GPA", "No", 
      extract_g(esc_B(0.003, 1.27, 564, 551,es.type = "g"))), 
    
    c("Baker", "2019", "No", "Interaction-White", "GPA", "No", 
      extract_g(esc_B(0.003, 1.27, 564, 551,es.type = "g"))),     
  
    
    c("Baker", "2019", "No", "Main", "GPA", "No", 
      extract_g(esc_mean_sd(2.30, 1.30, 564, 2.32, 1.23, 541, es.type = "g"))),
    
    c("Baker", "2019", "No", "Main", "Amount of time until withdraw from class", "No", 
      extract_g(esc_mean_sd(67.32, 24.98, 564, 65.23, 24.57, 541, es.type = "g")))

    )
  
  
}


# 2. Bancroft 2017 --------------------------------------------------------------

Bancroft2017 <- function(){
  
  c("Bancroft", "2017", rep(NA,8))
  
}

# 3. Bayly 2017 --------------------------------------------------------------

Bayly2017 <- function(){
  
  rbind(
    
  # VA+CA vs. control
  c("Bayly", "2017", "Yes", "Minority subgroup-URM or First generation", "GPA", "No", 
    extract_g(esc_mean_sd(2.72, 0.95, 191, 2.88, 0.84, 197, es.type = "g"))),
  
  # VA vs. control
  c("Bayly", "2017", "No", "Minority subgroup-URM or First generation", "GPA", "No", 
    extract_g(esc_mean_sd(2.8, 0.94, 198, 2.88, 0.84, 197, es.type = "g"))),
  
  # URM (adjusted)
  c("Bayly", "2017", "No", "Minority subgroup-URM", "GPA", "Yes", 
    extract_g(esc_B(-0.05, sqrt(((198-1) * 0.94^2 + (197-1) * 0.84^2) /(198 + 197 - 2)), 198 * (1-0.084-0.176) , 197 * (1-0.084-0.176), es.type = "g"))),
  
  # First generation (adjusted)
  c("Bayly", "2017", "No", "Minority subgroup-First generation", "GPA", "Yes", 
    extract_g(esc_B(-0.02, sqrt(((198-1) * 0.94^2 + (197-1) * 0.84^2) /(198 + 197 - 2)), 198 * 0.56 ,  197 * 0.56, es.type = "g")))
  
  )
}


# 4. Binning under review --------------------------------------------------------------

BinningUR <- function(){
  
  rbind(
    
    # use longitudinal model coefficients as adjusted coefficients
    c("Binning", "under review", "No", "Minority subgroup-URM", "GPA", "Yes", 
      extract_g(esc_B(0.77-0.43, sqrt((0.86^2 + 0.98^2 + 0.99^2)/3), 163/2, 163/2, es.type = "g")))

  )
}

# 5. Borman 2012 --------------------------------------------------------------

Borman2012 <- function(){
  
  rbind(
    # Table 6 Outcome is reported in standard deviations of 2012 test score, so it is technically an effect size already
    
    # MCA Reading & Math scores
    
    # Minority subgroup
    c("Borman","2012","No","Minority subgroup-Black","MCA reading score","Yes",
      dv2g(0.023, 0.049^2, 401-1)),
    c("Borman","2012","No","Minority subgroup-Black","MCA math score","Yes",
      dv2g(0.073, 0.054^2, 403-1)),
    c("Borman","2012","No","Minority subgroup-Hispanic","MCA reading score","Yes",
      dv2g(0.055, 0.084^2, 187-1)),
    c("Borman","2012","No","Minority subgroup-Hispanic","MCA math score","Yes",
      dv2g(0.053, 0.074^2, 186-1)),
    c("Borman","2012","No","Minority subgroup-Female","MCA reading score","Yes",
      dv2g(0.019, 0.039^2, 651-1)),
    c("Borman","2012","No","Minority subgroup-Female","MCA math score","Yes",
      dv2g(0.104, 0.035^2, 654-1)),
    # Majority subgroup
    c("Borman","2012","No","Majority subgroup-Asian","MCA reading score","Yes",
      dv2g(-0.066, 0.050^2, 341-1)),
    c("Borman","2012","No","Majority subgroup-Asian","MCA math score","Yes",
      dv2g(-0.075, 0.046^2, 342-1)),
    c("Borman","2012","No","Majority subgroup-Male","MCA reading score","Yes",
      dv2g(-0.022, 0.042^2, 667-1)),
    c("Borman","2012","No","Majority subgroup-Male","MCA math score","Yes",
      dv2g(-0.005, 0.039^2, 668-1))
    
  )
  
}

# 6. Borman 2015 --------------------------------------------------------------

Borman2015 <- function(){
  
  # leave empty to avoid double count with Borman 2018
  c("Borman", "2015", rep(NA,8))
  
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

# 8. Borman 2018 ----------------------------------------------------------

Borman2018 <- function(){
  
  # different outcomes from Borman 2016
  rbind(
    
  c("Borman","2018","No","Interaction-URM","7th-9th GPA","Yes",
    extract_g(esc_B(0.177,1.02,920/2,920/2, es.type = "g"))),
  c("Borman","2018","No","Interaction-White and Asian","7th-9th GPA","Yes",
    extract_g(esc_B(0.036,1.02,920/2,920/2, es.type = "g"))),
  c("Borman","2018","No","Minority subgroup-URM","7th-9th GPA","Yes",
    extract_g(esc_B(0.206,1.02,324/2,324/2, es.type = "g")))
  
  )
}


# 9. Bowen 2013 --------------------------------------------------------------

Bowen2013 <- function(){
  
  rbind(
    c("Bowen","2013","No","Minority subgroup-Black","Social Studies grades","Yes",
      d2g(0.57 / (1- 3/ (4*(58+74)-1)), 58,74))
  )
}


# 10. Bratter 2016 --------------------------------------------------------

Bratter2016 <- function(){
  rbind(
    
    c("Bratter","2016","No","Minority subgroup-Black","Semester English grades","No",
      extract_g(esc_B(2.48, sqrt(((430*0.26-1) * 11.78^2 + (456*0.25-1) * 12.46^2)/(430*0.26+456*0.25-2)),430*0.26,456*0.25, es.type = "g"))),
    c("Bratter","2016","No","Minority subgroup-Black","STAAR Reading","No",
      extract_g(esc_B(-22.34, sqrt(((430*0.26-1) * 228.73^2 + (456*0.25-1) * 248.34^2)/(430*0.26+456*0.25-2)),430*0.26,456*0.25, es.type = "g"))),
    c("Bratter","2016","No","Minority subgroup-Black","STAAR Algebra","No",
      extract_g(esc_B(-84.7, sqrt(((430*0.26-1) * 429.01^2 + (456*0.25-1) * 420.19^2)/(430*0.26+456*0.25-2)),430*0.26,456*0.25, es.type = "g"))),
    c("Bratter","2016","No","Minority subgroup-Hispanic","Semester English grades","No",
      extract_g(esc_B(-0.28, sqrt(((430*0.63-1) * 11.78^2 + (456*0.61-1) * 12.46^2)/(430*0.63+456*0.61-2)),430*0.63,456*0.61, es.type = "g"))),
    c("Bratter","2016","No","Minority subgroup-Hispanic","STAAR Reading","No",
      extract_g(esc_B(-21.51, sqrt(((430*0.63-1) * 228.73^2 + (456*0.61-1) * 248.34^2)/(430*0.63+456*0.61-2)),430*0.63,456*0.61, es.type = "g"))),
    c("Bratter","2016","No","Minority subgroup-Hispanic","STAAR Algebra","No",
      extract_g(esc_B(-9.15, sqrt(((430*0.63-1) * 429.01^2 + (456*0.61-1) * 420.19^2)/(430*0.63+456*0.61-2)),430*0.63,456*0.61, es.type = "g")))
    
  )
  
}


# 11. Churchill 2018 ------------------------------------------------------

Churchill2018 <- function(){
  
  rbind(
    c("Churchill","2018","No","Main","Music examination grade","No",
      extract_g(esc_mean_sd(62.18, 5.63, 35, 60.97, 5.98, 32, es.type = "g")))
    
  )
  
  
}



# 12. Cohen 2006 ----------------------------------------------------------

Cohen2006 <- function(){
  
  # some calculations are commented out in order not to double count different analysis using the same data
  rbind(
  c("Cohen (Study 1)","2006","No","Minority subgroup-Black","GPA in targeted course","Yes",
    extract_g(esc_B(0.26, 1, 50, 50,es.type = "g"))),
  #c("Cohen (Study 1)","2006","No","Interaction-Black","GPA in targeted course","Yes",
  #  extract_g(esc_B(0.29, 1, 55.5, 55.5,es.type = "g"))),
  c("Cohen (Study 1)","2006","No","Interaction-White","GPA in targeted course","Yes",
    extract_g(esc_B(0.09, 1, 55.5, 55.5,es.type = "g"))),
  c("Cohen (Study 1)","2006","No","Minority subgroup-Black","GPA in non-targeted course","Yes",
    extract_g(esc_B(0.31, 1, 25, 25,es.type = "g"))),
  #c("Cohen (Study 1)","2006","No","Interaction-Black","GPA in non-targeted course","Yes",
  #  extract_g(esc_B(0.45, 1, 55.5, 55.5,es.type = "g"))),
  c("Cohen (Study 2)","2006","No","Minority subgroup-Black","GPA in targeted course","Yes",
    extract_g(esc_B(0.34, 1, 34.5, 34.5,es.type = "g"))),
  #c("Cohen (Study 2)","2006","No","Interaction-Black","GPA in targeted course","Yes",
  ## extract_g(esc_B(0.52, 1, 66, 66,es.type = "g"))),
  c("Cohen (Study 2)","2006","No","Interaction-White","GPA in targeted course","Yes",
    extract_g(esc_B(0.03, 1, 66, 66,es.type = "g"))),
  c("Cohen (Study 2)","2006","No","Minority subgroup-Black","GPA in non-targeted course","Yes",
    extract_g(esc_B(0.21, 1, 34.5, 34.5,es.type = "g")))
  
  )
}


# 13. Cohen 2009 ----------------------------------------------------------

Cohen2009 <- function(){
  # some calculations are commented out in order not to double count different analysis using the same data
  rbind(
  c("Cohen","2009","No","Minority subgroup-Black","GPA","Yes",
    extract_g(esc_B(0.24, 1, 87.5, 87.5,es.type = "g"))),
  c("Cohen","2009","No","Majority subgroup-White","GPA","Yes",
    extract_g(esc_B(-0.07, 1, 94, 94,es.type = "g")))
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
  c("Cook", "2012", rep(NA,8))
  )
}



# 15. De Clercq 2019 ------------------------------------------------------

DeClercq2019 <- function(){
  
  rbind(
  # Author shared data
  c("De Clercq","2019","No","Main","Test score","No",
    extract_g(esc_mean_sd(2.664, 1.5642, 125, 2.737, 1.5112, 112, es.type = "g"))),
  c("De Clercq","2019","No","Main","Study time","No",
    extract_g(esc_mean_sd(3.6488, 1.09978, 84, 3.7821, 1.09482, 78, es.type = "g")))
  )
}


# 16. de Jong 2016 --------------------------------------------------------

deJong2016 <- function(){
  
  rbind(
  c("de Jong (Study 1)","2016","No","Minority subgroup-URM","Dutch","No",
    extract_g(esc_mean_sd(6.68, 0.78, 144, 6.76, 0.83, 133, es.type = "g"))),
  c("de Jong (Study 1)","2016","No","Minority subgroup-URM","English","No",
    extract_g(esc_mean_sd(6.96, 1.05, 144, 6.95, 1.28, 133, es.type = "g"))),
  c("de Jong (Study 1)","2016","No","Minority subgroup-URM","Mathematics","No",
    extract_g(esc_mean_sd(6.55, 1.12, 144, 6.63, 1.01, 133, es.type = "g"))),
  
  c("de Jong (Study 1)","2016","No","Majority subgroup-URM","Dutch","No",
    extract_g(esc_mean_sd(6.94, 0.95, 28, 7.09, 0.93, 31, es.type = "g"))),
  c("de Jong (Study 1)","2016","No","Majority subgroup-URM","English","No",
    extract_g(esc_mean_sd(7, 1.22, 28, 6.9, 1.29, 31, es.type = "g"))),
  c("de Jong (Study 1)","2016","No","Majority subgroup-URM","Mathematics","No",
    extract_g(esc_mean_sd(6.93, 1.16, 28, 7.07, 1.1, 31, es.type = "g"))),
  
  c("de Jong (Study 2)","2016","Yes","Minority subgroup-URM","Cito scores","No",
    extract_g(esc_mean_sd(530.21, 11.24, 88, 530.43, 11.71, 84, es.type = "g"))),
  c("de Jong (Study 2)","2016","No","Minority subgroup-URM","Cito scores","No",
    extract_g(esc_mean_sd(529.80, 10.26, 94, 530.43, 11.71, 84, es.type = "g"))),
  c("de Jong (Study 2)","2016","Yes","Majority subgroup-URM","Cito scores","No",
    extract_g(esc_mean_sd(530.50, 11.36, 6, 531.33, 8.08, 3, es.type = "g"))),
  c("de Jong (Study 2)","2016","No","Majority subgroup-URM","Cito scores","No",
    extract_g(esc_mean_sd(527.33, 5.69, 3, 531.33, 8.08, 3, es.type = "g")))
  )
  
  
}



# 17. Dee 2015 ----------------------------------------------------------------

Dee2015 <- function(){
  
  rbind(
    c("Dee","2015","No","Interaction-Black","Final grades in the treated subject","No",
      extract_g(esc_B(0.228, 10.8, 2348/2, 2348/2,es.type = "g"))),
    c("Dee","2015","No","Interaction-Hispanic","Final grades in the treated subject","No",
      extract_g(esc_B(0.582, 10.8, 2348/2, 2348/2,es.type = "g"))),
    c("Dee","2015","No","Interaction-Female","Final grades in the treated subject","No",
      extract_g(esc_B(-0.28, 10.8, 2348/2, 2348/2,es.type = "g"))),
    c("Dee","2015","No","Interaction-White","Final grades in the treated subject","No",
      extract_g(esc_B(-0.028, 10.8, 2348/2, 2348/2,es.type = "g"))),
    c("Dee","2015","No","Interaction-Black","Final grades in the treated subject","Yes",
      extract_g(esc_B(-0.009, 10.8, 2348/2, 2348/2,es.type = "g"))),
    c("Dee","2015","No","Interaction-Hispanic","Final grades in the treated subject","Yes",
      extract_g(esc_B(0.591, 10.8, 2348/2, 2348/2,es.type = "g"))),
    c("Dee","2015","No","Interaction-Female","Final grades in the treated subject","Yes",
      extract_g(esc_B(-0.431, 10.8, 2348/2, 2348/2,es.type = "g"))),
    c("Dee","2015","No","Interaction-White","Final grades in the treated subject","Yes",
      extract_g(esc_B(0.063, 10.8, 2348/2, 2348/2,es.type = "g")))
  )
  
}


# 18. Goyer 2017 --------------------------------------------------------------

Goyer2017 <- function(){
  
  rbind(
    # Study 1 uses Sherman 2013, the only Course Difficulty Scores were kept to avoid double counting
    
    c("Goyer (Study 1)","2017","No","Interaction-Hispanic","Course Difficulty Scores","No",
      d2g(0.42, 93, 92)),
    c("Goyer (Study 1)","2017","No","Interaction-White","Course Difficulty Scores","No",
      d2g(-0.11, 93, 92)),
    # c("Goyer (Study 1)","2017","No","Minority subgroup-Hispanic","Year 1 GPA","No",
    #   d2g(0.22, 41, 40)),
    # c("Goyer (Study 1)","2017","No","Minority subgroup-Black","Year 2 GPA","No",
    #   d2g(0.22, 41, 40)),
    c("Goyer (Study 1)","2017","No","Interaction-Hispanic","Course Difficulty Scores","Yes",
      d2g(0.48, 93, 92)),
    c("Goyer (Study 1)","2017","No","Interaction-White","Course Difficulty Scores","Yes",
      d2g(-0.22, 93, 92)),
    c("Goyer (Study 1)","2017","No","Interaction-Hispanic","Course Difficulty Scores","Yes",
      d2g(0.48, 93, 92)),
    
    # Study 2 uses Cohen 2006, 2009, no need to double count
    c("Goyer (Study 2)","2017", rep(NA,8))
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
    c("Gutmann","2019","No","Minority subgroup-Female","Physics 100 final grades","No",
      dv2g(-0.2212, 0.0273, 75+73)),
    c("Gutmann","2019","No","Majority subgroup-Male","Physics 100 final grades","No",
      dv2g(-0.328, 0.0162, 121+120)),
    c("Gutmann","2019","No","Minority subgroup-Female","Physics 212 final grades","No",
      dv2g(-0.1159, 0.0152, 143+122)),
    c("Gutmann","2019","No","Majority subgroup-Male","Physics 212 final grades","No",
      dv2g(0.0852, 0.0054, 366+381))
  )
  
}


# 20. Hadden 2019 -------------------------------------------------------------------

Hadden2019 <- function(){
  
  rbind(
    c("Hadden","2019","No","Minority subgroup-FSM","GCSE Mathematics","No",
      extract_g(esc_mean_se(-0.084,(-0.084+0.303)/1.96,69,-0.438,(-0.438+0.588)/1.96,59,es.type = "g"))),
    c("Hadden","2019","No","Majority subgroup-nonFSM","GCSE Mathematics","No",
      extract_g(esc_mean_se(0.088,(0.088+0.039)/1.96,224,0.048,(0.048+0.073)/1.96,210,es.type = "g"))),
    c("Hadden","2019","No","Minority subgroup-FSM","GCSE Mathematics","Yes",
      extract_g(esc_mean_se(-0.099,(-0.099+0.280)/1.96,69,-0.478,(-0.478+0.645)/1.96,59,es.type = "g"))),
    c("Hadden","2019","No","Majority subgroup-nonFSM","GCSE Mathematics","Yes",
      extract_g(esc_mean_se(0.1,(0.1+0.037)/1.96,224,0.049,(0.049+0.071)/1.96,210,es.type = "g")))
  )
  
}


# 21. Hanselman 2014 ------------------------------------------------------

Hanselman2014 <- function(){
  
  # Similar to Borman 2016, but they reported multilevel model with different sets of covariates
  # drop the results for now because the reviewer suggested that this is reanalysis of the same dataset
  rbind(
    #c("Hanselman","2014","No","Minority subgroup-URM","GPA","Yes",
    #  extract_g(esc_B(0.068,0.68,310/2,310/2,es.type = "g"))),
    #c("Hanselman","2014","No","Majority subgroup-White","GPA","Yes",
    #  extract_g(esc_B(-0.012,0.68,600/2,600/2,es.type = "g")))
    
    c("Hanselman","2014", rep(NA, 8))
    
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
    dv2g(0.101,	0.07^2, 331)),
  #c("Hanselman (Study 1)","2017","No","Minority subgroup-Black and Hispanic","WKCE Reading, Grade 7","Yes",
  # dv2g(-0.034,	0.069^2, 331)),
  c("Hanselman (Study 1)","2017","No","Minority subgroup-Black and Hispanic","WKCE Reading, Grade 8","Yes",
    dv2g(-0.03,	0.071^2, 331)),
  
  # Study 2 refers to Cohort 2
  c("Hanselman (Study 2)","2017","No","Minority subgroup-Black and Hispanic","7th GPA","Yes",
    dv2g(-0.002, 0.042^2, 449)),
  c("Hanselman (Study 2)","2017","No","Minority subgroup-Black and Hispanic","8th GPA","Yes",
    dv2g(-0.072, 0.058^2, 449)),
  c("Hanselman (Study 2)","2017","No","Minority subgroup-Black and Hispanic","WKCE Mathematcis, Grade 7","Yes",
    dv2g(-0.085,	0.047^2, 449)),
  c("Hanselman (Study 2)","2017","No","Minority subgroup-Black and Hispanic","WKCE Mathematcis, Grade 8","Yes",
    dv2g(-0.08,	0.044^2, 449)),
  c("Hanselman (Study 2)","2017","No","Minority subgroup-Black and Hispanic","WKCE Reading, Grade 7","Yes",
    dv2g(-0.005,	0.055^2, 449)),
  c("Hanselman (Study 2)","2017","No","Minority subgroup-Black and Hispanic","WKCE Reading, Grade 8","Yes",
    dv2g(-0.005,	0.056^2, 449))
  ) 
}

# 23. Harackiewicz 2014 ---------------------------------------------------

Harackiewicz2014<- function(){
  
  rbind(
    
    c("Harackiewicz","2014","No","Majority subgroup-Continuing generation","Biology Course Grade","No",
      extract_g(esc_mean_sd(2.82, 0.69, 325, 2.86, 0.69, 319, es.type = "g"))),
    c("Harackiewicz","2014","No","Majority subgroup-Continuing generation","GPA","No",
      extract_g(esc_mean_sd(3.17, 0.62, 325, 3.2, 0.63, 319, es.type = "g"))),
    c("Harackiewicz","2014","No","Majority subgroup-Continuing generation","Continuation","No",
      extract_g(esc_bin_prop(0.748,325,0.777,319, es.type = "g"))),
    
    c("Harackiewicz","2014","No","Minority subgroup-First generation","Biology Course Grade","No",
      extract_g(esc_mean_sd(2.62, 0.78, 77, 2.38, 0.85, 77, es.type = "g"))),
    c("Harackiewicz","2014","No","Minority subgroup-First generation","GPA","No",
      extract_g(esc_mean_sd(3.05, 0.64, 77, 2.81, 0.81, 77, es.type = "g"))),
    c("Harackiewicz","2014","No","Minority subgroup-First generation","Continuation","No",
      extract_g(esc_bin_prop(0.857,77,0.662,77, es.type = "g"))),
    
    c("Harackiewicz","2014","No","Interaction-First generation","Biology Course Grade","Yes",
      extract_g(esc_beta(0.09, 0.73, 325+77, 319+77, es.type = "g"))),
    c("Harackiewicz","2014","No","Interaction-First generation","GPA","Yes",
      extract_g(esc_beta(0.1, 0.65, 325+77, 319+77, es.type = "g"))),
    c("Harackiewicz","2014","No","Interaction-First generation","Continuation","Yes",
      extract_g(esc_chisq(8.8, 0.01, 798, es.type = "g")))

  )
  
}


# 24. Harackiewicz 2016 ---------------------------------------------------

# ignoring the UV condition

Harackiewicz2016 <- function(){
  
  rbind(
    
    c("Harackiewicz","2016","No","Interaction-URM","Biology course grade","Yes",
      extract_g(esc_B(2*(-0.01) + 2*(-0.02), 0.81, 1040/4, 1040/4, es.type = "g"))),
    c("Harackiewicz","2016","No","Interaction-White and Asian","Biology course grade","Yes",
      extract_g(esc_B(2*(-0.01) - 2*(-0.02), 0.81, 1040/4, 1040/4, es.type = "g"))),
    c("Harackiewicz","2016","No","Interaction-First generation","Biology course grade","Yes",
      extract_g(esc_B(2*(-0.01) + 2*(-0.05), 0.81, 1040/4, 1040/4, es.type = "g"))),
    c("Harackiewicz","2016","No","Interaction-Continuing generation","Biology course grade","Yes",
      extract_g(esc_B(2*(-0.01) - 2*(-0.05), 0.81, 1040/4, 1040/4, es.type = "g")))
    
  )
  
}


# 25. Hayes 2019 ----------------------------------------------------------

Hayes2019 <- function(){
  
  rbind(
    
    # Study 1: almost entire sample is Latino
    # there are three conditions, one of which is not relevant
    c("Hayes (Study 1)","2019","No","Minority subgroup-Hispanic","Semester Grade","No",
      extract_g(esc_mean_sd(84.21, 4.97, 116/3, 85.77, 5.45, 116/3, es.type = "g"))),
    
    # Study 2: # only 1st year student in the sample
    c("Hayes (Study 2)","2019","No","Minority subgroup-First generation","GPA","No",
      extract_g(esc_mean_se(2.86,0.1,39,2.93,0.1,39,es.type = "g")))
  )
  
}


# 26. Jordt 2017 ----------------------------------------------------------

Jordt2017 <- function(){
  
  rbind(
    c("Jordt","2017","No","Interaction-Female","Exam grades","Yes",
      extract_g(esc_B(-8.1+6.45, 278.3*0.042*2, 963, 970, es.type = "g"))),
    c("Jordt","2017","No","Interaction-Male","Exam grades","Yes",
      extract_g(esc_B(6.45, 278.3*0.042*2, 963, 970, es.type = "g"))),
    c("Jordt","2017","No","Interaction-URM","Exam grades","Yes",
      extract_g(esc_B(10.29+6.45, 278.3*0.042*2, 963, 970, es.type = "g"))),
    c("Jordt","2017","No","Interaction-White","Exam grades","Yes",
      extract_g(esc_B(6.45, 278.3*0.042*2, 963, 970, es.type = "g")))
  )
  
}


# 27. Kim 2019 ------------------------------------------------------------

Kim2019 <- function(){
  
  rbind(
    c("Kim","2019","Yes","Interaction-Female","GPA","Yes",
      f2g(3.24, 110, 110))
  )
  
}


# 28. Kinias 2016 ---------------------------------------------------------

Kinias2016 <- function(){
  
  rbind(
    c("Kinias (Study 2)","2016","No","Majority subgroup-Male","MBA core course grade","No",
      extract_g(esc_mean_sd(3.109, 0.688, 266/2, 3.205, 0.694, 266/2, es.type = "g"))),
    c("Kinias (Study 2)","2016","No","Minority subgroup-Female","MBA core course grade","No",
      extract_g(esc_mean_sd(2.948, 0.654, 130/2, 2.739, 0.763, 130/2, es.type = "g"))),
    c("Kinias (Study 2)","2016","No","Interaction-Female","MBA core course grade","Yes",
      extract_g(esc_B(2*0.028 + 2*0.076, sqrt(( (198-1) * 0.694^2 + (198-1) * 0.763^2 ) / (198 + 198 - 2)), 198, 198, es.type = "g"))),
    c("Kinias (Study 2)","2016","No","Interaction-Male","MBA core course grade","Yes",
      extract_g(esc_B(2*0.028 - 2*0.076, sqrt(( (198-1) * 0.694^2 + (198-1) * 0.763^2 ) / (198 + 198 - 2)), 198, 198, es.type = "g")))
  )
  
}


# 29. Kost-Smith 2010 -----------------------------------------------------

Kostsmith2010 <- function(){
  
  # the adjusted effect sizes on FMCE grades are not reported in Miyake 2013
  
  # using Miyake 2010 to get pooled SD
  sd_ks <- sqrt(((137-1) * 26.3^2 + (55-1) * 30.6^2 + (75-1) * 27.3^2 + (41-1) * 25.1^2) /(137 + 55 + 75 + 41 - 4))
  
  rbind(
    c("Kost-Smith","2010","No","Minority subgroup-Female","FMCE grade","Yes",
      extract_g(esc_B(12.9-0.6, sd_ks, 55,41, es.type = "g"))),
    c("Kost-Smith","2010","No","Minority subgroup-Male","FMCE grade","Yes",
      extract_g(esc_B(-0.6, sd_ks, 137,75, es.type = "g")))
  )
  
}

# 30. Kost-Smith 2012 -----------------------------------------------------

Kostsmith2012 <- function(kost2012){
  
  # Cohort2
  kost2012 <- kost[kost$Cohort == "Spring 2011",]
  
  temp <- kost2012 %>% group_by(FEMALE, AFFIRM) %>% summarise(
    m_course_grade = mean(COURSE_GRADE, na.rm = T),
    sd_course_grade = sd(COURSE_GRADE, na.rm = T),
    m_course_score = mean(COURSE_SCORE, na.rm = T),
    sd_course_score = sd(COURSE_SCORE, na.rm = T),
    m_exam_score = mean(RAW_ALL_EXAMS, na.rm = T),
    sd_exam_score = sd(RAW_ALL_EXAMS, na.rm = T),
    .groups = "drop_last"
  )
  
  # unadjusted
  unadjusted <- rbind(
  c("Kost-Smith","2012","No","Minority subgroup-Female","Physics course grade","No",
    extract_g(esc_mean_sd(temp[4,3], temp[4,4], 89, temp[3,3], temp[3,4], 60, es.type = "g"))),
  c("Kost-Smith","2012","No","Majority subgroup-Male","Physics course grade","No",
    extract_g(esc_mean_sd(temp[2,3], temp[2,4], 232, temp[1,3], temp[1,4], 150, es.type = "g"))),
  
  c("Kost-Smith","2012","No","Minority subgroup-Female","Physics course score","No",
    extract_g(esc_mean_sd(temp[4,5], temp[4,6], 89, temp[3,5], temp[3,6], 60, es.type = "g"))),
  c("Kost-Smith","2012","No","Majority subgroup-Male","Physics course score","No",
    extract_g(esc_mean_sd(temp[2,5], temp[2,6], 232, temp[1,5], temp[1,6], 150, es.type = "g"))),
  
  c("Kost-Smith","2012","No","Minority subgroup-Female","Physics exam score","No",
    extract_g(esc_mean_sd(temp[4,7], temp[4,8], 89, temp[3,7], temp[3,8], 60, es.type = "g"))),
  c("Kost-Smith","2012","No","Majority subgroup-Male","Physics exam score","No",
    extract_g(esc_mean_sd(temp[2,7], temp[2,8], 232, temp[1,7], temp[1,8], 150, es.type = "g")))
  )
  
  # adjusted
  kost2012$AFFIRM <- ifelse(kost2012$AFFIRM == 1, "Affirm", "Control")
  kost2012$FEMALE <- ifelse(kost2012$FEMALE == 1, "Female", "Male")
  
  # COURSE_GRADE
  fit <- lm(COURSE_GRADE ~ as.factor(FEMALE) * as.factor(AFFIRM) + MATH, kost2012)
  fit.emm <- emmeans(fit, "AFFIRM", "FEMALE")
  temp1 <- as.data.frame(eff_size(fit.emm, sigma = sigma(fit), edf = 526))
  
  # COURSE_SCORE
  fit <- lm(COURSE_SCORE ~ as.factor(FEMALE) * as.factor(AFFIRM) + MATH, kost2012)
  fit.emm <- emmeans(fit, "AFFIRM", "FEMALE")
  temp2 <- as.data.frame(eff_size(fit.emm, sigma = sigma(fit), edf = 526))
  
  #RAW_ALL_EXAMS
  fit <- lm(RAW_ALL_EXAMS ~ as.factor(FEMALE) * as.factor(AFFIRM) + MATH, kost2012)
  fit.emm <- emmeans(fit, "AFFIRM", "FEMALE")
  temp3 <- as.data.frame(eff_size(fit.emm, sigma = sigma(fit), edf = 526))
  
  adjusted <- rbind(
  c("Kost-Smith","2012","No","Minority subgroup-Female","Physics course grade","Yes",
    getgv(temp1[1,3], (temp1[1,4])^2, 531)),
  c("Kost-Smith","2012","No","Majority subgroup-Male","Physics course grade","Yes",
    getgv(temp1[2,3], (temp1[2,4])^2, 531)),
  c("Kost-Smith","2012","No","Minority subgroup-Female","Physics course score","Yes",
    getgv(temp2[1,3], (temp2[1,4])^2, 531)),
  c("Kost-Smith","2012","No","Majority subgroup-Male","Physics course score","Yes",
    getgv(temp2[2,3], (temp2[2,4])^2, 531)),
  c("Kost-Smith","2012","No","Minority subgroup-Female","Physics exam score","Yes",
    getgv(temp3[1,3], (temp3[1,4])^2, 531)),
  c("Kost-Smith","2012","No","Majority subgroup-Male","Physics exam score","Yes",
    getgv(temp3[2,3], (temp3[2,4])^2, 531))
  )
  
  return(rbind(unadjusted, adjusted))
  
}



# 31. Lauer 2013 --------------------------------------------------------------
Lauer2013 <- function(){
  
  rbind(
    c("Lauer","2013","No","Main-Main","Introductory biology learning gains","Yes",
      extract_g(esc_B(0.07,sqrt(((131-1) * 15.1^2 + (138-1) * 14.4^2) /(131 + 138 - 2)), 132/2,132/2, es.type = "g"))),
    c("Lauer","2013","No","Main-Main","Biochemistry learning gains","Yes",
      extract_g(esc_B(0.06,sqrt(((122-1) * 8.8^2 + (97-1) * 12.7^2) /(122 + 97 - 2)), 185/2,185/2, es.type = "g"))
    ),
    c("Lauer","2013","No","Main-Main","Physics 1 learning gains","Yes",
      extract_g(esc_B(0.25,sqrt(((13-1) * 10.8^2 + (52-1) * 20.7^2) /(13 + 52 - 2)), 44/2,44/2, es.type = "g"))),
    c("Lauer","2013","No","Main-Main","Physics 2 learning gains","Yes",
      extract_g(esc_B(-0.04,sqrt(((15-1) * 11.1^2 + (111-1) * 15.5^2) /(15 + 111 - 2)), 89/2,89/2, es.type = "g"))),
    
    c("Lauer","2013","No","Main-Main","Introductory biology learning gains","No",
      extract_g(esc_B(-1.7,sqrt(((131-1) * 15.1^2 + (138-1) * 14.4^2) /(131 + 138 - 2)), 260/2,260/2, es.type = "g"))),
    c("Lauer","2013","No","Main-Main","Biochemistry learning gains","No",
      extract_g(esc_B(-1.5,sqrt(((122-1) * 8.8^2 + (97-1) * 12.7^2) /(122 + 97 - 2)), 212/2,12/2, es.type = "g"))),
    c("Lauer","2013","No","Main-Main","Physics 1 learning gains","No",
      extract_g(esc_B(3.96,sqrt(((13-1) * 10.8^2 + (52-1) * 20.7^2) /(13 + 52 - 2)), 65/2,65/2, es.type = "g"))),
    c("Lauer","2013","No","Main-Main","Physics 2 learning gains","No",
      extract_g(esc_B(-5.44,sqrt(((15-1) * 11.1^2 + (111-1) * 15.5^2) /(15 + 111 - 2)), 124/2,124/2, es.type = "g")))
  )
  
}


# 32. Lokhande 2019------------------------------------------------------------

Lokhande2019 <- function(){
  
  rbind(
    c("Lokhande","2019","No","Main-Main","Wave 1 Math scores","No",
      extract_g(esc_mean_sd(5.23, 3.34, 374, 4.86, 3.46, 294, es.type = "g"))),
    c("Lokhande","2019","No","Main-Main","Wave 2 Math scores","No",
      extract_g(esc_mean_sd(6.02, 3.66, 374, 5.42, 3.54, 294, es.type = "g"))),
    
    c("Lokhande","2019","No","Minority subgroup-Female","Wave 1 Math scores","No",
      d2g(0.2, 294*0.488,374*0.488)),
    c("Lokhande","2019","No","Majority subgroup-Male","Wave 1 Math scores","No",
      d2g(-0.05, 294*(1-0.488),374*(1-0.488))),
    c("Lokhande","2019","No","Minority subgroup-Turkish","Wave 1 Math scores","No",
      d2g(0.33, 294*0.198,374*0.198)),
    c("Lokhande","2019","No","Minority subgroup-Arabic","Wave 1 Math scores","No",
      d2g(0.24, 294*0.135,374*0.135)),
    c("Lokhande","2019","No","Minority subgroup-Eastern European","Wave 1 Math scores","No",
      d2g(-0.07, 294*0.174,374*0.174)),
    
    c("Lokhande","2019","No","Interaction-Female","Wave 1 Math scores","Yes",
      extract_g(esc_B(0.63-0.55,3.39,374,294,es.type = "g"))),
    c("Lokhande","2019","No","Interaction-Turkish","Wave 1 Math scores","Yes",
      extract_g(esc_B(1.2-0.55,3.39,374,294,es.type = "g"))),
    c("Lokhande","2019","No","Interaction-Arabic","Wave 1 Math scores","Yes",
      extract_g(esc_B(0.75-0.55,3.39,374,294,es.type = "g"))),
    c("Lokhande","2019","No","Interaction-Eastern European","Wave 1 Math scores","Yes",
      extract_g(esc_B(0.08-0.55,3.39,374,294,es.type = "g"))),
    c("Lokhande","2019","No","Interaction-White","Wave 1 Math scores","Yes",
      extract_g(esc_B(-0.55,3.39,374,294,es.type = "g"))),
    
    c("Lokhande","2019","No","Minority subgroup-Female","Wave 2 Math scores","No",
      d2g(0.18, 294*0.488,374*0.488)),
    c("Lokhande","2019","No","Majority subgroup-Male","Wave 2 Math scores","No",
      d2g(0.18, 294*(1-0.488),374*(1-0.488))),
    c("Lokhande","2019","No","Minority subgroup-Turkish","Wave 2 Math scores","No",
      d2g(0.35, 294*0.198,374*0.198)),
    c("Lokhande","2019","No","Minority subgroup-Arabic","Wave 2 Math scores","No",
      d2g(0.35, 294*0.135,374*0.135)),
    c("Lokhande","2019","No","Minority subgroup-Eastern European","Wave 2 Math scores","No",
      d2g(0.2, 294*0.174,374*0.174)),
    
    c("Lokhande","2019","No","Interaction-Female","Wave 2 Math scores","Yes",
      extract_g(esc_B(0.27-0.14,3.61,374,294,es.type = "g"))),
    c("Lokhande","2019","No","Interaction-Turkish","Wave 2 Math scores","Yes",
      extract_g(esc_B(0.97-0.14,3.61,374,294,es.type = "g"))),
    c("Lokhande","2019","No","Interaction-Arabic","Wave 2 Math scores","Yes",
      extract_g(esc_B(0.78-0.14,3.61,374,294,es.type = "g"))),
    c("Lokhande","2019","No","Interaction-Eastern European","Wave 2 Math scores","Yes",
      extract_g(esc_B(0.65-0.14,3.61,374,294,es.type = "g"))),
    c("Lokhande","2019","No","Interaction-White","Wave 2 Math scores","Yes",
      extract_g(esc_B(-0.14,3.61,374,294,es.type = "g")))
  )
  
}


# 33. Miyake 2010 ---------------------------------------------------------

Miyake2010 <- function(){
  
  rbind(
    
        c("Miyake","2010","No","Majority subgroup-Male","Biology mean exam score","No",
          extract_g(esc_mean_sd(69.4, 13.2, 178, 72.7, 12.5, 105, es.type = "g"))),
        c("Miyake","2010","No","Minority subgroup-Female","Biology mean exam score","No",
          extract_g(esc_mean_sd(65.2, 13.8, 69, 62.7, 11.9, 47, es.type = "g"))),
        c("Miyake","2010","No","Majority subgroup-Male","Biology final exam score","No",
          extract_g(esc_mean_sd(70.4, 14.2, 178, 73.3, 12.8, 105, es.type = "g"))),
        c("Miyake","2010","No","Minority subgroup-Female","Biology final exam score","No",
          extract_g(esc_mean_sd(66.7, 15.6, 69, 61.3, 13.6, 47, es.type = "g"))),
        c("Miyake","2010","No","Majority subgroup-Male","Biology final course grade","No",
          extract_g(esc_mean_sd(73.9, 10.8, 178, 76.0, 10.5, 105, es.type = "g"))),
        c("Miyake","2010","No","Minority subgroup-Female","Biology final course grade","No",
          extract_g(esc_mean_sd(70.5, 12.1, 69, 69.3, 9.9, 47, es.type = "g"))),
        c("Miyake","2010","No","Majority subgroup-Male","End-of-semester FMCE","No",
          extract_g(esc_mean_sd(72.7, 26.3, 137, 74.7, 27.3, 75, es.type = "g"))),
        c("Miyake","2010","No","Minority subgroup-Female","End-of-semester FMCE","No",
          extract_g(esc_mean_sd(63.6, 30.6, 55, 56.2, 25.1, 41, es.type = "g"))),
        
        c("Miyake","2010","No","Majority subgroup-Male","Biology final exam score","Yes",
          extract_g(esc_mean_sd(70.4, 12.9, 178, 73.2, 12.8, 105, es.type = "g"))),
        c("Miyake","2010","No","Minority subgroup-Female","Biology final exam score","Yes",
          extract_g(esc_mean_sd(68.5, 14.9, 69, 60.2, 14.7, 47, es.type = "g"))),
        c("Miyake","2010","No","Majority subgroup-Male","Biology final course grade","Yes",
          extract_g(esc_mean_sd(73.7, 9.7, 178, 75.7, 9.7, 105, es.type = "g"))),
        c("Miyake","2010","No","Minority subgroup-Female","Biology final course grade","Yes",
          extract_g(esc_mean_sd(72.3, 11.2, 69, 68.2, 11.1, 47, es.type = "g")))
        
        )
  
}


# 34. Peters 2017 ---------------------------------------------------------


Peters2017 <- function(){
  
  rbind(
    
    c("Peters","2017","No","Main","Final grades","No",
      extract_g(esc_mean_sd(80.03, 13.55, 221/2, 80.03, 12.92, 221/2, es.type = "g")))
    
  )
  
}


# 35. Powers 2016 ---------------------------------------------------------

Powers2016 <- function(){
  
  rbind(
    
    c("Powers (Study 1)", "2016" , rep(NA,8)),
    c("Powers (Study 2)", "2016" , rep(NA,8))
    
  )
  
}





