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
  
  c("Bancroft", "2017", rep(NA,8))
  
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
    getB(esc_B(0.177,1.02,920/2,920/2, es.type = "g"))),
  c("Borman","2018","No","Interaction-White and Asian","7th-9th GPA","Yes",
    getB(esc_B(0.036,1.02,920/2,920/2, es.type = "g"))),
  c("Borman","2018","No","Minority subgroup-URM","7th-9th GPA","Yes",
    getB(esc_B(0.206,1.02,324/2,324/2, es.type = "g")))
  
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
      getB(esc_B(2.48, sqrt(((430*0.26-1) * 11.78^2 + (456*0.25-1) * 12.46^2)/(430*0.26+456*0.25-2)),430*0.26,456*0.25, es.type = "g"))),
    c("Bratter","2016","No","Minority subgroup-Black","STAAR Reading","No",
      getB(esc_B(-22.34, sqrt(((430*0.26-1) * 228.73^2 + (456*0.25-1) * 248.34^2)/(430*0.26+456*0.25-2)),430*0.26,456*0.25, es.type = "g"))),
    c("Bratter","2016","No","Minority subgroup-Black","STAAR Algebra","No",
      getB(esc_B(-84.7, sqrt(((430*0.26-1) * 429.01^2 + (456*0.25-1) * 420.19^2)/(430*0.26+456*0.25-2)),430*0.26,456*0.25, es.type = "g"))),
    c("Bratter","2016","No","Minority subgroup-Hispanic","Semester English grades","No",
      getB(esc_B(-0.28, sqrt(((430*0.63-1) * 11.78^2 + (456*0.61-1) * 12.46^2)/(430*0.63+456*0.61-2)),430*0.63,456*0.61, es.type = "g"))),
    c("Bratter","2016","No","Minority subgroup-Hispanic","STAAR Reading","No",
      getB(esc_B(-21.51, sqrt(((430*0.63-1) * 228.73^2 + (456*0.61-1) * 248.34^2)/(430*0.63+456*0.61-2)),430*0.63,456*0.61, es.type = "g"))),
    c("Bratter","2016","No","Minority subgroup-Hispanic","STAAR Algebra","No",
      getB(esc_B(-9.15, sqrt(((430*0.63-1) * 429.01^2 + (456*0.61-1) * 420.19^2)/(430*0.63+456*0.61-2)),430*0.63,456*0.61, es.type = "g")))
    
  )
  
}


# 11. Churchill 2018 ------------------------------------------------------

Churchill2018 <- function(){
  
  rbind(
    c("Churchill","2018","No","Main","Music examination grade","No",
      getB(esc_mean_sd(62.18, 5.63, 35, 60.97, 5.98, 32, es.type = "g")))
    
  )
  
  
}



# 12. Cohen 2006 ----------------------------------------------------------

Cohen2006 <- function(){
  
  # some calculations are commented out in order not to double count different analysis using the same data
  rbind(
  c("Cohen (Study 1)","2006","No","Minority subgroup-Black","GPA in targeted course","Yes",
    getB(esc_B(0.26, 1, 50, 50,es.type = "g"))),
  #c("Cohen (Study 1)","2006","No","Interaction-Black","GPA in targeted course","Yes",
  #  getB(esc_B(0.29, 1, 55.5, 55.5,es.type = "g"))),
  c("Cohen (Study 1)","2006","No","Interaction-White","GPA in targeted course","Yes",
    getB(esc_B(0.09, 1, 55.5, 55.5,es.type = "g"))),
  c("Cohen (Study 1)","2006","No","Minority subgroup-Black","GPA in non-targeted course","Yes",
    getB(esc_B(0.31, 1, 25, 25,es.type = "g"))),
  #c("Cohen (Study 1)","2006","No","Interaction-Black","GPA in non-targeted course","Yes",
  #  getB(esc_B(0.45, 1, 55.5, 55.5,es.type = "g"))),
  c("Cohen (Study 2)","2006","No","Minority subgroup-Black","GPA in targeted course","Yes",
    getB(esc_B(0.34, 1, 34.5, 34.5,es.type = "g"))),
  #c("Cohen (Study 2)","2006","No","Interaction-Black","GPA in targeted course","Yes",
  ## getB(esc_B(0.52, 1, 66, 66,es.type = "g"))),
  c("Cohen (Study 2)","2006","No","Interaction-White","GPA in targeted course","Yes",
    getB(esc_B(0.03, 1, 66, 66,es.type = "g"))),
  c("Cohen (Study 2)","2006","No","Minority subgroup-Black","GPA in non-targeted course","Yes",
    getB(esc_B(0.21, 1, 34.5, 34.5,es.type = "g")))
  
  )
}


# 13. Cohen 2009 ----------------------------------------------------------

Cohen2009 <- function(){
  # some calculations are commented out in order not to double count different analysis using the same data
  rbind(
  c("Cohen","2009","No","Minority subgroup-Black","GPA","Yes",
    getB(esc_B(0.24, 1, 87.5, 87.5,es.type = "g"))),
  c("Cohen","2009","No","Majority subgroup-White","GPA","Yes",
    getB(esc_B(-0.07, 1, 94, 94,es.type = "g")))
  #c("Cohen","2009","No","Interaction-Black","GPA","Yes",
  #  getB(esc_B(0.33, 1, 181.5, 181.5,es.type = "g")))
  #c("Cohen","2009","No","Interaction-White","GPA","Yes",
  #  getB(esc_B(-0.02, 1, 181.5, 181.5,es.type = "g")))
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
    getB(esc_mean_sd(2.664, 1.5642, 125, 2.737, 1.5112, 112, es.type = "g"))),
  c("De Clercq","2019","No","Main","Study time","No",
    getB(esc_mean_sd(3.6488, 1.09978, 84, 3.7821, 1.09482, 78, es.type = "g")))
  )
}


# 16. de Jong 2016 --------------------------------------------------------

deJong2016 <- function(){
  
  rbind(
  c("de Jong (Study 1)","2016","No","Minority subgroup-URM","Dutch","No",
    getB(esc_mean_sd(6.68, 0.78, 144, 6.76, 0.83, 133, es.type = "g"))),
  c("de Jong (Study 1)","2016","No","Minority subgroup-URM","English","No",
    getB(esc_mean_sd(6.96, 1.05, 144, 6.95, 1.28, 133, es.type = "g"))),
  c("de Jong (Study 1)","2016","No","Minority subgroup-URM","Mathematics","No",
    getB(esc_mean_sd(6.55, 1.12, 144, 6.63, 1.01, 133, es.type = "g"))),
  
  c("de Jong (Study 1)","2016","No","Majority subgroup-URM","Dutch","No",
    getB(esc_mean_sd(6.94, 0.95, 28, 7.09, 0.93, 31, es.type = "g"))),
  c("de Jong (Study 1)","2016","No","Majority subgroup-URM","English","No",
    getB(esc_mean_sd(7, 1.22, 28, 6.9, 1.29, 31, es.type = "g"))),
  c("de Jong (Study 1)","2016","No","Majority subgroup-URM","Mathematics","No",
    getB(esc_mean_sd(6.93, 1.16, 28, 7.07, 1.1, 31, es.type = "g"))),
  
  c("de Jong (Study 2)","2016","Yes","Minority subgroup-URM","Cito scores","No",
    getB(esc_mean_sd(530.21, 11.24, 88, 530.43, 11.71, 84, es.type = "g"))),
  c("de Jong (Study 2)","2016","No","Minority subgroup-URM","Cito scores","No",
    getB(esc_mean_sd(529.80, 10.26, 94, 530.43, 11.71, 84, es.type = "g"))),
  c("de Jong (Study 2)","2016","Yes","Majority subgroup-URM","Cito scores","No",
    getB(esc_mean_sd(530.50, 11.36, 6, 531.33, 8.08, 3, es.type = "g"))),
  c("de Jong (Study 2)","2016","No","Majority subgroup-URM","Cito scores","No",
    getB(esc_mean_sd(527.33, 5.69, 3, 531.33, 8.08, 3, es.type = "g")))
  )
  
  
}



# 17. Dee 2015 ----------------------------------------------------------------

Dee2015 <- function(){
  
  rbind(
    c("Dee","2015","No","Interaction-Black","Final grades in the treated subject","No",
      getB(esc_B(0.228, 10.8, 2348/2, 2348/2,es.type = "g"))),
    c("Dee","2015","No","Interaction-Hispanic","Final grades in the treated subject","No",
      getB(esc_B(0.582, 10.8, 2348/2, 2348/2,es.type = "g"))),
    c("Dee","2015","No","Interaction-Female","Final grades in the treated subject","No",
      getB(esc_B(-0.28, 10.8, 2348/2, 2348/2,es.type = "g"))),
    c("Dee","2015","No","Interaction-White","Final grades in the treated subject","No",
      getB(esc_B(-0.028, 10.8, 2348/2, 2348/2,es.type = "g"))),
    c("Dee","2015","No","Interaction-Black","Final grades in the treated subject","Yes",
      getB(esc_B(-0.009, 10.8, 2348/2, 2348/2,es.type = "g"))),
    c("Dee","2015","No","Interaction-Hispanic","Final grades in the treated subject","Yes",
      getB(esc_B(0.591, 10.8, 2348/2, 2348/2,es.type = "g"))),
    c("Dee","2015","No","Interaction-Female","Final grades in the treated subject","Yes",
      getB(esc_B(-0.431, 10.8, 2348/2, 2348/2,es.type = "g"))),
    c("Dee","2015","No","Interaction-White","Final grades in the treated subject","Yes",
      getB(esc_B(0.063, 10.8, 2348/2, 2348/2,es.type = "g")))
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
    #  getB(esc_mean_se(0.668485523, 0.687193764-0.668485523, 75, 0.673385301, 0.687639198-0.673385301, 73, es.type = "g"))),
    #c("Gutmann","2019","No","Majority subgroup-Male","Physics 100 midterm exam score","No",
    #  getB(esc_mean_se(0.672494432, 0.691648107-0.672494432, 121, 0.71481069, 0.729064588-0.71481069, 120, es.type = "g"))),
    #c("Gutmann","2019","No","Minority subgroup-Female","Physics 212 hour exam 1 score","No",
    #  getB(esc_mean_se(0.680530973, 0.696570796-0.680530973, 143, 0.710951327, 0.720353982-0.710951327, 122, es.type = "g"))),
    #c("Gutmann","2019","No","Majority subgroup-Male","Physics 212 hour exam 1 score","No",
    #  getB(esc_mean_se(0.748561947, 0.765707965-0.748561947, 366, 0.734734513, 0.743584071-0.734734513, 381, es.type = "g")),
    c("Gutmann","2019","No","Minority subgroup-Female","Physics 100 final grades","No",
      getgv(-0.2212, 0.0273, 75+73)),
    c("Gutmann","2019","No","Majority subgroup-Male","Physics 100 final grades","No",
      getgv(-0.328, 0.0162, 121+120)),
    c("Gutmann","2019","No","Minority subgroup-Female","Physics 212 final grades","No",
      getgv(-0.1159, 0.0152, 143+122)),
    c("Gutmann","2019","No","Majority subgroup-Male","Physics 212 final grades","No",
      getgv(0.0852, 0.0054, 366+381))
  )
  
}


# 20. Hadden 2019 -------------------------------------------------------------------

Hadden2019 <- function(){
  
  rbind(
    c("Hadden","2019","No","Minority subgroup-FSM","GCSE Mathematics","No",
      getB(esc_mean_se(-0.084,(-0.084+0.303)/1.96,69,-0.438,(-0.438+0.588)/1.96,59,es.type = "g"))),
    c("Hadden","2019","No","Majority subgroup-nonFSM","GCSE Mathematics","No",
      getB(esc_mean_se(0.088,(0.088+0.039)/1.96,224,0.048,(0.048+0.073)/1.96,210,es.type = "g"))),
    c("Hadden","2019","No","Minority subgroup-FSM","GCSE Mathematics","Yes",
      getB(esc_mean_se(-0.099,(-0.099+0.280)/1.96,69,-0.478,(-0.478+0.645)/1.96,59,es.type = "g"))),
    c("Hadden","2019","No","Majority subgroup-nonFSM","GCSE Mathematics","Yes",
      getB(esc_mean_se(0.1,(0.1+0.037)/1.96,224,0.049,(0.049+0.071)/1.96,210,es.type = "g")))
  )
  
}


# 21. Hanselman 2014 ------------------------------------------------------

Hanselman2014 <- function(){
  
  # Similar to Borman 2016, but they reported multilevel model with different sets of covariates
  # drop the results for now because the reviewer suggested that this is reanalysis of the same dataset
  rbind(
    #c("Hanselman","2014","No","Minority subgroup-URM","GPA","Yes",
    #  getB(esc_B(0.068,0.68,310/2,310/2,es.type = "g"))),
    #c("Hanselman","2014","No","Majority subgroup-White","GPA","Yes",
    #  getB(esc_B(-0.012,0.68,600/2,600/2,es.type = "g")))
    
    c("Hanselman","2014", rep(NA, 8))
    
  )
  
}


# 22. Hanselman 2017 ------------------------------------------------------

Hanselman2017 <- function(){

  rbind(
  # Study 1 refers to Cohort 1, and that uses the same data as Borman 2016, 2018. 
  # However, Math and Reading score for Grade 8 of the 2011 Cohort was not previously reported.
    
 #c("Hanselman (Study 1)","2017","No","Minority subgroup-Black and Hispanic","7th GPA","Yes",
 #  getgv(0.062,	0.057^2, 331)),
 #c("Hanselman (Study 1)","2017","No","Minority subgroup-Black and Hispanic","8th GPA","Yes",
 #   getgv(0.152,	0.07^2, 331)),
  #c("Hanselman (Study 1)","2017","No","Minority subgroup-Black and Hispanic","WKCE Mathematcis, Grade 7","Yes",
  #  getgv(0.072,	0.059^2, 331)),
  c("Hanselman (Study 1)","2017","No","Minority subgroup-Black and Hispanic","WKCE Mathematcis, Grade 8","Yes",
    getgv(0.101,	0.07^2, 331)),
  #c("Hanselman (Study 1)","2017","No","Minority subgroup-Black and Hispanic","WKCE Reading, Grade 7","Yes",
  # getgv(-0.034,	0.069^2, 331)),
  c("Hanselman (Study 1)","2017","No","Minority subgroup-Black and Hispanic","WKCE Reading, Grade 8","Yes",
    getgv(-0.03,	0.071^2, 331)),
  
  # Study 2 refers to Cohort 2
  c("Hanselman (Study 2)","2017","No","Minority subgroup-Black and Hispanic","7th GPA","Yes",
    getgv(-0.002, 0.042^2, 449)),
  c("Hanselman (Study 2)","2017","No","Minority subgroup-Black and Hispanic","8th GPA","Yes",
    getgv(-0.072, 0.058^2, 449)),
  c("Hanselman (Study 2)","2017","No","Minority subgroup-Black and Hispanic","WKCE Mathematcis, Grade 7","Yes",
    getgv(-0.085,	0.047^2, 449)),
  c("Hanselman (Study 2)","2017","No","Minority subgroup-Black and Hispanic","WKCE Mathematcis, Grade 8","Yes",
    getgv(-0.08,	0.044^2, 449)),
  c("Hanselman (Study 2)","2017","No","Minority subgroup-Black and Hispanic","WKCE Reading, Grade 7","Yes",
    getgv(-0.005,	0.055^2, 449)),
  c("Hanselman (Study 2)","2017","No","Minority subgroup-Black and Hispanic","WKCE Reading, Grade 8","Yes",
    getgv(-0.005,	0.056^2, 449))
  ) 
}


