# cleaning script

clean_master <- function(kost, purdie, turetsky){
   dat <- rbind(
     Baker2019(),
     Bancroft2017(),
     Bayly2017(),
     BinningUR(),
     Borman2012(),
     Borman2015(),
     Borman2016(),
     Borman2018(),
     Bowen2013(),
     Bratter2016(),
     Churchill2018(),
     Cohen2006(),
     Cohen2009(),
     Cook2012(),
     DeClercq2019(),
     Dee2015(),
     deJong2016(),
     Goyer2017(),
     Gutmann2019(),
     Hadden2019(),
     Hanselman2014(),
     Hanselman2017(),
     Harackiewicz2014(),
     Harackiewicz2016(),
     Hayes2019(),
     Jordt2017(),
     Kim2019(),
     Kinias2016(),
     Kostsmith2010(),
     Kostsmith2012(kost),
     Lauer2013(),
     Lokhande2019(),
     Miyake2010(),
     Peters2017(),
     Powers2016(),
     Protzko2016(),
     PurdieGreenawayUR(purdie),
     Rapa2016(),
     Rozek2015(),
     Schwalbe2018(),
     SerraGarciaUR(),
     Sherman2013(),
     Shnabel2013(),
     Silverman2014(),
     Tibbetts2016(),
     Tibbetts2018(),
     TuretskyUR(turetsky),
     Woolf2009(),
     Wynne2011()
   )
 
  

dat <- dat %>% 
  as.data.frame() %>% 
  `colnames<-`( c("author", "year", "adapted", "type",  "outcome", 
                  "adjusted", "es", "v", "lowerCI", "upperCI")) %>% 
  mutate_at(vars(es:upperCI), as.numeric) %>% 
  mutate(
    study = paste(author, year, sep = ", "),
    se = sqrt(v)) %>%
  group_by(study) %>% 
  mutate(id = cur_group_id()) %>%
  select(id, study, author, year, adapted:es, v, se, lowerCI, upperCI) %>% 
  mutate_at(vars(es:upperCI), round, digits = 4)
# need to work on creating clusters...

return(dat)

}
