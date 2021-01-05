library(tidyverse)
# devtools::install_github(repo = "lirabenjamin/Ben",force = T)
library(Ben)

d9 = read.csv("data/WCI_MBA_data_2019.csv") 
d0 = read.csv("data/WCI_MBA_data_2020.csv") 

d = rbind(d9,d0)

d %>% select(StartDate,EndDate)

sc = d %>% select(X,SubjectID,EvaluatorID,Relationship,
                  GritPer_scale,
                  GritPas_scale,
                  HumMot_scale,
                  HumRec_scale,
                  Giver_scale,
                  Matcher_scale,
                  Taker_scale,
                  avg_perf,
                  year,
                  class)

ids = d %>% select(X,SubjectID,EvaluatorID,Relationship,year,class)

ids %>% arrange(X)

# Descriptives ####
sc$SubjectID %>% unique() %>% length() #623 unique people were evaluated, 739 total, (116 ppl did it twice)
sc %>% select(SubjectID) %>% group_by(SubjectID) %>% summarise(n=n()) %>% pull(n) %>% table() #8 evals on avg, 1-43
sc$EvaluatorID %>% unique() %>% length() #3558 unique people were evaluators

# Do people change from year to year? ####
sc %>% 
  filter(Relationship =="Self") %>% 
  mutate(avg_perf = (avg_perf/100)*6+1) %>% 
  select(-SubjectID,-X,-class,-Relationship) %>% 
  gather(varaible,value,-year,-EvaluatorID) %>% 
  ggplot(aes(year,value))+geom_line(aes(group = EvaluatorID))+
  facet_wrap(~varaible)

# Is there variability within? ####
library(lme4)

data = sc %>% mutate(Value = GritPer_scale)
withinvar = function(data){
  lme = lme4::lmer(formula = Value ~ 1 + (1 | SubjectID),data=data) %>% VarCorr() %>% as.data.frame()
  cov = lme$vcov[2]/(lme$vcov[1]+lme$vcov[2])
  cor = lme$sdcor[2]/(lme$sdcor[1]+lme$sdcor[2])
  return(cov)
}

iccs = bind_rows(sc %>% 
  select(SubjectID,year,GritPer_scale:avg_perf) %>% 
  gather(Outcome, Value,-year,-SubjectID) %>% 
  group_by(Outcome,year) %>% 
  nest() %>% 
  mutate(ICC= map_dbl(data,withinvar))
,
sc %>% 
  select(SubjectID,year,GritPer_scale:avg_perf) %>% 
  gather(Outcome, Value,-year,-SubjectID) %>% 
  group_by(Outcome) %>% 
  nest() %>% 
  mutate(ICC= map_dbl(data,withinvar),
         year = 2021))

iccs %>% select(year,Outcome,ICC) %>% 
  mutate(year = case_when(year == 2019 ~ "2019",
                          year == 2020 ~ "2020",
                          year == 2021 ~ "Both"),
         ICC = numformat(ICC)) %>% 
  spread(year,ICC) %>% 
  write.clip()

# RSAs ####
library(RSA)
sc %>% 
  select(SubjectID,Relationship,GritPas_scale,avg_perf) %>% 
  filter(Relationship == "Self")
  spread(Relationship,GritPas_scale) 
RSA::RSA(formula = )


# Function: Returns mahalanobis distance for each evaluator.
mahal = function(x){
  x = select_if(x,is.numeric)
  m = colMeans(x,na.rm = T)
  c = cov(x,use = "pairwise.complete.obs")
  if(nrow(x) == 1){return(NA)}
  if(det(c)!= 0){d = mahalanobis(x,m,c,tol=1e-40)}
  if(det(c)!= 0){return(d)}
  if(det(c) == 0){return(NA)}
}


m = sc %>% 
  select(SubjectID,Relationship,GritPer_scale:avg_perf) %>% 
  group_by(SubjectID) %>% 
  nest() %>% 
  mutate(m=map(data,mahal)) %>% 
  unnest()


m %>% 
  select(SubjectID,Relationship,m,avg_perf) %>% 
  filter(m > 0) %>% 
  group_by(SubjectID) %>% 
  summarise(m = m[Relationship=="Self"],
            avg_perf = mean(avg_perf)) %>% 
  ungroup() %>% 
  ggplot(aes(m,avg_perf))+
  geom_point(aes(col = m>qchisq(.05,8)))+
  geom_smooth(method = "lm")+
  theme_ang() +
  coord_cartesian(xlim = c(0,100))
  
