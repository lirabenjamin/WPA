library(tidyverse)
devtools::install_github(repo = "lirabenjamin/Ben",force = T)
library(Ben)
theme_ang = function(){
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = .25))
}


d9 = read.csv("WCI_MBA_data_2019.csv") 
d0 = read.csv("WCI_MBA_data_2020.csv") 

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

sc$SubjectID %>% unique() %>% length() #623 unique people were evaluated, 739 total, (116 ppl did it twice)
sc %>% select(SubjectID) %>% group_by(SubjectID) %>% summarise(n=n()) %>% pull(n) %>% table() #8 evals on avg, 1-43
sc$EvaluatorID %>% unique() %>% length() #3558 unique people were evaluators

sc %>% filter(Relationship =="Self") %>% 
  mutate(avg_perf = (avg_perf/100)*7+1) %>% 
  select(-SubjectID,-X,-class,-Relationship) %>% 
  gather(varaible,value,-year,-EvaluatorID) %>% 
  ggplot(aes(year,value))+geom_line(aes(group = EvaluatorID))+
  facet_wrap(~varaible)

sc %>% 
  select(SubjectID,EvaluatorID,Relationship,GritPer_scale,year) %>% 
  filter(year == 2019) %>% 
  spread(Relationship,GritPer_scale)

(sc %>% 
  select(SubjectID,Relationship,GritPer_scale:avg_perf) %>% 
  group_by(SubjectID) %>% 
  nest())[1,2] %>% 
  unnest(data) -> a 

a %>% mahal()
x = a
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
  
