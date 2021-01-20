library(tidyverse)
# devtools::install_github(repo = "lirabenjamin/Ben",force = T)
library(Ben)
library(magrittr)
library(psych)

d9 = read.csv("data/WCI_MBA_data_2019.csv") 
d0 = read.csv("data/WCI_MBA_data_2020.csv") 

d = rbind(d9,d0)
d
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

#write_rds(sc,"Data/sc.rds")


ids = d %>% select(X,SubjectID,EvaluatorID,Relationship,year,class)

ids %>% arrange(EvaluatorID,SubjectID)

# Conversation with Chayce
# Control for year, not nest for.
# Compare fixed subjects effect to random slopes of their nested evaluators
# lmer(dv ~ IV_participant*IV_informant + (ParticipantID | InformantID))

# Descriptives ####
sc$SubjectID %>% unique() %>% length() #623 unique people were evaluated, 739 total, (116 ppl did it twice)
sc %>% select(SubjectID) %>% group_by(SubjectID) %>% summarise(n=n()) %>% pull(n) %>% sd() #8.59 (5.35) evals on avg, 1-43
sc$EvaluatorID %>% unique() %>% length() #3558 unique people were evaluators

# Psychometrics ####
scale = "RS"
d %>% 
  rename_at(vars(Giver_1:Taker_8),function(x){paste0("RS",x)}) %>% 
  select(starts_with(scale)&!matches("scale")) %>% 
  #psych::alpha()
  fa(nfactors = 3,rotate = "promax") %>% 
  print.psych(digits = 2, cut = .3,sort= T)
  

# Do people change from year to year? ####
sc %>% 
  filter(Relationship =="Self") %>% 
  mutate(avg_perf = (avg_perf/100)*6+1) %>% 
  select(-SubjectID,-X,-class,-Relationship) %>% 
  gather(varaible,value,-year,-EvaluatorID) %>% 
  ggplot(aes(year,value))+
  geom_line(aes(group = EvaluatorID),alpha=.3)+
  facet_wrap(~varaible,nrow = 2)+
  scale_x_continuous(breaks = c(2019,2020))+
  scale_y_continuous(breaks = 1:7)+
  theme_ang()

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
                   filter(Relationship != "Self") %>% 
  select(SubjectID,year,GritPer_scale:avg_perf) %>% 
  gather(Outcome, Value,-year,-SubjectID) %>% 
  group_by(Outcome,year) %>% 
  nest() %>% 
  mutate(ICC= map_dbl(data,withinvar))
,
sc %>% 
  filter(Relationship != "Self") %>% 
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
         ICC = numformat(1-ICC)) %>% 
  spread(year,ICC) %T>% 
  write.clip()

# RSAs ####
library(RSA)
#Only include people who have at least one self- and one other- rating.
ValidIDs = sc %>% 
  mutate(idy = paste0(SubjectID,year)) %>% 
  group_by(idy) %>% 
  mutate(inclself = Relationship %in% c("Self")) %>% 
  summarise(self = mean(inclself)) %>% 
  filter(self > 0 & self < 1) %>% 
  pull(idy)


# rsas = sc %>% 
#   mutate(idy = paste0(SubjectID,year)) %>% 
#   filter(idy %in% ValidIDs) %>% 
#   select(-X) %>% 
#   group_by(SubjectID) %>% 
#   mutate(p = mean(avg_perf)) %>% 
#   gather(Variable,o,-SubjectID,-EvaluatorID,-Relationship,-p,-class,-year,-idy) %>% 
#   group_by(idy,Variable) %>% 
#   mutate(s = o[Relationship == "Self"]) %>% 
#   filter(Relationship != "Self") %>% 
#   group_by(Variable,year) %>% 
#   mutate(Relationship = as.factor(Relationship),
#          year = factor(year),
#          class = factor(class),
#          o = scale(o),
#          s = scale(s),
#          p = scale(p)) %>% 
#   nest() %>% 
#   mutate(rsa = map(data,function(x){RSA(formula = p~o*s+Relationship+class, data = x)}))

# write_rds(rsas,"Output/rsas.rds")
rsas = read_rds("Output/rsas.rds")

extract = function(x){
  rsas$rsa[[1]]$models$full %>% parameterestimates(standardized = T) %>% 
    mutate(beta = formatest(std.all,pvalue)) %>% 
    select(label,std.all,pvalue,beta) %>% 
    filter(str_detect(label,"a")|str_detect(label,"b"))
}

rsaparams = rsas %>% 
  mutate(params = map(rsa,extract),
         r2 = map_dbl(rsa,"r.squared")) %>% 
  unnest(params) %>% 
  select(-data,-rsa,-pvalue,-std.all) %>% 
  spread(label,beta) %>% 
  select(-a5)
colnames(rsaparams)[4:12] = c("SLC","CLC","SLI","CLI","self","other","self2","interaction",'other2')
rsaparams[c(1:9,11,10,12)] %>% 
  mutate(r2 = numformat(r2)) %>% 
  write.clip()

rsas$rsa[[9]] %>% plot()

plots = rsas %>% mutate(plot = map2(rsa,paste(Variable,as.character(year)),function(x,y){plot(x,
                                                                    xlab = "Self", #Title
                                                      ylab = "Other",
                                                      zlab = Variable,
                                                      type = "3d",         #For those who have difficulty interpreting 3-d figures try "interactive" which will let you rotate the figure
                                                      surface = "predict", #Response surface is based on predicted values of outcome
                                                      rotation = list(x = -63, y = 32, z = 15),
                                                      legend  = FALSE,     #TRUE displays color legend
                                                      param   = FALSE,      #Display RSA parameters
                                                      coefs   = FALSE,      #Display polynomial coefficients 
                                                      axes    = c("LOC", "LOIC"), #Display line of congruence and line of incongruence
                                                      project = NULL,      #TRUE displays projections onto the bottom of the plot
                                                      hull    = FALSE,     #TRUE displays a bag plot on the surface
                                                      points = FALSE)})) %>% 
  select(year,Variable,plot)


for (i in 1:16){
  png(paste0("Plots/",i,"RSA.png"), res = 500,height = 1500,width = 1500) 
  print(plots$plot[[i]])
  dev.off() 
}

# Mahalanobis approach ####

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
            avg_perf = mean(avg_perf[Relationship!="Self"])) %>% 
  ungroup() %>% 
  ggplot(aes(m,avg_perf))+
  geom_point(aes(col = m>qchisq(.05,8)))+
  geom_smooth(method = "lm")+
  theme_ang() +
  coord_cartesian(xlim = c(0,100))+
  labs(y = "Average performance",
       x = "Mahalanobis distance of self-evaluation",
       col = "Is the distance significant (p < .05)",
       title ="Mahalnobis distance of self-evaluations\ndo not predict performance",
       subtitle = "r = .06 ns")
  
