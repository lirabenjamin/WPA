library(tidyverse)
#devtools::install_github(repo = "lirabenjamin/Ben",force = T)
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

sc %>% 
  filter(Relationship !="Self") %>% 
  group_by(year,SubjectID) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  ggplot(aes(n))+geom_histogram(bins = 20)+facet_wrap(~year)+theme_ang()

sc %>% 
  filter(Relationship !="Self") %>% 
  group_by(year,SubjectID) %>% 
  summarise(n = n()) %>% 
  filter(year==2019) %>% 
  pull(n) %>% max()

# Psychometrics ####
scale = "RS"
d %>% 
  rename_at(vars(Giver_1:Taker_8),function(x){paste0("RS",x)}) %>% 
  select(starts_with(scale)&!matches("scale")) %>% 
  #psych::alpha()
  fa(nfactors = 3,rotate = "promax") %>% 
  print.psych(digits = 2, cut = .3,sort= T)
  

# Do people's self reports change from year to year? ####
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
                   filter(Relationship != "Self") %>% # comment this line to include the self in ICCs
  select(SubjectID,year,GritPer_scale:avg_perf) %>% 
  gather(Outcome, Value,-year,-SubjectID) %>% 
  group_by(Outcome,year) %>% 
  nest() %>% 
  mutate(ICC= map_dbl(data,withinvar))
,
sc %>% 
  filter(Relationship != "Self") %>% # comment this line to include the self in ICCs
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

# Using the psych::ICC
iccsp = sc %>% 
  filter(Relationship != "Self") %>% 
  select(SubjectID,EvaluatorID,year,GritPer_scale:avg_perf) %>% 
  group_by(SubjectID) %>% 
  #slice_sample(n = 2) %>% # Comment out to keep all judges. Run this line to limit to select 8 random judges
  gather(Outcome, Value,-year,-SubjectID,-EvaluatorID) %>% 
  #
  group_by(SubjectID,Outcome,year) %>% 
  mutate(EvaluatorID = 1:n()) %>% 
  #
  group_by(Outcome,year) %>% 
  nest() %>% 
  mutate(data = map(data,spread,EvaluatorID,Value),
         data = map(data,select,-1)) %>% 
  mutate(iccs = map(data,ICC),
         iccs = map(iccs,"results"),
         iccs = map(iccs, select, type,ICC),
         iccs = map(iccs, spread,type,ICC))

iccsp$data[[1]] %>% head
iccsp$data[[10]] %>% ICC



# Comparing manual vs. psych()
right_join(iccsp %>% unnest(iccs) %>% select(-data),
iccs %>% mutate(ICC = 1 - ICC) %>% select(-data))

iccsp %>% unnest(iccs) %>%  select(year,Outcome,ICC1,ICC1k) %>% 
  mutate(icc = paste(numformat(ICC1),numformat(ICC1k))) %>% 
  select(year,Outcome,icc) %>% 
  spread(year,icc) %>% 
  separate(`2019`, c("ICC1","ICC1k"),sep = " ") %>% 
  separate(`2020`, c("ICC1 ","ICC1k "),sep = " ") %T>% 
  write.clip()

  
  # Plot ICCs  
for(i in 1:10){
sc %>% 
  filter(Relationship != "Self") %>% 
  select(SubjectID,GritPer_scale:avg_perf) %>% 
  group_by(SubjectID) %>% 
  nest() %>% 
  ungroup() %>% 
  slice_sample(n = 15) %>% 
  unnest(data) %>% 
  gather(Variable,Value,-SubjectID) %>% 
  ggplot(aes(SubjectID,Value))+
  geom_point(alpha = .3)+
  stat_summary(fun = "mean",geom = "point",shape = "-",size = 10,col= "Red")+
  facet_wrap(~Variable,scales = "free_y",nrow = 2)+
  theme_ang()+
  theme(axis.text.x = element_blank(),panel.grid.major.x = element_line(colour = "gray",size = .1))
ggsave(paste0("Plots/ICC/",i,".pdf"),width = 10,height = 5)
}
#system("convert -delay 180 -density 500 Plots/ICC/*.pdf example_1.gif")


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

#Outcome = Mean rating from all judges
# rsas = sc %>%
#   mutate(idy = paste0(SubjectID,year)) %>%
#   filter(idy %in% ValidIDs) %>%
#   select(-X) %>%
#   group_by(SubjectID) %>%
#   mutate(p = mean(avg_perf[Relationship != "Self"])) %>%
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
#   mutate(rsa = map(data,function(x){RSA(formula = p~o*s,control.variables = "Relationship", data = x)}))
# write_rds(rsas,"Output/rsas_mean.rds")

#Outcome = Individual rating from that specific judge
# rsas = sc %>%
#   mutate(idy = paste0(SubjectID,year)) %>%
#   filter(idy %in% ValidIDs) %>%
#   select(-X) %>%
#   group_by(SubjectID) %>%
#   gather(Variable,o,-SubjectID,-EvaluatorID,-Relationship,-avg_perf,-class,-year,-idy) %>%
#   rename(p = avg_perf) %>% 
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
#   mutate(rsa = map(data,function(x){RSA(formula = p~o*s,control.variables = "Relationship", data = x)}))
# 
# write_rds(rsas,"Output/rsas_judgei.rds")


#rsas = read_rds("Output/rsas.rds")
rsas = read_rds("Output/rsas_judgei.rds")

extract = function(x){
  x$models$full %>% parameterestimates(standardized = T) %>% 
    mutate(beta = formatest(std.all,pvalue)) %>% 
    select(label,std.all,pvalue,beta) %>% 
    filter(str_detect(label,"a")|str_detect(label,"b"))
}

rsaparams = rsas %>% 
  rowwise() %>% 
  ungroup() %>% 
  mutate(params = map(rsa,extract),
         r2 = map_dbl(rsa,"r.squared")) %>% 
  unnest(params) %>% 
  select(-data,-rsa,-pvalue,-std.all) %>% 
  spread(label,beta) %>% 
  select(-a5)
colnames(rsaparams)[4:12] = c("SLC","CLC","SLI","CLI","self","other","self2","interaction",'other2')
rsaparams[c(1:9,11,10,12)] %>% 
  mutate(r2 = numformat(r2)) %T>% 
  write.clip()



  plots = rsas %>% mutate(plot = map(rsa,paste(Variable),function(x){plot(x,
                                                                    xlab = "Self", #Title
                                                      ylab = "Other",
                                                      zlab = "Performance",
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

  
  plot(rsas$rsa[[1]],
       xlab = "Self", #Title
       ylab = "Other",
       zlab = "Performance",
       type = "3d",         #For those who have difficulty interpreting 3-d figures try "interactive" which will let you rotate the figure
       surface = "predict", #Response surface is based on predicted values of outcome
       rotation = list(x = -63, y = 32, z = 15),
       legend  = FALSE,     #TRUE displays color legend
       param   = FALSE,      #Display RSA parameters
       coefs   = FALSE,      #Display polynomial coefficients 
       axes    = c("LOC", "LOIC"), #Display line of congruence and line of incongruence
       project = NULL,      #TRUE displays projections onto the bottom of the plot
       hull    = FALSE,     #TRUE displays a bag plot on the surface
       points = FALSE)

  for (i in 1:16){
  png(paste0("Plots/RSA2/",i,"RSA.png"), res = 500,height = 1500,width = 1500) 
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
  

# Appendix ####
#Manual calc from internet
#https://stackoverflow.com/questions/3205176/iccs-when-the-number-of-judges-is-not-constant

library(lme4)

# calculate the hierarchical model
m1 = lmer(GritPas_scale ~ (1|SubjectID) + (1|EvaluatorID), data=sc %>% filter(year == 2020))
print(m1)

# helper function to pull out the variances
xVars <- function(m1) {
  exvars = lme4::VarCorr(m1)
  vars = c(exvars$SubjectID[1,1], exvars$EvaluatorID[1,1], attr(exvars,"sc")^2) 
  names(vars) <- c('person var', 'judge var', 'residual var')
  vars
}
# helper function for ICC(k) variations
icck <- function(variances, k=1) {
  icc = numeric()
  for (i in 1:k){
    icc = append(icc,variances[1] / (variances[1] + (variances[2] + variances[3]) / i))
  }
  
  names(icc) = 1:k
  enframe(icc) %>% return
}

iccplot = sc %>% 
  select(SubjectID,EvaluatorID,year,GritPer_scale:avg_perf) %>% 
  gather(Variable,Value,-SubjectID,-EvaluatorID,-year) %>% 
  group_by(year,Variable) %>% 
  nest() %>% 
  mutate(lmer = map2(data,year,function(x,y){lmer(Value ~ (1|SubjectID) + (1|EvaluatorID), data=x %>% filter(year == y))}),
         varcomp = map(lmer,xVars),
         k = ifelse(year == 2019 , 19,23),
         icck = map2(varcomp,k,function(x,y){icck(x,y)})) %>% 
  unnest(icck) %>%
  select(year,Variable,name,value) %>% 
  mutate(name = as.numeric(name)) %>% 
  ggplot(aes(name,value,color=Variable))+
  geom_line(aes(group = Variable))+
  scale_color_brewer(palette = "Set1")+
  facet_wrap(~year)+
  theme_ang()+
  geom_text(col = "black",data = . %>% filter(name %in% c(1,8,max(name))),size = 2,vjust = .1,aes(label = numformat(value)))
ggsave(iccplot,filename = "Plots/ICC/Vark.pdf",width = 7,height = 5)


# the output you want
icc = numeric()
for (i in 1:23){icc = icck(xVars(m1), i) %>% append(icc,.)}
p = icc %>% 
  enframe() %>% 
  mutate(name = str_remove(name,"ICC") %>% as.numeric()) %>% 
  select(k = name,
         icc = value) %>% 
  ggplot(aes(k,icc))+
  geom_line(aes(group = 1))+
  theme_ang()+
  geom_hline(yintercept = 1,size = .1)+
  coord_cartesian(ylim = c(0,1.05))+
  labs(title = "As the number of judges increases, ICC aproximates 1",
       subtitle = "Ratings for Grit Passion in 2020\nICC = 1.00 rounded to two decimals when k = 844")

ggsave(p,filename = "Plots/ICC/k.pdf",width = 5,height = 4)
  