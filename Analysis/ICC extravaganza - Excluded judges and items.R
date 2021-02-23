library(tidyverse)
#devtools::install_github(repo = "lirabenjamin/Ben",force = T)
library(Ben)
library(magrittr)
library(psych)
library(irrNA)
library(lme4)

#d9 = read.csv("data/WCI_MBA_data_2019.csv") 
#d0 = read.csv("data/WCI_MBA_data_2020.csv") 
#d = rbind(d9,d0)
d0 = read.csv("data/MBA_data_2020 - w:experimental items.csv")
 
code = read.csv("data/Item codebook.csv")
ex = function(x){code %>% filter(Subscale == unique(code$Subscale)[x]) %>% pull(Item)}
score = function(vars,data=d0){data %>% rowwise %>% select(vars) %>% rowMeans(na.rm = T)}
scores = tibble(ex(1) %>% score,
       ex(2) %>% score,
       ex(3) %>% score,
       ex(4) %>% score,
       ex(5) %>% score,
       ex(6) %>% score,
       ex(7) %>% score)
colnames(scores) = c("GritPas_scale","GritPer_scale"  , "HumMot_scale","HumRec_scale"  ,"Giver_scale"   ,"Matcher_scale" ,"Taker_scale"  )
d = cbind(d0 %>% select(1:4,year,class),scores)

d = d %>% filter(Relationship != "Self")

mnj = d %>% 
  group_by(SubjectID,year) %>% 
  summarise(nj = n()) %>% 
  group_by(year) %>% 
  summarise(nj = mean(nj)) 

ng = d %>% 
  group_by(SubjectID,year) %>% 
  summarise(nj = n(),
            nj2 = nj^2) %>% 
  group_by(year) %>% 
  mutate(nt = n(),
         ng = (1/(nt-1))*(sum(nj) - (sum(nj2)/sum(nj)))) %>% 
  group_by(year) %>% 
  summarise(ng = mean(ng))

left_join(mnj,ng)

library(careless)
careless = d0 %>% filter(Relationship != "Self") %>% select(SubjectID,
                                                        EvaluatorID,
                                                        starts_with("Giver_"),
                                                        starts_with("Matcher_"),
                                                        starts_with("Taker_"),
                                                        starts_with("GritPas_"),
                                                        starts_with("GritPer_"),
                                                        starts_with("HumMot_"),
                                                        starts_with("HumRec_"),
                                                        starts_with("HumExp_"),
                                                        -ends_with("_scale")) %>% longstring()>9

nd = 
  d %>% filter(!careless) %>% 
  gather(Variable,Value,-c(X:Relationship,year,class)) %>% 
  group_by(SubjectID,year,Variable) %>% 
  mutate(JudgeN = 1:n()) %>%
  group_by(year,Variable) %>% 
  nest() %>% 
  left_join(mnj) %>% 
  left_join(ng)

#iccna
nd$data[[1]] %>% select(SubjectID,JudgeN,Value) %>% spread(JudgeN,Value) %>% select(-SubjectID) %>% irrNA::iccNA()
#variance from lme
nd$data[[1]] %>% lme4::lmer(Value ~ 1 + (1|SubjectID),data = .) %>% VarCorr() %>% as.data.frame() %>% select(grp,vcov) %>% spread(grp,vcov) %>% rename(MSB = Residual, MSW = SubjectID)
#icc from lme
nd$data[[16]] %>% lme4::lmer(Value ~ 1 + (1|SubjectID),data = .) %>% VarCorr() %>% as.data.frame() %>% select(grp,vcov) %>% spread(grp,vcov) %>% rename(MSB = Residual, MSW = SubjectID) %>% mutate(icclme = MSW/(MSW+MSB)) %>% select(icclme)
#variance from lm
nd$data[[1]] %>% lm(Value ~ SubjectID,data = .) %>% anova() %>% as.data.frame() %>% select(`Mean Sq`) %>% rownames_to_column("id") %>% rename(msq = `Mean Sq`) %>% spread(id,msq) %>% rename(MSB = Residuals, MSW = SubjectID)


iccna = nd %>% 
  mutate(iccNA = map(data,function(x){x%>% select(SubjectID,JudgeN,Value) %>% spread(JudgeN,Value) %>% select(-SubjectID) %>% irrNA::iccNA()}),
         iccNA = map(iccNA,function(x){x%>% `$`(ICCs) %>% as.data.frame %>% slice(1:2) %>% rownames_to_column("iccna") %>% select(iccna, ICC) %>% spread(iccna,ICC) %>% rename_all(function(x){paste0("iccna_",x)})}),
         Variancelme = map(data,function(x){lme4::lmer(Value ~ 1 + (1|SubjectID),data = x) %>% VarCorr() %>% as.data.frame() %>% select(grp,vcov) %>% spread(grp,vcov) %>% rename(MSB = Residual, MSW = SubjectID)}),
         Variancelm = map(data,function(x){x %>% lm(Value ~ SubjectID,data = .) %>% anova() %>% as.data.frame() %>% select(`Mean Sq`) %>% rownames_to_column("id") %>% rename(msq = `Mean Sq`) %>% spread(id,msq) %>% rename(MSW = Residuals, MSB = SubjectID)}),
         icclme = map(data,function(x){lme4::lmer(Value ~ 1 + (1|SubjectID),data = x) %>% VarCorr() %>% as.data.frame() %>% select(grp,vcov) %>% spread(grp,vcov) %>% rename(MSB = Residual, MSW = SubjectID) %>% mutate(icclme = MSW/(MSW+MSB)) %>% select(icclme) %>% return()}))

iccna %>% unnest(Variancelm) %>% unnest(iccNA) %>% unnest(icclme) %>% 
  rowwise() %>% 
  mutate(ICC1_ng = (MSB - MSW)/(MSB+MSW*(ng-1)),
         ICC1_k = (MSB - MSW)/(MSB+MSW*(nj-1)),
         ICC1k_m = (MSB-MSW)/(MSB),
         ICC1k_sb_ng = (ng*ICC1_ng)/(1+(ng-1)*ICC1_ng),
         ICC1k_sb_k = (ng*ICC1_k)/(1+(nj-1)*ICC1_k)) %>% 
  select(-nj, -ng, -data,-Variancelme,-MSW,-MSB) %T>% write.clip()

#Per evaluator ####
dev = d %>% filter(!careless) %>% group_by(SubjectID,Relationship,year) %>% 
  mutate(n = n()) %>% 
  filter(n >1)

mnj = dev %>% 
  group_by(SubjectID,year,Relationship) %>% 
  summarise(nj = n()) %>% 
  group_by(year,Relationship) %>% 
  summarise(nj = mean(nj)) 

ng = dev %>% 
  group_by(SubjectID,year,Relationship) %>% 
  summarise(nj = n(),
            nj2 = nj^2) %>% 
  group_by(year,Relationship) %>% 
  mutate(nt = n(),
         ng = (1/(nt-1))*(sum(nj) - (sum(nj2)/sum(nj)))) %>% 
  group_by(year,Relationship) %>% 
  summarise(ng = mean(ng))

left_join(mnj,ng)

nd = 
  dev %>% gather(Variable,Value,-c(X:Relationship,year,class)) %>% 
  group_by(SubjectID,year,Variable,Relationship) %>% 
  mutate(JudgeN = 1:n()) %>%
  group_by(year,Variable,Relationship) %>% 
  nest() %>% 
  left_join(mnj) %>% 
  left_join(ng)

#iccna
nd$data[[1]] %>% select(SubjectID,JudgeN,Value) %>% spread(JudgeN,Value) %>% select(-SubjectID) %>% irrNA::iccNA()
#variance from lme
nd$data[[1]] %>% lme4::lmer(Value ~ 1 + (1|SubjectID),data = .) %>% VarCorr() %>% as.data.frame() %>% select(grp,vcov) %>% spread(grp,vcov) %>% rename(MSB = Residual, MSW = SubjectID)
#icc from lme
nd$data[[16]] %>% lme4::lmer(Value ~ 1 + (1|SubjectID),data = .) %>% VarCorr() %>% as.data.frame() %>% select(grp,vcov) %>% spread(grp,vcov) %>% rename(MSB = Residual, MSW = SubjectID) %>% mutate(icclme = MSW/(MSW+MSB)) %>% select(icclme)
#variance from lm
nd$data[[1]] %>% lm(Value ~ SubjectID,data = .) %>% anova() %>% as.data.frame() %>% select(`Mean Sq`) %>% rownames_to_column("id") %>% rename(msq = `Mean Sq`) %>% spread(id,msq) %>% rename(MSB = Residuals, MSW = SubjectID)

nd %>% filter(Variable == "n") %>% unnest(data)

iccna2 = nd %>% 
  mutate(iccNA = map(data,function(x){x%>% select(SubjectID,JudgeN,Value) %>% spread(JudgeN,Value) %>% select(-SubjectID) %>% irrNA::iccNA()}),
         iccNA = map(iccNA,function(x){x%>% `$`(ICCs) %>% as.data.frame %>% slice(1:2) %>% rownames_to_column("iccna") %>% select(iccna, ICC) %>% spread(iccna,ICC) %>% rename_all(function(x){paste0("iccna_",x)})}),
         Variancelme = map(data,function(x){lme4::lmer(Value ~ 1 + (1|SubjectID),data = x) %>% VarCorr() %>% as.data.frame() %>% select(grp,vcov) %>% spread(grp,vcov) %>% rename(MSB = Residual, MSW = SubjectID)}),
         Variancelm = map(data,function(x){x %>% lm(Value ~ SubjectID,data = .) %>% anova() %>% as.data.frame() %>% select(`Mean Sq`) %>% rownames_to_column("id") %>% rename(msq = `Mean Sq`) %>% spread(id,msq) %>% rename(MSW = Residuals, MSB = SubjectID)}),
         icclme = map(data,function(x){lme4::lmer(Value ~ 1 + (1|SubjectID),data = x) %>% VarCorr() %>% as.data.frame() %>% select(grp,vcov) %>% spread(grp,vcov) %>% rename(MSB = Residual, MSW = SubjectID) %>% mutate(icclme = MSW/(MSW+MSB)) %>% select(icclme) %>% return()}))

iccna2 %>% unnest(Variancelm) %>% unnest(iccNA) %>% unnest(icclme) %>% 
  rowwise() %>% 
  mutate(ICC1_ng = (MSB - MSW)/(MSB+MSW*(ng-1)),
         ICC1_k = (MSB - MSW)/(MSB+MSW*(nj-1)),
         ICC1k_m = (MSB-MSW)/(MSB),
         ICC1k_sb_ng = (ng*ICC1_ng)/(1+(ng-1)*ICC1_ng),
         ICC1k_sb_k = (ng*ICC1_k)/(1+(nj-1)*ICC1_k)) %>% 
  select(-nj,  -data,-Variancelme,-MSW,-MSB) %T>% write.clip()

iccna2 %>% unnest(Variancelm) %>% unnest(iccNA) %>% unnest(icclme) %>% 
  rowwise() %>% 
  mutate(ICC1_ng = (MSB - MSW)/(MSB+MSW*(ng-1)),
         ICC1_k = (MSB - MSW)/(MSB+MSW*(nj-1)),
         ICC1k_m = (MSB-MSW)/(MSB),
         ICC1k_sb_ng = (ng*ICC1_ng)/(1+(ng-1)*ICC1_ng),
         ICC1k_sb_k = (ng*ICC1_k)/(1+(nj-1)*ICC1_k)) %>% 
  select(Relationship,year,Variable,ng,`iccna_ICC(1)`,`iccna_ICC(k)`) %>% 
  mutate_at(5:6,numformat) %>% 
  unite(ICC, matches("ICC"),sep = " ") %>% 
  select(-ng) %>% 
  spread(Relationship,ICC) %>% 
  filter(Variable != "n") %T>% 
  write.clip

