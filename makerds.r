setwd("A:/Zarathu/LASSO, RF/HYUPSY3")
library(data.table)
library(magrittr)
library(labelled)
library(haven)
a<-as.data.table(read_sav("REAP study_20140616.sav"))
b<-data.table()
##OUTCOME--------------------------------------------------------------------------------------------------------------
# (1) dantidepressant (2): 1�� �׿������ �� ������ ����� ����̰� 2�� �׿������ �� ���� �̻� ����� ����Դϴ�.
b$Antidepressant<-ifelse(a$dantidepressant==1,"single",ifelse(a$dantidepressant==2,"multiple",NA)) %>% as.factor
# (2) daps (1): 1�� �����ź��๰�� ����� ����̰� 0�� �����ź��๰�� ������� ���� ����Դϴ�.
b$Aps<-a$daps
# (3) dmoodstabilizer (1): 1�� ��о������� ����� ����̰� 0�� ��о������� ������� ���� ����Դϴ�.
b$Moodstabilizer<-a$dmoodstabilizer
# (4) danxiolytics (1): 1�� �׺Ҿ����� ����� ����̰� 0�� �׺Ҿ����� ������� �ʴ� ����Դϴ�.
b$Anxiolytics<-a$danxiolytics
# (5) dhypnotics (1): 1�� �������� ����� ����̰� 0�� �������� ������� ���� ����Դϴ�.
b$Hypnotics<-a$dhypnotics
# (6) psychotic (1): 1�� ���ź��� ������ �ִ� ����̰� 0�� ���ź��� ������ ���� ����Դϴ�.
b$Psychotic<-a$psychotic

af<-names(b)[-1]
b[,(af):=lapply(.SD,function(x){ factor(x, labels=c("No","Yes"))}),.SDcols=af]

##VARIABLES--------------------------------------------------------------------------------------------------------------
#Region code area�� Incode code are�� Country�� ������ �̿��ؼ� �Ʒ��� ���� ���� ������ ��Ź�帳�ϴ�.
#Country 1=China, 2=Hong Kong, 3=Japan, 4=Korea, 5=Singapore. 6=Taiwan, 7=India, 8=Malaysia, 9=Thailand, 10=Indonesia

# (1) Region_code_area: 1=eastern_asia; 2=southeastern_asia; 3=southern_asia
# �� According to the United Nations classification: 
#   Eastern Asia (China, Hong Kong, Japan, Korea and Taiwan), 
#   Southern Asia (Bangladesh, India, Pakistan and Sri Lanka) and 
#   Southeastern Asia (Indonesia, Malaysia, Myanmar, Singapore, Thailand and Viet Nam)
b$Region_code_area<-ifelse(a$country %in% c(1,2,3,4,6),"Eastern Asia",
                          ifelse(a$country %in% c(7),"Southern Asia",
                                 ifelse(a$country %in% c(10,8,5,9),"Southeastern Asia",NA)))

# (2) Income_code_area: 1=high_income; 2=upper_middle_income; 3=lower_middle_income
# �� According to the World Bank list of economies:
#   high income (Hong Kong, Japan, Korea, Singapore and Taiwan), 
#   upper middle income (China, Malaysia, and Thailand) and 
#   Lower middle income (Bangladesh, India, Indonesia, Myanmar, Pakistan, Sri Lanka and Viet Nam)
b$Income_code_area<-ifelse(a$country %in% c(2,3,4,5,6),"High income",
                           ifelse(a$country %in% c(1,8,9),"Upper middle income",
                                  ifelse(a$country %in% c(7,10),"Lower middle income",NA)))

# (3) Age: ���Ӻ���
b$Age<-a$age
# (4) Sex: 1=Male, 2=Female
b$Sex<-ifelse(a$sex==1,"Male","Female")
# (5) Current: 1=outpatient 2=inpatient
b$Current<-ifelse(a$current==1,"Outpatient",ifelse(a$current==2,"Inpatient",NA))
# (6) Season of Birth: birthmonth ������ �̿��ؼ� ������ ���� ��Ź�帳�ϴ�.
# 1=spring (3-5��), 2=summer (6-8��), 3=autumn (9-11��), 4=winter (12-2��)
b$Season_birth<-ifelse(a$birthmonth %in% c(3,4,5),"Spring",
                       ifelse(a$birthmonth %in% c(6,7,8),"Summer",
                              ifelse(a$birthmonth %in% c(9,10,11),"Autumn",
                                     ifelse(a$birthmonth %in% c(12,1,2),"Winter",NA))))

# (7) Sad 1= presence, 0=absence
b$Sad<-a$sad
# (8) Inter 1= presence, 0=absence
b$Inter<-a$inter
# (9) Fatig 1= presence, 0=absence
b$Fatig<-a$fatig
# (10) Sleep 1= presence, 0=absence
b$Sleep<-a$sleep
# (11) concern 1= presence, 0=absence
b$Concern<-a$concen
# (12) selfcon 1= presence, 0=absence
b$Selfcon<-a$selfcon
# (13) appe 1= presence, 0=absence
b$Appe<-a$appe
# (14) suici 1= presence, 0=absence
b$Suici<-a$suici
# (15) agit 1= presence, 0=absence
b$Agit<-a$agit
# (16) guilt 1= presence, 0=absence
b$Guilt<-a$guilt
# (18) somatic 1= presence, 0=absence
b$Somatic<-a$Somatic
# (19) anxiety 1= presence, 0=absence
b$Anxiety<-a$anxiety 
# (20) CO_ANX 1= presence, 0=absence
b$CO_ANX<-a$CO_ANX
# (21) CO_SUB 1= presence, 0=absence
b$CO_SUB<-a$CO_SUB

af<-names(b)[13:26]
b[,(af):=lapply(.SD,function(x){ factor(x, labels=c("Absence","Presence"))}),.SDcols=af]

# (17) degreeofdepression 1=subthreshold, 2=mild depression, 3=moderate depression, 4=severe depression 1= presence, 0=absence
b$Degree_depression<-ifelse(a$degreeofdepression==1,"Subthreshold",
                            ifelse(a$degreeofdepression==2,"Mild depresseion",
                                   ifelse(a$degreeofdepression==3,"Moderate depression",
                                          ifelse(a$degreeofdepression==4,"Severe depression",NA))))
b$Depression<-ifelse(a$degreeofdepression==1,"Absence",ifelse(a$degreeofdepression %in% c(2,3,4),"Presence",NA))

af<-names(b[,!c("Age")])
b[,(af):=lapply(.SD,as.factor),.SDcols=af]

saveRDS(b, file = "REAP-3-20220426.rds")