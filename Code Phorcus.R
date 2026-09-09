library(readxl)
library(ggplot2)
library(glmmTMB)
library(sjPlot)
library(DHARMa)
library(performance)
library(ggeffects)
library(lsmeans)
library(multcomp)
library(effects)
library(ggbeeswarm)
library(onewaytests)
####Load the data from excel####

Monodonta<- read_excel("Tuttemonodontaanalisi.xlsx")
#create database whit right data and right plot for the analysis
Monodonta2 <- subset(Monodonta,Monodonta$Spatial_Replication =="AB"|Monodonta$Spatial_Replication =="CD"| Monodonta$Spatial_Replication =="EF"|Monodonta$Spatial_Replication =="GH")
Monodontaplot <- subset(Monodonta2,Monodonta2$Zone =="Buio"|Monodonta2$Zone =="Faro"| Monodonta2$Zone =="LampBuio"|Monodonta2$Zone =="Lentisco")

#Set variable as factor or numeric
Monodontaplot$MOON<-as.factor(Monodontaplot$MOON)
Monodontaplot$Zone<-as.factor(Monodontaplot$Zone)
Monodontaplot$Time<-as.factor(Monodontaplot$Time)
Monodontaplot$LITUNLIT<-as.factor(Monodontaplot$LITUNLIT)
Monodontaplot$Boulder<-as.factor(Monodontaplot$Boulder)
Monodontaplot$Side<-as.factor(Monodontaplot$Side)
Monodontaplot$LITUNLIT<-as.factor(Monodontaplot$LITUNLIT)
Monodontaplot$Temp<-as.numeric(Monodontaplot$Temp)
Monodontaplot$Day<-as.factor(Monodontaplot$Day)

#whit subset, create separated dataset for different observation times  
Monodontaplotmattina<-subset(Monodontaplot,Monodontaplot$Time =="Mattina")
Monodontaplottramonto<-subset(Monodontaplot,Monodontaplot$Time =="Tramonto")
MonodontaplotNotte1<-subset(Monodontaplot,Monodontaplot$Time =="Notte1")
MonodontaplotNotte2<-subset(Monodontaplot,Monodontaplot$Time =="Notte2")


#set right referements level
levels(Monodontaplotmattina$LITUNLIT)
table(Monodontaplotmattina$LITUNLIT)
Monodontaplotmattina<- within(Monodontaplotmattina,LITUNLIT<-relevel(LITUNLIT,ref = "UNLIT"))

levels(Monodontaplottramonto$LITUNLIT)
table(Monodontaplottramonto$LITUNLIT)
Monodontaplottramonto<- within(Monodontaplottramonto,LITUNLIT<-relevel(LITUNLIT,ref = "UNLIT"))

levels(MonodontaplotNotte1$LITUNLIT)
table(MonodontaplotNotte1$LITUNLIT)
MonodontaplotNotte1<- within(MonodontaplotNotte1,LITUNLIT<-relevel(LITUNLIT,ref = "UNLIT"))

levels(MonodontaplotNotte2$LITUNLIT)
table(MonodontaplotNotte2$LITUNLIT)
MonodontaplotNotte2<- within(MonodontaplotNotte2,LITUNLIT<-relevel(LITUNLIT,ref = "UNLIT"))

#whit subset, create separated dataset for different side of the boulders# 
MonodontaplotmattinaS<-subset(Monodontaplotmattina,Monodontaplotmattina$Side=="S")
MonodontaplottramontoS<-subset(Monodontaplottramonto,Monodontaplottramonto$Side=="S")
MonodontaplotmattinaL<-subset(Monodontaplotmattina,Monodontaplotmattina$Side=="L")
MonodontaplottramontoL<-subset(Monodontaplottramonto,Monodontaplottramonto$Side=="L")
MonodontaplotNotte1S<-subset(MonodontaplotNotte1,MonodontaplotNotte1$Side=="S")
MonodontaplotNotte1<-subset(Monodontaplot,Monodontaplot$Time =="Notte1")
MonodontaplotNotte1L<-subset(MonodontaplotNotte1,MonodontaplotNotte1$Side=="L")
MonodontaplotNotte2L<-subset(MonodontaplotNotte2,MonodontaplotNotte2$Side=="L")
MonodontaplotNotte2S<-subset(MonodontaplotNotte2,MonodontaplotNotte2$Side=="S")

####TEST OW####
#repeat the same script in precedent row, but this time the dependent variable is the number of individuals over the water level
#Morning, L side
Modello_mattinaL_OW<-glmmTMB(OW~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotmattinaL,family = "poisson",REML=FALSE)
car::Anova(Modello_mattinaL_OW)
check_overdispersion(Modello_mattinaL_OW)
Modello_mattinaL_OWcontrollo<-simulateResiduals(Modello_mattinaL_OW) 
plot(Modello_mattinaL_OWcontrollo)
testZeroInflation(Modello_mattinaL_OW)
testOutliers(Modello_mattinaL_OW)
pmatdL<-ggemmeans(Modello_mattinaL_OW,terms =c("LITUNLIT"))
pmatL<-plot(ggemmeans(Modello_mattinaL_OW,terms =c("LITUNLIT")))+ylim(0,3)+labs(title="Landward side",tag="a",subtitle = "(Morning)",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )
pmatL
lsm.ALL.temp <- lsmeans(Modello_mattinaL_OW, c("LITUNLIT"))
cld(lsm.ALL.temp, Letters=letters)
emm <- emmeans(Modello_mattinaL_OW,~LITUNLIT,type = "response")
pairs(emm, adjust = "tukey")
cld_res <- cld(emm, adjust = "tukey", Letters = letters, alpha = 0.05)
cld_res
ptwdL<-ggemmeans(Modello_mattinaL_OW,terms =c("LITUNLIT"))
pmatL+geom_text(aes(label = cld_res$.group, y =  ptwdL$conf.high), vjust = -1)
#serve per altro#
plot(ggpredict(Modello_mattinaL_OW,terms =c("Temp")))+ylim(0,4)+labs(title="Landward side",tag=,subtitle="(Morning)",x ="Temperature (°C)", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) ) 


#Sunset, L side
Modello_tramontoL_OW<-glmmTMB(OW~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplottramontoL,family = "poisson")
car::Anova(Modello_tramontoL_OW)
check_overdispersion(Modello_tramontoL_OW)
Modello_tramontoL_OWcontrollo<-simulateResiduals(Modello_tramontoL_OW)
plot(Modello_tramontoL_OWcontrollo)
testZeroInflation(Modello_tramontoL_OW)
testOutliers(Modello_tramontoL_OW)
psundL<-ggemmeans(Modello_tramontoL_OW,terms =c("LITUNLIT"))
psunL<-plot(ggemmeans(Modello_tramontoL_OW,terms =c("LITUNLIT")))+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,3)+labs(title="Landward side",tag="b",subtitle = "(Sunset)",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )
psunL
lsm.ALL.temp <- lsmeans(Modello_tramontoL_OW, c("LITUNLIT"))
cld(lsm.ALL.temp, Letters=letters)
emm <- emmeans(Modello_tramontoL_OW,~LITUNLIT,type = "response")
pairs(emm, adjust = "tukey")
cld_res <- cld(emm, adjust = "tukey", Letters = letters, alpha = 0.05)
cld_res
ptwdL<-ggemmeans(Modello_tramontoL_OW,terms =c("LITUNLIT"))
psunL+geom_text(aes(label = cld_res$.group, y =  ptwdL$conf.high), vjust = -1)



#Dusk, L side
MonodontaplotNotte1L$LITUNLIT<- factor(MonodontaplotNotte1L$LITUNLIT, levels = c("UNLIT","LIT"))
Modello_Notte1L_OW<-glmmTMB(OW~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte1L,family = "poisson")
car::Anova(Modello_Notte1L_OW)
plot_model(Modello_Notte1L_OW,type="int")+geom_text(label=lettere,position = position_dodge(width = 0.6),hjust=0.2, vjust=0.2, size=6,show.legend = FALSE)+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,4)+labs(title="Landward side",subtitle = "(Twilight)",tag="C",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )
check_overdispersion(Modello_Notte1L_OW)
Modello_Notte1L_OWcontrollo<-simulateResiduals(Modello_Notte1L_OW) 
plot(Modello_Notte1L_OWcontrollo)
testZeroInflation(Modello_Notte1L_OW)
testOutliers(Modello_Notte1L_OW)
ptwdL<-ggemmeans(Modello_Notte1L_OW,terms =c("LITUNLIT"))
ptwL<-plot(ggemmeans(Modello_Notte1L_OW,terms =c("LITUNLIT")))+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,3)+labs(title="Landward side",tag="c",subtitle = "(Twilight)",x="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )
ptwL


lsm.ALL.temp <- lsmeans(Modello_Notte1L_OW, c("LITUNLIT"))
cld(lsm.ALL.temp, Letters=letters)
emm <- emmeans(Modello_Notte1L_OW,~LITUNLIT,type = "response")
pairs(emm, adjust = "tukey")
cld_res <- cld(emm, adjust = "tukey", Letters = letters, alpha = 0.05)
cld_res
ptwL+geom_text(aes(label = cld_res$.group, y =  ptwdL$conf.high), vjust = -1)


#Night, L side
MonodontaplotNotte2L1<-MonodontaplotNotte2L[-49,]
Modello_Notte2L_OW<-glmmTMB(OW~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte2L1,family = "poisson")
car::Anova(Modello_Notte2L_OW)
ptnhdL<-ggemmeans(Modello_Notte2L_OW,terms =c("LITUNLIT"))
ptnhL<-plot(ggemmeans(Modello_Notte2L_OW,terms =c("LITUNLIT")))+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,3)+labs(title="Landward side",tag="d",subtitle = "(Night)",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )
ptnhL
lsm.ALL.temp <- lsmeans(Modello_Notte2L_OW, c("LITUNLIT"))
cld(lsm.ALL.temp, Letters=letters)
emm <- emmeans(Modello_Notte2L_OW,~LITUNLIT,type = "response")
pairs(emm, adjust = "tukey")
cld_res <- cld(emm, adjust = "tukey", Letters = letters, alpha = 0.05)
cld_res
ptwdL<-ggemmeans(Modello_Notte2L_OW,terms =c("LITUNLIT"))
ptnhL+geom_text(aes(label = cld_res$.group, y =  ptnhdL$conf.high), vjust = -1)
check_overdispersion(Modello_Notte2L_OW)
Modello_Notte1L_OWcontrollo<-simulateResiduals(Modello_Notte2L_OW) 
plot(Modello_Notte1L_OWcontrollo)
testZeroInflation(Modello_Notte2L_OW)
testOutliers(Modello_Notte2L_OW)


####Repeat for Total individual ####

Modello_mattinaL_OW<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotmattinaL,family = "poisson",REML=FALSE)
car::Anova(Modello_mattinaL_OW)
check_overdispersion(Modello_mattinaL_OW)
Modello_mattinaL_OWcontrollo<-simulateResiduals(Modello_mattinaL_OW) 
plot(Modello_mattinaL_OWcontrollo)
testZeroInflation(Modello_mattinaL_OW)
testOutliers(Modello_mattinaL_OW)
plot(ggemmeans(Modello_mattinaL_OW,terms =c("LITUNLIT","MOON")))+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,4)+labs(title="Landward side",tag="a",subtitle="(Morning)",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )

lsm.ALL.temp <- lsmeans(Modello_mattinaL_OW, c("LITUNLIT","MOON"))
cld(lsm.ALL.temp, Letters=letters)
emm <- emmeans(Modello_mattinaL_OW,~LITUNLIT+MOON,type = "response")
pairs(emm, adjust = "tukey")
cld_res <- cld(emm, adjust = "tukey", Letters = letters, alpha = 0.05)
cld_res

#Sunset#

Modello_tramontoL_OW<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplottramontoL,family = "poisson")
car::Anova(Modello_tramontoL_OW)
check_overdispersion(Modello_tramontoL_OW)
Modello_tramontoL_OWcontrollo<-simulateResiduals(Modello_tramontoL_OW)
plot(Modello_tramontoL_OWcontrollo)
testZeroInflation(Modello_tramontoL_OW)
testOutliers(Modello_tramontoL_OW)
psundL<-ggemmeans(Modello_tramontoL_OW,terms =c("LITUNLIT"))
psunL<-plot(ggemmeans(Modello_tramontoL_OW,terms =c("LITUNLIT")))+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,3)+labs(title="Landward side",tag="b",subtitle = "(Sunset)",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )
psunL
plot(ggemmeans(Modello_tramontoL_OW,terms =c("LITUNLIT","MOON")))+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,4)+labs(title="Landward side",tag="b",subtitle="(Sunset)",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )

lsm.ALL.temp <- lsmeans(Modello_tramontoL_OW, c("LITUNLIT","MOON"))
cld(lsm.ALL.temp, Letters=letters)
emm <- emmeans(Modello_tramontoL_OW,~LITUNLIT+MOON,type = "response")
pairs(emm, adjust = "tukey")
cld_res <- cld(emm, adjust = "tukey", Letters = letters, alpha = 0.05)
cld_res

MonodontaplotNotte1L$LITUNLIT<- factor(MonodontaplotNotte1L$LITUNLIT, levels = c("UNLIT","LIT"))
Modello_Notte1L_OW<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte1L,family = "poisson")
car::Anova(Modello_Notte1L_OW)
check_overdispersion(Modello_Notte1L_OW)
Modello_Notte1L_OWcontrollo<-simulateResiduals(Modello_Notte1L_OW) 
plot(Modello_Notte1L_OWcontrollo)
testZeroInflation(Modello_Notte1L_OW)
testOutliers(Modello_Notte1L_OW)
plot(ggemmeans(Modello_Notte1L_OW,terms =c("LITUNLIT","MOON")))+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,4)+labs(title="Landward side",tag="c",subtitle="(Twilight)",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )

lsm.ALL.temp <- lsmeans(Modello_Notte1L_OW, c("LITUNLIT","MOON"))
cld(lsm.ALL.temp, Letters=letters)

Modello_Notte2L_OW<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte2L,family = "poisson")
car::Anova(Modello_Notte2L_OW)
plot(ggemmeans(Modello_Notte2L_OW,terms =c("LITUNLIT","MOON")))+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,4)+labs(title="Landward side",tag="d",subtitle="(Night)",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )
check_overdispersion(Modello_Notte2L_OW)
Modello_Notte1L_OWcontrollo<-simulateResiduals(Modello_Notte2L_OW) 
plot(Modello_Notte1L_OWcontrollo)
testZeroInflation(Modello_Notte2L_OW)
testOutliers(Modello_Notte2L_OW)
lsm.ALL.temp <- lsmeans(Modello_Notte2L_OW, c("LITUNLIT","MOON"))
cld(lsm.ALL.temp, Letters=letters)


####Repeat for Total individual in the seaward side ####
Modello_MonodontaplotmattinaS_noDaynointtemppois<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotmattinaS,family = "poisson",REML=FALSE)
car::Anova(Modello_MonodontaplotmattinaS_noDaynointtemppois)
pmatL<-plot(ggemmeans(Modello_MonodontaplotmattinaS_noDaynointtemppois,terms =c("LITUNLIT")))+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,4)+labs(title="Seaward side",tag="a",subtitle="(Morning)",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )
check_overdispersion(Modello_MonodontaplotmattinaS_noDaynointtemppois)
Modello_Notte1L_OWcontrollo<-simulateResiduals(Modello_MonodontaplotmattinaS_noDaynointtemppois) 
plot(Modello_Notte1L_OWcontrollo)
testZeroInflation(Modello_MonodontaplotmattinaS_noDaynointtemppois)
testOutliers(Modello_MonodontaplotmattinaS_noDaynointtemppois)
lsm.ALL.temp <- lsmeans(Modello_MonodontaplotmattinaS_noDaynointtemppois, c("LITUNLIT"))
cld(lsm.ALL.temp, Letters=letters)
pmatL
emm <- emmeans(Modello_MonodontaplotmattinaS_noDaynointtemppois,~LITUNLIT,type = "response")
cld_res <- cld(emm, adjust = "tukey", Letters = letters, alpha = 0.05)
cld_res
ptwdL<-ggemmeans(Modello_MonodontaplotmattinaS_noDaynointtemppois,terms =c("LITUNLIT"))
pmatL+geom_text(aes(label = cld_res$.group, y =  ptwdL$conf.high), vjust = -1)

Modello_MonodontaplottramontoS_noDaynointtemppois<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplottramontoS,family="poisson",REML = FALSE)
car::Anova(Modello_MonodontaplottramontoS_noDaynointtemppois)
pmatL<-plot(ggemmeans(Modello_MonodontaplottramontoS_noDaynointtemppois,terms =c("LITUNLIT")))+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,4)+labs(title="Seaward side",tag="b",subtitle="(Sunset)",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )
check_overdispersion(Modello_MonodontaplottramontoS_noDaynointtemppois)
Modello_Notte1L_OWcontrollo<-simulateResiduals(Modello_MonodontaplottramontoS_noDaynointtemppois) 
plot(Modello_Notte1L_OWcontrollo)
testZeroInflation(Modello_MonodontaplottramontoS_noDaynointtemppois)
testOutliers(Modello_MonodontaplottramontoS_noDaynointtemppois)
lsm.ALL.temp <- lsmeans(Modello_MonodontaplottramontoS_noDaynointtemppois, c("LITUNLIT"))
cld(lsm.ALL.temp, Letters=letters)
pmatL
emm <- emmeans(Modello_MonodontaplottramontoS_noDaynointtemppois,~LITUNLIT,type = "response")
cld_res <- cld(emm, adjust = "tukey", Letters = letters, alpha = 0.05)
cld_res
ptwdL<-ggemmeans(Modello_MonodontaplottramontoS_noDaynointtemppois,terms =c("LITUNLIT"))
pmatL+geom_text(aes(label = cld_res$.group, y =  ptwdL$conf.high), vjust = -1)

Modello_MonodontaplotNotte1S_noDaynointtemppois<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte1S,family="poisson",REML=FALSE)
car::Anova(Modello_MonodontaplotNotte1S_noDaynointtemppois)
pmatL<-plot(ggemmeans(Modello_MonodontaplotNotte1S_noDaynointtemppois,terms =c("LITUNLIT")))+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,4)+labs(title="Seaward side",tag="c",subtitle="(Twilight)",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )
check_overdispersion(Modello_MonodontaplotNotte1S_noDaynointtemppois)
Modello_Notte1L_OWcontrollo<-simulateResiduals(Modello_MonodontaplotNotte1S_noDaynointtemppois) 
plot(Modello_Notte1L_OWcontrollo)
testZeroInflation(Modello_MonodontaplotNotte1S_noDaynointtemppois)
testOutliers(Modello_MonodontaplotNotte1S_noDaynointtemppois)
lsm.ALL.temp <- lsmeans(Modello_MonodontaplotNotte1S_noDaynointtemppois, c("LITUNLIT"))
cld(lsm.ALL.temp, Letters=letters)
pmatL
emm <- emmeans(Modello_MonodontaplotNotte1S_noDaynointtemppois,~LITUNLIT,type = "response")
cld_res <- cld(emm, adjust = "tukey", Letters = letters, alpha = 0.05)
cld_res
ptwdL<-ggemmeans(Modello_MonodontaplotNotte1S_noDaynointtemppois,terms =c("LITUNLIT"))
pmatL+geom_text(aes(label = cld_res$.group, y =  ptwdL$conf.high), vjust = -1)

Modello_MonodontaplotNotte2S_noDaynointtemppois<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte2S,family="poisson",REML=FALSE)
car::Anova(Modello_MonodontaplotNotte2S_noDaynointtemppois)
pmatL<-plot(ggemmeans(Modello_MonodontaplotNotte2S_noDaynointtemppois,terms =c("LITUNLIT")))+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,4)+labs(title="Seaward side",tag="d",subtitle="(Night)",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )
check_overdispersion(Modello_MonodontaplotNotte2S_noDaynointtemppois)
Modello_Notte1L_OWcontrollo<-simulateResiduals(Modello_MonodontaplotNotte2S_noDaynointtemppois) 
plot(Modello_Notte1L_OWcontrollo)
testZeroInflation(Modello_MonodontaplotNotte2S_noDaynointtemppois)
testOutliers(Modello_MonodontaplotNotte2S_noDaynointtemppois)
lsm.ALL.temp <- lsmeans(Modello_MonodontaplotNotte2S_noDaynointtemppois, c("LITUNLIT"))
cld(lsm.ALL.temp, Letters=letters)
pmatL
emm <- emmeans(Modello_MonodontaplotNotte2S_noDaynointtemppois,~LITUNLIT,type = "response")
cld_res <- cld(emm, adjust = "tukey", Letters = letters, alpha = 0.05)
cld_res
ptwdL<-ggemmeans(Modello_MonodontaplotNotte2S_noDaynointtemppois,terms =c("LITUNLIT"))
pmatL+geom_text(aes(label = cld_res$.group, y =  ptwdL$conf.high), vjust = -1)


#####Repeat all underwater #####

Modello_mattinaL_UW<-glmmTMB(UW~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotmattinaL,family = "poisson",REML=FALSE)
car::Anova(Modello_mattinaL_UW)
check_overdispersion(Modello_mattinaL_UW)
Modello_mattinaL_UWcontrollo<-simulateResiduals(Modello_mattinaL_UW) 
plot(Modello_mattinaL_UWcontrollo)
testZeroInflation(Modello_mattinaL_UW)
testOutliers(Modello_mattinaL_UW)
pmatdL<-ggemmeans(Modello_mattinaL_UW,terms =c("LITUNLIT"))
pmatL<-plot(ggemmeans(Modello_mattinaL_UW,terms =c("LITUNLIT")))+ylim(0,3)+labs(title="Landward side",tag="a",subtitle = "(Morning)",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )
pmatL
lsm.ALL.temp <- lsmeans(Modello_mattinaL_UW, c("LITUNLIT"))
cld(lsm.ALL.temp, Letters=letters)
emm <- emmeans(Modello_mattinaL_UW,~LITUNLIT,type = "response")
pairs(emm, adjust = "tukey")
cld_res <- cld(emm, adjust = "tukey", Letters = letters, alpha = 0.05)
cld_res
ptwdL<-ggemmeans(Modello_mattinaL_UW,terms =c("LITUNLIT"))
pmatL+geom_text(aes(label = cld_res$.group, y =  ptwdL$conf.high), vjust = -1)


#
plot(ggpredict(Modello_mattinaL_UW,terms =c("Temp")))+ylim(0,4)+labs(title="Landward side",tag=,subtitle="(Morning)",x ="Temperature (°C)", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdUWn())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) ) 


#Sunset, L side
Modello_tramontoL_UW<-glmmTMB(UW~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplottramontoL,family = "poisson")
car::Anova(Modello_tramontoL_UW)
check_overdispersion(Modello_tramontoL_UW)
Modello_tramontoL_UWcontrollo<-simulateResiduals(Modello_tramontoL_UW)
plot(Modello_tramontoL_UWcontrollo)
testZeroInflation(Modello_tramontoL_UW)
testOutliers(Modello_tramontoL_UW)
psundL<-ggemmeans(Modello_tramontoL_UW,terms =c("LITUNLIT"))
psunL<-plot(ggemmeans(Modello_tramontoL_UW,terms =c("LITUNLIT")))+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,3)+labs(title="Landward side",tag="b",subtitle = "(Sunset)",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )
psunL
lsm.ALL.temp <- lsmeans(Modello_tramontoL_UW, c("LITUNLIT"))
cld(lsm.ALL.temp, Letters=letters)
emm <- emmeans(Modello_tramontoL_UW,~LITUNLIT,type = "response")
pairs(emm, adjust = "tukey")
cld_res <- cld(emm, adjust = "tukey", Letters = letters, alpha = 0.05)
cld_res
ptwdL<-ggemmeans(Modello_tramontoL_UW,terms =c("LITUNLIT"))
psunL+geom_text(aes(label = cld_res$.group, y =  ptwdL$conf.high), vjust = -1)



#Dusk, L side
MonodontaplotNotte1L$LITUNLIT<- factor(MonodontaplotNotte1L$LITUNLIT, levels = c("UNLIT","LIT"))
Modello_Notte1L_UW<-glmmTMB(UW~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte1L,family = "poisson")
car::Anova(Modello_Notte1L_UW)
plot_model(Modello_Notte1L_UW,type="int")+geom_text(label=lettere,position = position_dodge(width = 0.6),hjust=0.2, vjust=0.2, size=6,shUW.legend = FALSE)+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,4)+labs(title="Landward side",subtitle = "(Twilight)",tag="C",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdUWn())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )
check_overdispersion(Modello_Notte1L_UW)
Modello_Notte1L_UWcontrollo<-simulateResiduals(Modello_Notte1L_UW) 
plot(Modello_Notte1L_UWcontrollo)
testZeroInflation(Modello_Notte1L_UW)
testOutliers(Modello_Notte1L_UW)
ptwdL<-ggemmeans(Modello_Notte1L_UW,terms =c("LITUNLIT"))
ptwL<-plot(ggemmeans(Modello_Notte1L_UW,terms =c("LITUNLIT")))+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,3)+labs(title="Landward side",tag="c",subtitle = "(Twilight)",x="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )
ptwL


lsm.ALL.temp <- lsmeans(Modello_Notte1L_UW, c("LITUNLIT"))
cld(lsm.ALL.temp, Letters=letters)
emm <- emmeans(Modello_Notte1L_UW,~LITUNLIT,type = "response")
pairs(emm, adjust = "tukey")
cld_res <- cld(emm, adjust = "tukey", Letters = letters, alpha = 0.05)
cld_res
ptwL+geom_text(aes(label = cld_res$.group, y =  ptwdL$conf.high), vjust = -1)


#Night, L side
MonodontaplotNotte2L1<-MonodontaplotNotte2L[-49,]
Modello_Notte2L_UW<-glmmTMB(UW~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte2L1,family = "poisson")
car::Anova(Modello_Notte2L_UW)
ptnhdL<-ggemmeans(Modello_Notte2L_UW,terms =c("LITUNLIT"))
ptnhL<-plot(ggemmeans(Modello_Notte2L_UW,terms =c("LITUNLIT")))+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,3)+labs(title="Landward side",tag="d",subtitle = "(Night)",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )
ptnhL
lsm.ALL.temp <- lsmeans(Modello_Notte2L_UW, c("LITUNLIT"))
cld(lsm.ALL.temp, Letters=letters)
emm <- emmeans(Modello_Notte2L_UW,~LITUNLIT,type = "response")
pairs(emm, adjust = "tukey")
cld_res <- cld(emm, adjust = "tukey", Letters = letters, alpha = 0.05)
cld_res
ptwdL<-ggemmeans(Modello_Notte2L_UW,terms =c("LITUNLIT"))
ptnhL+geom_text(aes(label = cld_res$.group, y =  ptnhdL$conf.high), vjust = -1)
check_overdispersion(Modello_Notte2L_UW)
Modello_Notte1L_UWcontrollo<-simulateResiduals(Modello_Notte2L_UW) 
plot(Modello_Notte1L_UWcontrollo)
testZeroInflation(Modello_Notte2L_UW)
testOutliers(Modello_Notte2L_UW)










#glmm for morning data, L side of the boulders
Modello_MonodontaplotmattinaL_noDaynointtemppois<-glmmTMB(OW~Temp+LITUNLIT+MOON+LITUNLIT:MOON+(1|Zone/Boulder),MonodontaplotmattinaL,family = "poisson",REML=FALSE)
Modello_MonodontaplotmattinaL_noDaynointtemppois<-glmmTMB(OW~Temp+LITUNLIT+MOON+(1|Zone/Boulder),MonodontaplotmattinaL,family = "poisson",REML=FALSE)
summary(Modello_MonodontaplotmattinaL_noDaynointtemppois)
car::Anova(Modello_MonodontaplotmattinaL_noDaynointtemppois)
plot(ggemmeans(Modello_MonodontaplotmattinaL_noDaynointtemppois,terms =c("LITUNLIT")))+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,4)+labs(title="Landward side",tag="A",subtitle="(Morning)",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) ) 
lsm.ALL.temp <- lsmeans(Modello_MonodontaplotmattinaL_noDaynointtemppois, c("LITUNLIT","MOON"))
pino<-cld(lsm.ALL.temp, Letters=letters)
lettere<-c("ab","b","a","a")


summary(prova)
names(Monodontaplotmattina)
pairs(Monodontaplotmattina)
Modello_MonodontaplotmattinaL_noDaynointtemppois<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotmattinaL,family = "poisson",REML=FALSE)
pino<-lm(TOT~Temp+LITUNLIT+MOON,data=MonodontaplotmattinaL)
gvif(pino)
check_collinearity(Modello_MonodontaplotmattinaL_noDaynointtemppois)

#check if there is overdispersion

check_overdispersion(Modello_MonodontaplotmattinaL_noDaynointtemppois)

#analysis statistical assumptions
Modello_MonodontaplotmattinaL_noDaynointtemppoiscontrollo<-simulateResiduals(Modello_MonodontaplotmattinaL_noDaynointtemppois) 
Modello_MonodontaplotmattinaL_noDaynointtemppoiscontrollo
plot(Modello_MonodontaplotmattinaL_noDaynointtemppoiscontrollo)#ceck Q-Q plot and residual
testZeroInflation(Modello_MonodontaplotmattinaL_noDaynointtemppois)#ceck zero inflation
testOutliers(Modello_MonodontaplotmattinaL_noDaynointtemppois)#outlier test
testSpatialAutocorrelation(Modello_MonodontaplotmattinaL_noDaynointtemppoiscontrollo,x=MonodontaplotmattinaL$Temp,y=MonodontaplotmattinaL$MOON)

#check Rsquared
PredictedLmattina<-predict(Modello_MonodontaplotmattinaL_noDaynointtemppois,type="response")
ResidualsLmattina<-residuals(Modello_MonodontaplotmattinaL_noDaynointtemppois)
efronRSquared(residual = ResidualsLmattina, predicted = PredictedLmattina,statistic = "EfronRSquared")

#repeat for morning observations, S side
Modello_MonodontaplotmattinaS_noDaynointtemppois<-glmmTMB(OW~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotmattinaS,family = "poisson",REML=FALSE)
summary(Modello_MonodontaplotmattinaS_noDaynointtemppois)
car::Anova(Modello_MonodontaplotmattinaS_noDaynointtemppois)
lsm.ALL.temp <- lsmeans(Modello_MonodontaplotmattinaS_noDaynointtemppois, c("LITUNLIT","MOON"))
pino<-cld(lsm.ALL.temp, Letters=letters)
pino
plot(ggpredict(Modello_MonodontaplotmattinaS_noDaynointtemppois,terms =c("LITUNLIT","MOON")))+geom_text(label=pino$.group,position = position_dodge(width = 0.5),hjust=0.2, vjust=0.2, size=5,show.legend = FALSE)+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,4)+labs(title="Seaward side",tag="A",subtitle="(Morning)",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) ) 
check_overdispersion(Modello_MonodontaplotmattinaS_noDaynointtemppois)
Modello_MonodontaplotmattinaS_noDaynointtemppoiscontrollo<-simulateResiduals(Modello_MonodontaplotmattinaS_noDaynointtemppois)
plot(Modello_MonodontaplotmattinaS_noDaynointtemppoiscontrollo)
testZeroInflation(Modello_MonodontaplotmattinaS_noDaynointtemppois)
testOutliers(Modello_MonodontaplotmattinaS_noDaynointtemppois)
PredictedSmattina<-predict(Modello_MonodontaplotmattinaS_noDaynointtemppois,type="response")
ResidualsSmattina<-residuals(Modello_MonodontaplotmattinaS_noDaynointtemppois)
efronRSquared(residual = ResidualsSmattina, predicted = PredictedSmattina,statistic = "EfronRSquared")
lsm.ALL.temp <- lsmeans(Modello_MonodontaplotmattinaS_noDaynointtemppois, c("LITUNLIT","MOON"))
pino<-cld(lsm.ALL.temp, Letters=letters)
pino

#repeat for sunset observations, S side 
Modello_MonodontaplottramontoS_noDaynointtemppois<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplottramontoS,family="poisson",REML = FALSE)
summary(Modello_MonodontaplottramontoS_noDaynointtemppois)
car::Anova(Modello_MonodontaplottramontoS_noDaynointtemppois)
lsm.ALL.temp <- lsmeans(Modello_MonodontaplottramontoS_noDaynointtemppois, c("LITUNLIT","MOON"))
pino<-cld(lsm.ALL.temp, Letters=letters)
pino
plot(ggpredict(Modello_MonodontaplottramontoS_noDaynointtemppois,terms =c("LITUNLIT","MOON")))+geom_text(label=pino$.group,position = position_dodge(width = 0.5),hjust=0.2, vjust=0.2, size=5,show.legend = FALSE)+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,4)+labs(title="Seaward side",tag="B",subtitle="(Sunset)",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) ) 
check_overdispersion(Modello_MonodontaplottramontoS_noDaynointtemppois)
Modello_MonodontaplottramontoS_noDaynointtemppoiscontrollo<-simulateResiduals(Modello_MonodontaplottramontoS_noDaynointtemppois)
plot(Modello_MonodontaplottramontoS_noDaynointtemppoiscontrollo)
testZeroInflation(Modello_MonodontaplottramontoS_noDaynointtemppois)
testOutliers(Modello_MonodontaplottramontoS_noDaynointtemppois)
PredictedStramonto<-predict(Modello_MonodontaplottramontoS_noDaynointtemppois,type="response")
ResidualsStramonto<-residuals(Modello_MonodontaplottramontoS_noDaynointtemppois)

efronRSquared(residual = ResidualsStramonto, predicted = PredictedStramonto,statistic = "EfronRSquared")

#repeat for sunset observations, L side of boulders
Modello_MonodontaplottramontoL_noDaynointtemppois<-glmmTMB(OW~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplottramontoL,family="poisson",REML=FALSE)
Modello_MonodontaplottramontoL_noDaynointtemppois2<-glmmTMB(TOT~Temp+LITUNLIT+MOON+(1|Zone/Boulder),MonodontaplottramontoL,family="poisson",REML=FALSE)
summary(Modello_MonodontaplottramontoL_noDaynointtemppois)
car::Anova(Modello_MonodontaplottramontoL_noDaynointtemppois,type = "II")
plot(ggemmeans(Modello_MonodontaplottramontoL_noDaynointtemppois,terms =c("LITUNLIT")))+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,4)+labs(title="Landward side",tag="A",subtitle="(Morning)",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) ) 
lsm.ALL.temp <- lsmeans(Modello_MonodontaplottramontoL_noDaynointtemppois,c("LITUNLIT","MOON"),type = "response")
cld(lsm.ALL.temp, Letters=letters)
plot(ggpredict(Modello_MonodontaplottramontoL_noDaynointtemppois,terms =c("LITUNLIT","MOON")))+geom_text(label=lettere,position = position_dodge(width = 0.6),hjust=0.2, vjust=0.2, size=6,show.legend = FALSE)+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,4)+labs(title="Landward side",tag="B",subtitle="(Sunset)",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) ) 
plot(ggpredict(Modello_MonodontaplottramontoL_noDaynointtemppois,terms =c("Temp")))+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,4)+labs(title="Landward side",tag="B",subtitle = "(Sunset)",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )
lsm.ALL.temp <- lsmeans(Modello_MonodontaplottramontoL_noDaynointtemppois,c("LITUNLIT","MOON"),type = "response")
cld(lsm.ALL.temp, Letters=letters)
check_collinearity()
emm <- emmeans(Modello_MonodontaplottramontoL_noDaynointtemppois,~LITUNLIT+MOON,type = "response")
pairs(emm, adjust = "tukey")
cld_res <- cld(emm, adjust = "tukey", Letters = letters, alpha = 0.05)

cld_res

check_overdispersion(Modello_MonodontaplottramontoL_noDaynointtemppois)
Modello_MonodontaplottramontoL_noDaynointtemppoiscontrollo<-simulateResiduals(Modello_MonodontaplottramontoL_noDaynointtemppois)
plot(Modello_MonodontaplottramontoL_noDaynointtemppoiscontrollo)
testZeroInflation(Modello_MonodontaplottramontoL_noDaynointtemppois)
testOutliers(Modello_MonodontaplottramontoL_noDaynointtemppois)
PredictedLtramonto<-predict(Modello_MonodontaplottramontoL_noDaynointtemppois,type="response")
ResidualsLtramonto<-residuals(Modello_MonodontaplottramontoL_noDaynointtemppois)
efronRSquared(residual = ResidualsLtramonto, predicted = PredictedLtramonto,statistic = "EfronRSquared")

Modello_MonodontaplottramontoL_prova<-glmmTMB(TOT~MOON+Day+LITUNLIT+(1|Zone/Boulder),MonodontaplottramontoL,family="poisson",REML=FALSE)
car::Anova(Modello_MonodontaplottramontoL_prova)
plot(ggpredict(Modello_MonodontaplottramontoL_prova,terms =c("LITUNLIT","Day")))


#repeat for twilight observations, S side of boulders
Modello_MonodontaplotNotte1S_noDaynointtemppois<-glmmTMB(OW~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte1S,family="poisson",REML=FALSE)
summary(Modello_MonodontaplotNotte1S_noDaynointtemppois)
car::Anova(Modello_MonodontaplotNotte1S_noDaynointtemppois)
plot_model(Modello_MonodontaplotNotte1S_noDaynointtemppois,type="int")+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,4)+labs(title="Seaward side",tag="C",subtitle="(Twilight)",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )
check_overdispersion(Modello_MonodontaplotNotte1S_noDaynointtemppois)
testZeroInflation(Modello_MonodontaplotNotte1S_noDaynointtemppois)
testOutliers(Modello_MonodontaplotNotte1S_noDaynointtemppois)
Modello_MonodontaplotNotte1S_noDaynointtemppoiscontrollo<-simulateResiduals(Modello_MonodontaplotNotte1S_noDaynointtemppois)
plot(Modello_MonodontaplotNotte1S_noDaynointtemppoiscontrollo)

PredictedSNotte1<-predict(Modello_MonodontaplotNotte1S_noDaynointtemppois,type="response")
ResidualsSNotte1<-residuals(Modello_MonodontaplotNotte1S_noDaynointtemppois)

efronRSquared(residual = ResidualsSNotte1, predicted = PredictedSNotte1,statistic = "EfronRSquared")

#repeat for twilight observations, L side of boulders
MonodontaplotNotte1L$LITUNLIT<- factor(MonodontaplotNotte1L$LITUNLIT, levels = c("UNLIT","LIT"))
Modello_MonodontaplotNotte1L_noDaynointtemppois<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte1L,family="poisson",REML=FALSE)
summary(Modello_MonodontaplotNotte1L_noDaynointtemppois)
car::Anova(Modello_MonodontaplotNotte1L_noDaynointtemppois,)
lsm.ALL.temp <- lsmeans(Modello_MonodontaplotNotte1L_noDaynointtemppois,c("MOON*LITUNLIT"),type = "response")
cld(lsm.ALL.temp, Letters=letters)
lettereA<-c("ab","b","ab","b")

plot(ggpredict(Modello_MonodontaplotNotte1L_noDaynointtemppois,terms =c("LITUNLIT","MOON")))+geom_text(label=lettereA,position = position_dodge(width = 0.6),hjust=0.2, vjust=0.2, size=6,show.legend = FALSE)+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,4)+labs(title="Landward side",tag="C",subtitle="(Twilight)",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )
check_overdispersion(Modello_MonodontaplotNotte1L_noDaynointtemppois)
Modello_MonodontaplotNotte1LL_noDaynointtemppoiscontrollo<-simulateResiduals(Modello_MonodontaplotNotte1L_noDaynointtemppois)
plot(Modello_MonodontaplotNotte1LL_noDaynointtemppoiscontrollo)
testZeroInflation(Modello_MonodontaplotNotte1L_noDaynointtemppois)
testOutliers(Modello_MonodontaplotNotte1L_noDaynointtemppois)
PredictedLNotte1<-predict(Modello_MonodontaplotNotte1L_noDaynointtemppois,type="response")
ResidualsLNotte1<-residuals(Modello_MonodontaplotNotte1L_noDaynointtemppois)
plot(ggpredict(Modello_MonodontaplotNotte1L_noDaynointtemppois,terms =c("LITUNLIT","MOON")))+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,4)+labs(title="Landward side",tag="B",subtitle = "(Sunset)",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )
lsm.ALL.temp <- lsmeans(Modello_MonodontaplotNotte1L_noDaynointtemppois, c("LITUNLIT","MOON"))
cld(lsm.ALL.temp, Letters=letters)
check_collinearity(Modello_MonodontaplotNotte1L_noDaynointtemppois)
emm <- emmeans(Modello_MonodontaplotNotte1L_noDaynointtemppois,~MOON*LITUNLIT,type = "response")
pairs(emm, adjust = "sidak")
cld_res <- cld(emm, adjust = "sidak", Letters = letters, alpha = 0.05)
cld_res

efronRSquared(residual = ResidualsLNotte1, predicted = PredictedLNotte1,statistic = "EfronRSquared")

#repeat for night observations, L side of boulders

Modello_MonodontaplotNotte2L_noDaynointtemppois<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte2L,family="poisson",REML=FALSE)
Modello_MonodontaplotNotte2L_noDaynointtemppois1<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte2L,family="poisson",REML=FALSE)

summary(Modello_MonodontaplotNotte2L_noDaynointtemppois)
car::Anova(Modello_MonodontaplotNotte2L_noDaynointtemppois,type = "II")
car::Anova(Modello_MonodontaplotNotte2L_noDaynointtemppois)
lsm.ALL.temp <- lsmeans(Modello_MonodontaplotNotte2L_noDaynointtemppois,c("LITUNLIT","MOON"),type = "response")
cld(lsm.ALL.temp, Letters=letters)
plot(ggpredict(Modello_MonodontaplotNotte2L_noDaynointtemppois,terms =c("LITUNLIT","MOON")))+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,4)+labs(title="Landward side",tag="B",subtitle = "(Sunset)",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )
lsm.ALL.temp <- lsmeans(Modello_MonodontaplotNotte2L_noDaynointtemppois, c("LITUNLIT","MOON"))
cld(lsm.ALL.temp, Letters=letters)
plot(ggpredict(Modello_MonodontaplotNotte2L_noDaynointtemppois,terms =c("LITUNLIT","MOON")))+geom_text(label=lettere,position = position_dodge(width = 0.6),hjust=0.2, vjust=0.2, size=6,show.legend = FALSE)+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,4)+labs(title="Landward side",tag="D",subtitle="(Night)",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )
check_overdispersion(Modello_MonodontaplotNotte2L_noDaynointtemppois)
Modello_MonodontaplotNotte2LL_noDaynointtemppoiscontrollo<-simulateResiduals(Modello_MonodontaplotNotte2L_noDaynointtemppois)
plot(Modello_MonodontaplotNotte2LL_noDaynointtemppoiscontrollo)
testZeroInflation(Modello_MonodontaplotNotte2L_noDaynointtemppois)
testOutliers(Modello_MonodontaplotNotte2L_noDaynointtemppois)
PredictedLNotte2<-predict(Modello_MonodontaplotNotte2L_noDaynointtemppois,type="response")
ResidualsLNotte2<-residuals(Modello_MonodontaplotNotte2L_noDaynointtemppois)
emm <- emmeans(Modello_MonodontaplotNotte2L_noDaynointtemppois,~LITUNLIT+MOON,type = "response")
pairs(emm, adjust = "tukey")
cld_res <- cld(emm, adjust = "tukey", Letters = letters, alpha = 0.05)
cld_res
outliers(Modello_MonodontaplotNotte2LL_noDaynointtemppoiscontrollo)
efronRSquared(residual = ResidualsLNotte2, predicted = PredictedLNotte2,statistic = "EfronRSquared")

#repeat for night observations, S side of boulders
Modello_MonodontaplotNotte2S_noDaynointtemppois<-glmmTMB(OW~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte2S,family="poisson",REML=FALSE)
summary(Modello_MonodontaplotNotte2S_noDaynointtemppois)
car::Anova(Modello_MonodontaplotNotte2S_noDaynointtemppois)
plot_model(Modello_MonodontaplotNotte2S_noDaynointtemppois,type="int")+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+labs(title="Seaward side",tag="D",subtitle="(Night)",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )
check_overdispersion(Modello_MonodontaplotNotte2S_noDaynointtemppois)
Modello_MonodontaplotNotte2S_noDaynointtemppoiscontrollo<-simulateResiduals(Modello_MonodontaplotNotte2S_noDaynointtemppois)
plot(Modello_MonodontaplotNotte2S_noDaynointtemppoiscontrollo)
testZeroInflation(Modello_MonodontaplotNotte2S_noDaynointtemppois)
testOutliers(Modello_MonodontaplotNotte2S_noDaynointtemppois)
PredictedSNotte2<-predict(Modello_MonodontaplotNotte2S_noDaynointtemppois,type="response")
ResidualsSNotte2<-residuals(Modello_MonodontaplotNotte2S_noDaynointtemppois)

efronRSquared(residual = ResidualsSNotte2, predicted = PredictedSNotte2,statistic = "EfronRSquared")

####TEST AW####
#repeat the same script in precedent row, but this time the dependent variable is the number of individuals over the water level
#Morning, L side
Modello_mattinaL_OW<-glmmTMB(OW~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotmattinaL,family = "poisson",REML=FALSE)
summary(Modello_mattinaL_OW)
car::Anova(Modello_mattinaL_OW)
plot_model(Modello_mattinaL_OW,type="int")+geom_text(label=lettereA,position = position_dodge(width = 0.6),hjust=0.2, vjust=0.2, size=6,show.legend = FALSE)+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,4)+labs(title="Landward side",subtitle = "(Morning)",tag="A",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )
check_overdispersion(Modello_mattinaL_OW)
Modello_mattinaL_OWcontrollo<-simulateResiduals(Modello_mattinaL_OW) 
plot(Modello_mattinaL_OWcontrollo)
testZeroInflation(Modello_mattinaL_OW)
testOutliers(Modello_mattinaL_OW)
PredictedLmattinaOW<-predict(Modello_mattinaL_OW,type="response")
ResidualsLmattinaOW<-residuals(Modello_mattinaL_OW)
efronRSquared(residual = ResidualsLmattinaOW, predicted = PredictedLmattinaOW,statistic = "EfronRSquared")
plot(ggpredict(Modello_mattinaL_OW,terms =c("Temp","LITUNLIT")))+ylim(0,4)+labs(title="Seaward side",tag="B",subtitle="(Sunset)",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) ) 
pmatdL<-ggemmeans(Modello_mattinaL_OW,terms =c("LITUNLIT"))
pmatL<-plot(ggemmeans(Modello_mattinaL_OW,terms =c("LITUNLIT")))+ylim(0,3)+labs(title="Landward side",tag="a",subtitle = "(Morning)",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )
pmatL+geom_text(aes(label = cld_res$.group, y =  pmatdL$conf.high), vjust = -1)

pmatdL<-ggemmeans(Modello_mattinaL_OW,terms =c("LITUNLIT"))

gplot(emm_dfOWML,aes(x=LITUNLIT,y=rate))+geom_point(shape = 15, size = 3, color = "blue")+geom_text(label=cld_res$.group,position = position_dodge(width = 0.8),hjust=2, vjust=0.2, size=6,show.legend = FALSE)+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,4)+labs(title="Landward side",tag="A",subtitle = "(Morning)",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )+geom_errorbar(aes(ymin = emm_dfOWML$asymp.LCL, ymax = emm_dfOWML$asymp.UCL),color="blue",width = 0.1,linewidth = 1)+

#Estimated Marginal Means (medie marginali stimate),sono medie marginali stimate
plot(effect("LITUNLIT", Modello_mattinaL_OW))
lsm.ALL.temp <- lsmeans(Modello_mattinaL_OW, c("LITUNLIT"))
cld(lsm.ALL.temp, Letters=letters)
pino<-cld(lsm.ALL.temp, Letters=letters)
lettere<-c("ab","b","a","a")
emmOWML <- emmeans(Modello_mattinaL_OW,~LITUNLIT,type = "response")
emm_dfOWML <- as.data.frame(emmOWML)
plot(emm,comparison=TRUE)
pmL<-plot(emmOWML,comparison=TRUE,horizontal = FALSE,CIs = TRUE,col = list(comparison = "black",estimate = "blue",CIs = "red"),lwd = 4)
pmL+geom_text(label=cld_res$.group,position = position_dodge(width = 0.8),hjust=2, vjust=0.2, size=6,show.legend = FALSE)+xlim(0,3)+labs(title="Landward side",tag="A",subtitle = "(Morning)", x="Mean number of *P.turbinatus*",y ="ALAN")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))
contr_df <- as.data.frame(pairs(emm))



pairs(emm, adjust = "sidak")
cld_res <- cld(emmOWML , adjust = "tukey", Letters = letters, alpha = 0.05)
cld_res

#Sunset, L side
Modello_tramontoL_OW<-glmmTMB(OW~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplottramontoL,family = "poisson")
summary(Modello_tramontoL_OW)
car::Anova(Modello_tramontoL_OW)
plot_model(Modello_tramontoL_OW,type="int")+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,4)+labs(title="Landward side",subtitle = "(Sunset)",tag="B",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )
check_overdispersion(Modello_tramontoL_OW)
Modello_tramontoL_OWcontrollo<-simulateResiduals(Modello_tramontoL_OW)
plot(Modello_tramontoL_OWcontrollo)
testZeroInflation(Modello_tramontoL_OW)
testOutliers(Modello_tramontoL_OW)
PredictedLtramontoOW<-predict(Modello_tramontoL_OW,type="response")
ResidualsLtramontoOW<-residuals(Modello_tramontoL_OW)
efronRSquared(residual = ResidualsLtramontoOW, predicted = PredictedLtramontoOW,statistic = "EfronRSquared")
psundL<-ggemmeans(Modello_tramontoL_OW,terms =c("LITUNLIT"))
psunL<-plot(ggemmeans(Modello_tramontoL_OW,terms =c("LITUNLIT")))+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,3)+labs(title="Landward side",tag="b",subtitle = "(Sunset)",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )
psunL+geom_text(aes(label = cld_res$.group, y =  psundL$conf.high), vjust = -1)

plot(effect("LITUNLIT", Modello_tramontoL_OW))

plot(ggpredict(Modello_tramontoL_OW,terms =c("LITUNLIT","MOON")))+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,4)+labs(title="Landward side",tag="B",subtitle = "(Sunset)",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )
ggplot(emm_dfOWSL,aes(x=LITUNLIT,y=rate))+geom_point(shape = 15, size = 3, color = "blue")+geom_text(label=cld_res$.group,position = position_dodge(width = 0.8),hjust=2, vjust=0.2, size=6,show.legend = FALSE)+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,4)+labs(title="Landward side",tag="B",subtitle = "(Sunset)",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )+geom_errorbar(aes(ymin = emm_dfOWSL$asymp.LCL, ymax = emm_dfOWSL$asymp.UCL),color="blue",width = 0.1,linewidth = 1) 
lsm.ALL.temp <- lsmeans(Modello_tramontoL_OW, c("LITUNLIT"))
cld(lsm.ALL.temp, Letters=letters)
emmOWSL <- emmeans(Modello_tramontoL_OW,~LITUNLIT,type = "response")
emm_dfOWSL <- as.data.frame(emm)
pSL<-plot(emmOWSL,comparison=TRUE,horizontal = FALSE,CIs = TRUE,col = list(comparison = "black",estimate = "blue",CIs = "red"),lwd = 4)
pSL+geom_text(label=cld_res$.group,position = position_dodge(width = 0.8),hjust=2, vjust=0.2, size=6,show.legend = FALSE)+xlim(0,3)+labs(title="Landward side",tag="B",subtitle = "(Sunset)", x="Mean number of *P.turbinatus*",y ="ALAN")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))
pSL$layers

#Dusk, L side
MonodontaplotNotte1L$LITUNLIT<- factor(MonodontaplotNotte1L$LITUNLIT, levels = c("UNLIT","LIT"))
Modello_Notte1L_OW<-glmmTMB(OW~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte1L,family = "poisson")
summary(Modello_Notte1L_OW)
car::Anova(Modello_Notte1L_OW)
plot_model(Modello_Notte1L_OW,type="int")+geom_text(label=lettere,position = position_dodge(width = 0.6),hjust=0.2, vjust=0.2, size=6,show.legend = FALSE)+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,4)+labs(title="Landward side",subtitle = "(Twilight)",tag="C",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )
check_overdispersion(Modello_Notte1L_OW)
Modello_Notte1L_OWcontrollo<-simulateResiduals(Modello_Notte1L_OW) 
plot(Modello_Notte1L_OWcontrollo)
testZeroInflation(Modello_Notte1L_OW)
testOutliers(Modello_Notte1L_OW)
PredictedLNotte1OW<-predict(Modello_Notte1L_OW,type="response")
ResidualsLNotte1OW<-residuals(Modello_Notte1L_OW)
efronRSquared(residual = ResidualsLNotte1OW, predicted = PredictedLNotte1OW,statistic = "EfronRSquared")
ptwdL<-ggemmeans(Modello_Notte1L_OW,terms =c("LITUNLIT"))
ptwL<-plot(ggemmeans(Modello_Notte1L_OW,terms =c("LITUNLIT")))+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,3)+labs(title="Landward side",tag="c",subtitle = "(Twilight)",x="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )
ptwL+geom_text(aes(label = cld_res$.group, y =  ptwdL$conf.high), vjust = -1)

lsm.ALL.temp <- lsmeans(Modello_Notte1L_OW, c("LITUNLIT"))
cld(lsm.ALL.temp, Letters=letters)
emm <- emmeans(Modello_Notte1L_OW,~LITUNLIT,type = "response")
pairs(emm, adjust = "tukey")
cld_res <- cld(emm, adjust = "tukey", Letters = letters, alpha = 0.05)
cld_res
emmOWTL <- emmeans(Modello_Notte1L_OW,~LITUNLIT,type = "response")
emm_dfOWTL <- as.data.frame(emm)
ggplot(emm_dfOWTL,aes(x=LITUNLIT,y=rate))+geom_point(shape = 15, size = 3, color = "blue")+geom_text(label=cld_res$.group,position = position_dodge(width = 0.8),hjust=2, vjust=0.2, size=6,show.legend = FALSE)+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,4)+labs(title="Landward side",tag="C",subtitle = "(Twilight)",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )+geom_errorbar(aes(ymin = emm_dfOWTL$asymp.LCL, ymax = emm_dfOWTL$asymp.UCL),color="blue",width = 0.1,linewidth =1) 
MonodontaplotNotte1L<- within(MonodontaplotNotte1L,LITUNLIT<-relevel(LITUNLIT,ref = "UNLIT"))
lettere<-c("ab","b","ab","a")

pTL<-plot(emmOWTL,comparison=TRUE,horizontal = FALSE,CIs = TRUE,col = list(comparison = "black",estimate = "blue",CIs = "red"),lwd = 4)
pTL+geom_text(label=cld_res$.group,position = position_dodge(width = 0.8),hjust=2, vjust=0.2, size=6,show.legend = FALSE)+xlim(0,3)+labs(title="Landward side",tag="C",subtitle = "(Twilight)",x ="Mean number of *P.turbinatus*",y ="ALAN")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))

#Night, L side
MonodontaplotNotte2L1<-MonodontaplotNotte2L[-49,]
Modello_Notte2L_OW<-glmmTMB(OW~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte2L1,family = "poisson")
summary(Modello_Notte2L_OW)
car::Anova(Modello_Notte2L_OW)

plot_model(Modello_Notte2L_OW,type="int")+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,4)+theme(text = element_text(size = 15))+labs(title="Landward side",subtitle = "(Night)",tag="D",x ="ALAN", y="Mean number of *P.turbinatus* OW")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )
ptnhdL<-ggemmeans(Modello_Notte1L_OW,terms =c("LITUNLIT"))
ptnhL<-plot(ggemmeans(Modello_Notte2L_OW,terms =c("LITUNLIT")))+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,3)+labs(title="Landward side",tag="d",subtitle = "(Night)",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )
ptnhL
ptnhL+geom_text(aes(label = cld_res$.group, y =  ptnhdL$conf.high), vjust = -1)

lsm.ALL.temp <- lsmeans(Modello_Notte2L_OW, c("LITUNLIT","MOON"))
cld(lsm.ALL.temp, Letters=letters)
check_overdispersion(Modello_Notte2L_OW)
Modello_Notte2L_OWcontrollo<-simulateResiduals(Modello_Notte2L_OW)
plot(Modello_Notte2L_OWcontrollo)
testZeroInflation(Modello_Notte2L_OW)
testOutliers(Modello_Notte2L_OW)
PredictedLNotte2OW<-predict(Modello_Notte2L_OW,type="response")
ResidualsLNotte2OW<-residuals(Modello_Notte2L_OW)
efronRSquared(residual = ResidualsLNotte2OW, predicted = PredictedLNotte2OW,statistic = "EfronRSquared")
lsm.ALL.temp <- lsmeans(Modello_Notte2L_OW, c("LITUNLIT","MOON"))
cld(lsm.ALL.temp, Letters=letters)

pairs(emm, adjust = "tukey")
pNL<-plot(emmOWNL,comparison=TRUE,horizontal = FALSE,CIs = TRUE,col = list(comparison = "black",estimate = "blue",CIs = "red"),lwd = 4)
pNL

cld_res <- cld(emm, adjust = "tukey", Letters = letters, alpha = 0.05)
cld_res
outliers(Modello_Notte2L_OWcontrollo)
MonodontaplotNotte2L1<-MonodontaplotNotte2L[-49,]
Modello_Notte2L_OW<-glmmTMB(OW~Temp+LITUNLIT+MOON+LITUNLIT:MOON+(1|Zone/Boulder),MonodontaplotNotte2L1,family = "poisson")
emmOWNL <- emmeans(Modello_Notte2L_OW,~LITUNLIT,type = "response")
emm_dfOWNL <- as.data.frame(emm)
ggplot(emm_dfOWNL,aes(x=LITUNLIT,y=rate))+geom_point(shape = 15, size = 3, color = "blue")+geom_text(label=cld_res$.group,position = position_dodge(width = 0.8),hjust=2, vjust=0.2, size=6,show.legend = FALSE)+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,4)+labs(title="Landward side",tag="D",subtitle = "(Night)",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )+geom_errorbar(aes(ymin = emm_dfOWNL$asymp.LCL, ymax = emm_dfOWNL$asymp.UCL),color="blue",width = 0.1,linewidth =1) 
pNL<-plot(emmOWNL,comparison=TRUE,horizontal = FALSE,CIs = TRUE,col = list(comparison = "black",estimate = "blue",CIs = "red"),lwd = 4)
pNL+geom_text(label=cld_res$.group,position = position_dodge(width = 0.8),hjust=2, vjust=0.2, size=6,show.legend = FALSE)+xlim(0,3)+labs(title="Landward side",tag="D",subtitle = "(Night)",x ="Mean number of *P.turbinatus*",y ="ALAN")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))
####TEST BW####
#repeat the same script in precedent row, but this time the dependent variable is the number of individuals under the water level
#Morning, L side
Modello_mattinaL_UW<-glmmTMB(UW~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotmattinaL,family = "poisson")
summary(Modello_mattinaL_UW)
car::Anova(Modello_mattinaL_UW)
plot_model(Modello_mattinaL_UW,type="int")+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,4)+labs(title="Landward side",subtitle = "(Morning)",tag="A",x ="ALAN", y="Mean number of *P.turbinatus* UW")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )
check_overdispersion(Modello_mattinaL_UW)
Modello_mattinaL_UWcontrollo<-simulateResiduals(Modello_mattinaL_UW) 
plot(Modello_mattinaL_UWcontrollo)
testOutliers(Modello_mattinaL_UW)
testZeroInflation(Modello_mattinaL_UW)
PredictedLmattinaUW<-predict(Modello_mattinaL_UW,type="response")
ResidualsLmattinaUW<-residuals(Modello_mattinaL_UW)
efronRSquared(residual = ResidualsLmattinaUW, predicted = PredictedLmattinaUW,statistic = "EfronRSquared")
plot(ggpredict(Modello_mattinaL_UW,terms =c("LITUNLIT")))+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,4)+labs(title="Landward side",tag="B",subtitle = "(Sunset)",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )

#Sunset, L side
Modello_tramontoL_UW<-glmmTMB(UW~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplottramontoL,family = "poisson")
summary(Modello_tramontoL_UW)
car::Anova(Modello_tramontoL_UW)
plot_model(Modello_tramontoL_UW,type="int")+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,4)+labs(title="Landward side",subtitle = "(Sunset)",tag="B",x ="ALAN", y="Mean number of *P.turbinatus* UW")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )
check_overdispersion(Modello_tramontoL_UW)
Modello_tramontoL_UWcontrollo<-simulateResiduals(Modello_tramontoL_UW)
plot(Modello_tramontoL_UWcontrollo)
testOutliers(Modello_tramontoL_UW)
testZeroInflation(Modello_tramontoL_UW)
PredictedLtramontoUW<-predict(Modello_tramontoL_UW,type="response")
ResidualsLtramontoUW<-residuals(Modello_tramontoL_UW)
efronRSquared(residual = ResidualsLtramontoUW, predicted = PredictedLtramontoUW,statistic = "EfronRSquared")
plot(ggpredict(Modello_tramontoL_UW,terms =c("LITUNLIT")))+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,4)+labs(title="Landward side",tag="B",subtitle = "(Sunset)",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )
#Dusk, L side
Modello_Notte1L_UW<-glmmTMB(UW~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte1L,family = "poisson")
summary(Modello_Notte1L_UW)
car::Anova(Modello_Notte1L_UW)
plot_model(Modello_Notte1L_UW,type="int")+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,4)+labs(title="Landward side",subtitle = "(Twilight)",tag="C",x ="ALAN", y="Mean number of *P.turbinatus* UW")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )
check_overdispersion(Modello_Notte1L_UW)
Modello_Notte1L_UWcontrollo<-simulateResiduals(Modello_Notte1L_UW)
plot(Modello_Notte1L_UWcontrollo)
testOutliers(Modello_Notte1L_UW)
testZeroInflation(Modello_Notte1L_UW)
PredictedLNotte1UW<-predict(Modello_Notte1L_UW,type="response")
ResidualsLNotte1UW<-residuals(Modello_Notte1L_UW)
efronRSquared(residual = ResidualsLNotte1UW, predicted = PredictedLNotte1UW,statistic = "EfronRSquared")
plot(ggpredict(Modello_Notte1L_UW,terms =c("LITUNLIT")))+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,4)+labs(title="Landward side",tag="B",subtitle = "(Sunset)",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )
#Night, L side
Modello_Notte2L_UW<-glmmTMB(UW~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte2L,family = "poisson")
summary(Modello_Notte2L_UW)
car::Anova(Modello_Notte2L_UW)
plot_model(Modello_Notte2L_UW,type="int")+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,4)+labs(title="Landward side",subtitle = "(Night)",tag="D",x ="ALAN", y="Mean number of *P.turbinatus* UW")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )
check_overdispersion(Modello_Notte2L_UW)
Modello_Notte2L_UWcontrollo<-simulateResiduals(Modello_Notte2L_UW)
plot(Modello_Notte2L_UWcontrollo)
testOutliers(Modello_Notte2L_UW)
testZeroInflation(Modello_Notte2L_UW)
PredictedLNotte2UW<-predict(Modello_Notte2L_UW,type="response")
ResidualsLNotte2UW<-residuals(Modello_Notte2L_UW)
efronRSquared(residual = ResidualsLNotte2UW, predicted = PredictedLNotte2UW,statistic = "EfronRSquared")
plot(ggpredict(Modello_Notte2L_UW,terms =c("LITUNLIT")))+scale_color_manual(name = "Moon", labels = c("New", "Full"),values=c('red','blue'))+ylim(0,4)+labs(title="Landward side",tag="B",subtitle = "(Sunset)",x ="ALAN", y="Mean number of *P.turbinatus*")+theme_light(base_size =15)+theme(text = element_text(size = 15),axis.title.y = ggtext::element_markdown())+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5) )

####passage for choose the best model, analysis changing the fixed part whit AIC and anova####
#write all model and test the AIC
#Morning, S side

Modello_MonodontaplotmattinaS_1<-glmmTMB(TOT~Temp+MOON+LITUNLIT+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotmattinaS,family="poisson",REML=FALSE)
Modello_MonodontaplotmattinaS_2<-glmmTMB(TOT~MOON+LITUNLIT+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotmattinaS,family="poisson",REML=FALSE)#parte
Modello_MonodontaplotmattinaS_3<-glmmTMB(TOT~Temp+(1|Zone/Boulder),MonodontaplotmattinaS,family="poisson",REML=FALSE)
Modello_MonodontaplotmattinaS_4<-glmmTMB(TOT~Temp+MOON+(1|Zone/Boulder),MonodontaplotmattinaS,family="poisson",REML=FALSE)
Modello_MonodontaplotmattinaS_null<-glmmTMB(TOT~(1|Zone/Boulder),MonodontaplotmattinaS,family="poisson",REML=FALSE)
Modello_MonodontaplotmattinaS_5<-glmmTMB(TOT~Temp+MOON+LITUNLIT+(1|Zone/Boulder),MonodontaplotmattinaS,family="poisson",REML=FALSE)
Modello_MonodontaplotmattinaS_6<-glmmTMB(TOT~Temp+LITUNLIT+(1|Zone/Boulder),MonodontaplotmattinaS,family="poisson",REML=FALSE)
Modello_MonodontaplotmattinaS_7<-glmmTMB(TOT~LITUNLIT+(1|Zone/Boulder),MonodontaplotmattinaS,family="poisson",REML=FALSE)
Modello_MonodontaplotmattinaS_8<-glmmTMB(TOT~MOON+(1|Zone/Boulder),MonodontaplotmattinaS,family="poisson",REML=FALSE)
Modello_MonodontaplotmattinaS_9<-glmmTMB(TOT~MOON+LITUNLIT+(1|Zone/Boulder),MonodontaplotmattinaS,family="poisson",REML=FALSE)
AIC(Modello_MonodontaplotmattinaS_1,Modello_MonodontaplotmattinaS_2,Modello_MonodontaplotmattinaS_3,Modello_MonodontaplotmattinaS_4,Modello_MonodontaplotmattinaS_5,Modello_MonodontaplotmattinaS_6,Modello_MonodontaplotmattinaS_7,Modello_MonodontaplotmattinaS_8,Modello_MonodontaplotmattinaS_9,Modello_MonodontaplotmattinaS_null)


#Morning,L side
Modello_MonodontaplotmattinaL_1<-glmmTMB(TOT~Temp+MOON+LITUNLIT+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotmattinaL,family="poisson",REML=FALSE)
Modello_MonodontaplotmattinaL_2<-glmmTMB(TOT~MOON+LITUNLIT+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotmattinaL,family="poisson",REML=FALSE)#parte
Modello_MonodontaplotmattinaL_3<-glmmTMB(TOT~Temp+(1|Zone/Boulder),MonodontaplotmattinaL,family="poisson",REML=FALSE)
Modello_MonodontaplotmattinaL_4<-glmmTMB(TOT~Temp+MOON+(1|Zone/Boulder),MonodontaplotmattinaL,family="poisson",REML=FALSE)
Modello_MonodontaplotmattinaL_null<-glmmTMB(TOT~(1|Zone/Boulder),MonodontaplotmattinaL,family="poisson",REML=FALSE)
Modello_MonodontaplotmattinaL_5<-glmmTMB(TOT~Temp+MOON+LITUNLIT+(1|Zone/Boulder),MonodontaplotmattinaL,family="poisson",REML=FALSE)
Modello_MonodontaplotmattinaL_6<-glmmTMB(TOT~Temp+LITUNLIT+(1|Zone/Boulder),MonodontaplotmattinaL,family="poisson",REML=FALSE)
Modello_MonodontaplotmattinaL_7<-glmmTMB(TOT~LITUNLIT+(1|Zone/Boulder),MonodontaplotmattinaL,family="poisson",REML=FALSE)
Modello_MonodontaplotmattinaL_8<-glmmTMB(TOT~MOON+(1|Zone/Boulder),MonodontaplotmattinaL,family="poisson",REML=FALSE)
Modello_MonodontaplotmattinaL_9<-glmmTMB(TOT~MOON+LITUNLIT+(1|Zone/Boulder),MonodontaplotmattinaL,family="poisson",REML=FALSE)
AIC(Modello_MonodontaplotmattinaL_1,Modello_MonodontaplotmattinaL_2,Modello_MonodontaplotmattinaL_3,Modello_MonodontaplotmattinaL_4,Modello_MonodontaplotmattinaL_5,Modello_MonodontaplotmattinaL_6,Modello_MonodontaplotmattinaL_7,Modello_MonodontaplotmattinaL_8,Modello_MonodontaplotmattinaL_9,Modello_MonodontaplotmattinaL_null)


#Sunset, S side
Modello_MonodontaplottramontoS_1<-glmmTMB(TOT~Temp+MOON+LITUNLIT+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplottramontoS,family="poisson",REML=FALSE)
Modello_MonodontaplottramontoS_2<-glmmTMB(TOT~MOON+LITUNLIT+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplottramontoS,family="poisson",REML=FALSE)#parte
Modello_MonodontaplottramontoS_3<-glmmTMB(TOT~Temp+(1|Zone/Boulder),MonodontaplottramontoS,family="poisson",REML=FALSE)
Modello_MonodontaplottramontoS_4<-glmmTMB(TOT~Temp+MOON+(1|Zone/Boulder),MonodontaplottramontoS,family="poisson",REML=FALSE)
Modello_MonodontaplottramontoS_null<-glmmTMB(TOT~(1|Zone/Boulder),MonodontaplottramontoS,family="poisson",REML=FALSE)
Modello_MonodontaplottramontoS_5<-glmmTMB(TOT~Temp+MOON+LITUNLIT+(1|Zone/Boulder),MonodontaplottramontoS,family="poisson",REML=FALSE)
Modello_MonodontaplottramontoS_6<-glmmTMB(TOT~Temp+LITUNLIT+(1|Zone/Boulder),MonodontaplottramontoS,family="poisson",REML=FALSE)
Modello_MonodontaplottramontoS_7<-glmmTMB(TOT~LITUNLIT+(1|Zone/Boulder),MonodontaplottramontoS,family="poisson",REML=FALSE)
Modello_MonodontaplottramontoS_8<-glmmTMB(TOT~MOON+(1|Zone/Boulder),MonodontaplottramontoS,family="poisson",REML=FALSE)
Modello_MonodontaplottramontoS_9<-glmmTMB(TOT~MOON+LITUNLIT+(1|Zone/Boulder),MonodontaplottramontoS,family="poisson",REML=FALSE)
AIC(Modello_MonodontaplottramontoS_1,Modello_MonodontaplottramontoS_2,Modello_MonodontaplottramontoS_3,Modello_MonodontaplottramontoS_4,Modello_MonodontaplottramontoS_5,Modello_MonodontaplottramontoS_6,Modello_MonodontaplottramontoS_7,Modello_MonodontaplottramontoS_8,Modello_MonodontaplottramontoS_9,Modello_MonodontaplottramontoS_null)



#Sunset, L side
Modello_MonodontaplottramontoL_1<-glmmTMB(TOT~Temp+MOON+LITUNLIT+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplottramontoL,family="poisson",REML=FALSE)
Modello_MonodontaplottramontoL_2<-glmmTMB(TOT~MOON+LITUNLIT+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplottramontoL,family="poisson",REML=FALSE)#parte
Modello_MonodontaplottramontoL_3<-glmmTMB(TOT~Temp+(1|Zone/Boulder),MonodontaplottramontoL,family="poisson",REML=FALSE)
Modello_MonodontaplottramontoL_4<-glmmTMB(TOT~Temp+MOON+(1|Zone/Boulder),MonodontaplottramontoL,family="poisson",REML=FALSE)
Modello_MonodontaplottramontoL_null<-glmmTMB(TOT~(1|Zone/Boulder),MonodontaplottramontoL,family="poisson",REML=FALSE)
Modello_MonodontaplottramontoL_5<-glmmTMB(TOT~Temp+MOON+LITUNLIT+(1|Zone/Boulder),MonodontaplottramontoL,family="poisson",REML=FALSE)
Modello_MonodontaplottramontoL_6<-glmmTMB(TOT~Temp+LITUNLIT+(1|Zone/Boulder),MonodontaplottramontoL,family="poisson",REML=FALSE)
Modello_MonodontaplottramontoL_7<-glmmTMB(TOT~LITUNLIT+(1|Zone/Boulder),MonodontaplottramontoL,family="poisson",REML=FALSE)
Modello_MonodontaplottramontoL_8<-glmmTMB(TOT~MOON+(1|Zone/Boulder),MonodontaplottramontoL,family="poisson",REML=FALSE)
Modello_MonodontaplottramontoL_9<-glmmTMB(TOT~MOON+LITUNLIT+(1|Zone/Boulder),MonodontaplottramontoL,family="poisson",REML=FALSE)
AIC(Modello_MonodontaplottramontoL_1,Modello_MonodontaplottramontoL_2,Modello_MonodontaplottramontoL_3,Modello_MonodontaplottramontoL_4,Modello_MonodontaplottramontoL_5,Modello_MonodontaplottramontoL_6,Modello_MonodontaplottramontoL_7,Modello_MonodontaplottramontoL_8,Modello_MonodontaplottramontoL_9,Modello_MonodontaplottramontoL_null)


#Dusk, S side
Modello_MonodontaplotNotte1S_1<-glmmTMB(TOT~Temp+MOON+LITUNLIT+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte1S,family="poisson",REML=FALSE)
Modello_MonodontaplotNotte1S_2<-glmmTMB(TOT~MOON+LITUNLIT+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte1S,family="poisson",REML=FALSE)#parte
Modello_MonodontaplotNotte1S_3<-glmmTMB(TOT~Temp+(1|Zone/Boulder),MonodontaplotNotte1S,family="poisson",REML=FALSE)
Modello_MonodontaplotNotte1S_4<-glmmTMB(TOT~Temp+MOON+(1|Zone/Boulder),MonodontaplotNotte1S,family="poisson",REML=FALSE)
Modello_MonodontaplotNotte1S_null<-glmmTMB(TOT~(1|Zone/Boulder),MonodontaplotNotte1S,family="poisson",REML=FALSE)
Modello_MonodontaplotNotte1S_5<-glmmTMB(TOT~Temp+MOON+LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte1S,family="poisson",REML=FALSE)
Modello_MonodontaplotNotte1S_6<-glmmTMB(TOT~Temp+LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte1S,family="poisson",REML=FALSE)
Modello_MonodontaplotNotte1S_7<-glmmTMB(TOT~LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte1S,family="poisson",REML=FALSE)
Modello_MonodontaplotNotte1S_8<-glmmTMB(TOT~MOON+(1|Zone/Boulder),MonodontaplotNotte1S,family="poisson",REML=FALSE)
Modello_MonodontaplotNotte1S_9<-glmmTMB(TOT~MOON+LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte1S,family="poisson",REML=FALSE)
AIC(Modello_MonodontaplotNotte1S_1,Modello_MonodontaplotNotte1S_2,Modello_MonodontaplotNotte1S_3,Modello_MonodontaplotNotte1S_4,Modello_MonodontaplotNotte1S_5,Modello_MonodontaplotNotte1S_6,Modello_MonodontaplotNotte1S_7,Modello_MonodontaplotNotte1S_8,Modello_MonodontaplotNotte1S_9,Modello_MonodontaplotNotte1S_null)
anova(Modello_MonodontaplotNotte1S_1,Modello_MonodontaplotNotte1S_null,Modello_MonodontaplotNotte1S_6,Modello_MonodontaplotNotte1S_5,test="Chisq")



#Dusk, L side
Modello_MonodontaplotNotte1L_1<-glmmTMB(TOT~Temp+MOON+LITUNLIT+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte1L,family="poisson",REML=FALSE)
Modello_MonodontaplotNotte1L_2<-glmmTMB(TOT~MOON+LITUNLIT+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte1L,family="poisson",REML=FALSE)#parte
Modello_MonodontaplotNotte1L_3<-glmmTMB(TOT~Temp+(1|Zone/Boulder),MonodontaplotNotte1L,family="poisson",REML=FALSE)
Modello_MonodontaplotNotte1L_4<-glmmTMB(TOT~Temp+MOON+(1|Zone/Boulder),MonodontaplotNotte1L,family="poisson",REML=FALSE)
Modello_MonodontaplotNotte1L_null<-glmmTMB(TOT~(1|Zone/Boulder),MonodontaplotNotte1L,family="poisson",REML=FALSE)
Modello_MonodontaplotNotte1L_5<-glmmTMB(TOT~Temp+MOON+LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte1L,family="poisson",REML=FALSE)
Modello_MonodontaplotNotte1L_6<-glmmTMB(TOT~Temp+LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte1L,family="poisson",REML=FALSE)
Modello_MonodontaplotNotte1L_7<-glmmTMB(TOT~LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte1L,family="poisson",REML=FALSE)
Modello_MonodontaplotNotte1L_8<-glmmTMB(TOT~MOON+(1|Zone/Boulder),MonodontaplotNotte1L,family="poisson",REML=FALSE)
Modello_MonodontaplotNotte1L_9<-glmmTMB(TOT~MOON+LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte1L,family="poisson",REML=FALSE)
AIC(Modello_MonodontaplotNotte1L_1,Modello_MonodontaplotNotte1L_2,Modello_MonodontaplotNotte1L_3,Modello_MonodontaplotNotte1L_4,Modello_MonodontaplotNotte1L_5,Modello_MonodontaplotNotte1L_6,Modello_MonodontaplotNotte1L_7,Modello_MonodontaplotNotte1L_8,Modello_MonodontaplotNotte1L_9,Modello_MonodontaplotNotte1L_null)
anova(Modello_MonodontaplotNotte1L_1,Modello_MonodontaplotNotte1L_null,Modello_MonodontaplotNotte1L_6,Modello_MonodontaplotNotte1L_5,test="Chisq")

car::Anova(Modello_MonodontaplotNotte1L_noDaynointtemp)

#Night, S side
Modello_MonodontaplotNotte2S_1<-glmmTMB(TOT~Temp+MOON+LITUNLIT+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte2S,family="poisson",REML=FALSE)
Modello_MonodontaplotNotte2S_2<-glmmTMB(TOT~MOON+LITUNLIT+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte2S,family="poisson",REML=FALSE)#parte
Modello_MonodontaplotNotte2S_3<-glmmTMB(TOT~Temp+(1|Zone/Boulder),MonodontaplotNotte2S,family="poisson",REML=FALSE)
Modello_MonodontaplotNotte2S_4<-glmmTMB(TOT~Temp+MOON+(1|Zone/Boulder),MonodontaplotNotte2S,family="poisson",REML=FALSE)
Modello_MonodontaplotNotte2S_null<-glmmTMB(TOT~(1|Zone/Boulder),MonodontaplotNotte2S,family="poisson",REML=FALSE)
Modello_MonodontaplotNotte2S_5<-glmmTMB(TOT~Temp+MOON+LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte2S,family="poisson",REML=FALSE)
Modello_MonodontaplotNotte2S_6<-glmmTMB(TOT~Temp+LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte2S,family="poisson",REML=FALSE)
Modello_MonodontaplotNotte2S_7<-glmmTMB(TOT~LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte2S,family="poisson",REML=FALSE)
Modello_MonodontaplotNotte2S_8<-glmmTMB(TOT~MOON+(1|Zone/Boulder),MonodontaplotNotte2S,family="poisson",REML=FALSE)
Modello_MonodontaplotNotte2S_9<-glmmTMB(TOT~MOON+LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte2S,family="poisson",REML=FALSE)
AIC(Modello_MonodontaplotNotte2S_1,Modello_MonodontaplotNotte2S_2,Modello_MonodontaplotNotte2S_3,Modello_MonodontaplotNotte2S_4,Modello_MonodontaplotNotte2S_5,Modello_MonodontaplotNotte2S_6,Modello_MonodontaplotNotte2S_7,Modello_MonodontaplotNotte2S_8,Modello_MonodontaplotNotte2S_9,Modello_MonodontaplotNotte2S_null)
anova(Modello_MonodontaplotNotte2S_1,Modello_MonodontaplotNotte2S_null,Modello_MonodontaplotNotte2S_6,Modello_MonodontaplotNotte2S_5,test="Chisq")


car::Anova(Modello_MonodontaplotNotte2S_solotemp)
car::Anova(Modello_MonodontaplotNotte2S_noDaynointtemp)

#Night, L side
Modello_MonodontaplotNotte2L_1<-glmmTMB(TOT~Temp+MOON+LITUNLIT+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte2L,family="poisson",REML=FALSE)
Modello_MonodontaplotNotte2L_2<-glmmTMB(TOT~MOON+LITUNLIT+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte2L,family="poisson",REML=FALSE)#parte
Modello_MonodontaplotNotte2L_3<-glmmTMB(TOT~Temp+(1|Zone/Boulder),MonodontaplotNotte2L,family="poisson",REML=FALSE)
Modello_MonodontaplotNotte2L_4<-glmmTMB(TOT~Temp+MOON+(1|Zone/Boulder),MonodontaplotNotte2L,family="poisson",REML=FALSE)
Modello_MonodontaplotNotte2L_null<-glmmTMB(TOT~(1|Zone/Boulder),MonodontaplotNotte2L,family="poisson",REML=FALSE)
Modello_MonodontaplotNotte2L_5<-glmmTMB(TOT~Temp+MOON+LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte2L,family="poisson",REML=FALSE)
Modello_MonodontaplotNotte2L_6<-glmmTMB(TOT~Temp+LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte2L,family="poisson",REML=FALSE)
Modello_MonodontaplotNotte2L_7<-glmmTMB(TOT~LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte2L,family="poisson",REML=FALSE)
Modello_MonodontaplotNotte2L_8<-glmmTMB(TOT~MOON+(1|Zone/Boulder),MonodontaplotNotte2L,family="poisson",REML=FALSE)
Modello_MonodontaplotNotte2L_9<-glmmTMB(TOT~MOON+LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte2L,family="poisson",REML=FALSE)
AIC(Modello_MonodontaplotNotte2L_1,Modello_MonodontaplotNotte2L_2,Modello_MonodontaplotNotte2L_3,Modello_MonodontaplotNotte2L_4,Modello_MonodontaplotNotte2L_5,Modello_MonodontaplotNotte2L_6,Modello_MonodontaplotNotte2L_7,Modello_MonodontaplotNotte2L_8,Modello_MonodontaplotNotte2L_9,Modello_MonodontaplotNotte2L_null)
anova(Modello_MonodontaplotNotte2L_1,Modello_MonodontaplotNotte2L_null,Modello_MonodontaplotNotte2L_6,Modello_MonodontaplotNotte2L_5,test="Chisq")

####passage for choose the best model, analysis changing the random part whit AIC and anova,only for model whit significative results####
#Morning, L side
Modello_MonodontaplotmattinaL_noDaynointtemp1<-glmmTMB(OW~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotmattinaL,family="poisson",REML=TRUE)
Modello_MonodontaplotmattinaL_noDaynointtemp2<-glmmTMB(OW~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder)+(1|Day),MonodontaplotmattinaL,family="poisson",REML=TRUE)
Modello_MonodontaplotmattinaL_noDaynointtemp3<-glmmTMB(OW~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone)+(1|Day),MonodontaplotmattinaL,family="poisson",REML=TRUE)
Modello_MonodontaplotmattinaL_noDaynointtemp4<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Boulder)+(1|Day),MonodontaplotmattinaL,family="poisson",REML=TRUE)
Modello_MonodontaplotmattinaL_noDaynointtemp5<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Boulder),MonodontaplotmattinaL,family="poisson",REML=TRUE)
Modello_MonodontaplotmattinaL_noDaynointtemp6<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone),MonodontaplotmattinaL,family="poisson",REML=TRUE)
Modello_MonodontaplotmattinaL_noDaynointtemp7<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Day),MonodontaplotmattinaL,family="poisson",REML=TRUE)
Modello_MonodontaplotmattinaL_noDaynointtemp8<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT,MonodontaplotmattinaL,family="poisson",REML=TRUE)
AIC(Modello_MonodontaplotmattinaL_noDaynointtemp1,Modello_MonodontaplotmattinaL_noDaynointtemp2,Modello_MonodontaplotmattinaL_noDaynointtemp3,Modello_MonodontaplotmattinaL_noDaynointtemp4,Modello_MonodontaplotmattinaL_noDaynointtemp5,Modello_MonodontaplotmattinaL_noDaynointtemp6,Modello_MonodontaplotmattinaL_noDaynointtemp7,Modello_MonodontaplotmattinaL_noDaynointtemp8)


#Morning, S side
Modello_MonodontaplotmattinaS_noDaynointtemp1<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotmattinaS,family="poisson",REML=TRUE)
Modello_MonodontaplotmattinaS_noDaynointtemp2<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder)+(1|Day),MonodontaplotmattinaS,family="poisson",REML=TRUE)
Modello_MonodontaplotmattinaS_noDaynointtemp3<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone)+(1|Day),MonodontaplotmattinaS,family="poisson",REML=TRUE)
Modello_MonodontaplotmattinaS_noDaynointtemp4<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Boulder)+(1|Day),MonodontaplotmattinaS,family="poisson",REML=TRUE)
Modello_MonodontaplotmattinaS_noDaynointtemp5<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Boulder),MonodontaplotmattinaS,family="poisson",REML=TRUE)
Modello_MonodontaplotmattinaS_noDaynointtemp6<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone),MonodontaplotmattinaS,family="poisson",REML=TRUE)
Modello_MonodontaplotmattinaS_noDaynointtemp7<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Day),MonodontaplotmattinaS,family="poisson",REML=TRUE)
Modello_MonodontaplotmattinaS_noDaynointtemp8<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT,MonodontaplotmattinaS,family="poisson",REML=TRUE)
AIC(Modello_MonodontaplotmattinaS_noDaynointtemp1,Modello_MonodontaplotmattinaS_noDaynointtemp2,Modello_MonodontaplotmattinaS_noDaynointtemp3,Modello_MonodontaplotmattinaS_noDaynointtemp4,Modello_MonodontaplotmattinaS_noDaynointtemp5,Modello_MonodontaplotmattinaS_noDaynointtemp6,Modello_MonodontaplotmattinaS_noDaynointtemp7,Modello_MonodontaplotmattinaS_noDaynointtemp8)


#Sunset,L side
Modello_MonodontaplottramontoL_noDaynointtemp1<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplottramontoL,family="poisson",REML=TRUE)
Modello_MonodontaplottramontoL_noDaynointtemp2<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder)+(1|Day),MonodontaplottramontoL,family="poisson",REML=TRUE)
Modello_MonodontaplottramontoL_noDaynointtemp3<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone)+(1|Day),MonodontaplottramontoL,family="poisson",REML=TRUE)
Modello_MonodontaplottramontoL_noDaynointtemp4<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Boulder)+(1|Day),MonodontaplottramontoL,family="poisson",REML=TRUE)
Modello_MonodontaplottramontoL_noDaynointtemp5<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Boulder),MonodontaplottramontoL,family="poisson",REML=TRUE)
Modello_MonodontaplottramontoL_noDaynointtemp6<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone),MonodontaplottramontoL,family="poisson",REML=TRUE)
Modello_MonodontaplottramontoL_noDaynointtemp7<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Day),MonodontaplottramontoL,family="poisson",REML=TRUE)
Modello_MonodontaplottramontoL_noDaynointtemp8<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT,MonodontaplottramontoL,family="poisson",REML=TRUE)
AIC(Modello_MonodontaplottramontoL_noDaynointtemp1,Modello_MonodontaplottramontoL_noDaynointtemp2,Modello_MonodontaplottramontoL_noDaynointtemp3,Modello_MonodontaplottramontoL_noDaynointtemp4,Modello_MonodontaplottramontoL_noDaynointtemp5,Modello_MonodontaplottramontoL_noDaynointtemp6,Modello_MonodontaplottramontoL_noDaynointtemp7,Modello_MonodontaplottramontoL_noDaynointtemp8)
anova(Modello_MonodontaplottramontoL_noDaynointtemp1,Modello_MonodontaplottramontoL_noDaynointtemp4,Modello_MonodontaplottramontoL_noDaynointtemp5)
anova(Modello_MonodontaplottramontoL_noDaynointtemp1,Modello_MonodontaplottramontoL_noDaynointtemp2,Modello_MonodontaplottramontoL_noDaynointtemp8)

Modello_MonodontaplottramontoS_noDaynointtemp1<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplottramontoS,family="poisson",REML=TRUE)
Modello_MonodontaplottramontoS_noDaynointtemp2<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder)+(1|Day),MonodontaplottramontoS,family="poisson",REML=TRUE)
Modello_MonodontaplottramontoS_noDaynointtemp3<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone)+(1|Day),MonodontaplottramontoS,family="poisson",REML=TRUE)
Modello_MonodontaplottramontoS_noDaynointtemp4<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Boulder)+(1|Day),MonodontaplottramontoS,family="poisson",REML=TRUE)
Modello_MonodontaplottramontoS_noDaynointtemp5<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Boulder),MonodontaplottramontoS,family="poisson",REML=TRUE)
Modello_MonodontaplottramontoS_noDaynointtemp6<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone),MonodontaplottramontoS,family="poisson",REML=TRUE)
Modello_MonodontaplottramontoS_noDaynointtemp7<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Day),MonodontaplottramontoS,family="poisson",REML=TRUE)
Modello_MonodontaplottramontoS_noDaynointtemp8<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT,MonodontaplottramontoS,family="poisson",REML=TRUE)
AIC(Modello_MonodontaplottramontoS_noDaynointtemp1,Modello_MonodontaplottramontoS_noDaynointtemp2,Modello_MonodontaplottramontoS_noDaynointtemp3,Modello_MonodontaplottramontoS_noDaynointtemp4,Modello_MonodontaplottramontoS_noDaynointtemp5,Modello_MonodontaplottramontoS_noDaynointtemp6,Modello_MonodontaplottramontoS_noDaynointtemp7,Modello_MonodontaplottramontoS_noDaynointtemp8)
anova(Modello_MonodontaplottramontoS_noDaynointtemp1,Modello_MonodontaplottramontoS_noDaynointtemp4,Modello_MonodontaplottramontoS_noDaynointtemp5)
anova(Modello_MonodontaplottramontoS_noDaynointtemp1,Modello_MonodontaplottramontoS_noDaynointtemp2,Modello_MonodontaplottramontoS_noDaynointtemp8)



#Dusk, L side
Modello_MonodontaplotNotte1L_noDaynointtemp1<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte1L,family="poisson",REML=TRUE)
Modello_MonodontaplotNotte1L_noDaynointtemp2<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder)+(1|Day),MonodontaplotNotte1L,family="poisson",REML=TRUE)
Modello_MonodontaplotNotte1L_noDaynointtemp3<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone)+(1|Day),MonodontaplotNotte1L,family="poisson",REML=TRUE)
Modello_MonodontaplotNotte1L_noDaynointtemp4<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Boulder)+(1|Day),MonodontaplotNotte1L,family="poisson",REML=TRUE)
Modello_MonodontaplotNotte1L_noDaynointtemp5<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Boulder),MonodontaplotNotte1L,family="poisson",REML=TRUE)
Modello_MonodontaplotNotte1L_noDaynointtemp6<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone),MonodontaplotNotte1L,family="poisson",REML=TRUE)
Modello_MonodontaplotNotte1L_noDaynointtemp7<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Day),MonodontaplotNotte1L,family="poisson",REML=TRUE)
Modello_MonodontaplotNotte1L_noDaynointtemp8<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT,MonodontaplotNotte1L,family="poisson",REML=TRUE)
AIC(Modello_MonodontaplotNotte1L_noDaynointtemp1,Modello_MonodontaplotNotte1L_noDaynointtemp2,Modello_MonodontaplotNotte1L_noDaynointtemp3,Modello_MonodontaplotNotte1L_noDaynointtemp4,Modello_MonodontaplotNotte1L_noDaynointtemp5,Modello_MonodontaplotNotte1L_noDaynointtemp6,Modello_MonodontaplotNotte1L_noDaynointtemp7,Modello_MonodontaplotNotte1L_noDaynointtemp8)


car::Anova(Modello_MonodontaplotmattinaL_noDaynointtemp)

Modello_MonodontaplotNotte1S_noDaynointtemp1<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte1S,family="poisson",REML=TRUE)
Modello_MonodontaplotNotte1S_noDaynointtemp2<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder)+(1|Day),MonodontaplotNotte1S,family="poisson",REML=TRUE)
Modello_MonodontaplotNotte1S_noDaynointtemp3<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone)+(1|Day),MonodontaplotNotte1S,family="poisson",REML=TRUE)
Modello_MonodontaplotNotte1S_noDaynointtemp4<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Boulder)+(1|Day),MonodontaplotNotte1S,family="poisson",REML=TRUE)
Modello_MonodontaplotNotte1S_noDaynointtemp5<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Boulder),MonodontaplotNotte1S,family="poisson",REML=TRUE)
Modello_MonodontaplotNotte1S_noDaynointtemp6<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone),MonodontaplotNotte1S,family="poisson",REML=TRUE)
Modello_MonodontaplotNotte1S_noDaynointtemp7<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Day),MonodontaplotNotte1S,family="poisson",REML=TRUE)
Modello_MonodontaplotNotte1S_noDaynointtemp8<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT,MonodontaplotNotte1S,family="poisson",REML=TRUE)
AIC(Modello_MonodontaplotNotte1S_noDaynointtemp1,Modello_MonodontaplotNotte1S_noDaynointtemp2,Modello_MonodontaplotNotte1S_noDaynointtemp3,Modello_MonodontaplotNotte1S_noDaynointtemp4,Modello_MonodontaplotNotte1S_noDaynointtemp5,Modello_MonodontaplotNotte1S_noDaynointtemp6,Modello_MonodontaplotNotte1S_noDaynointtemp7,Modello_MonodontaplotNotte1S_noDaynointtemp8)


#Night, L side
Modello_MonodontaplotNotte2L_noDaynointtemp1<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte2L,family="poisson",REML=TRUE)
Modello_MonodontaplotNotte2L_noDaynointtemp2<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder)+(1|Day),MonodontaplotNotte2L,family="poisson",REML=TRUE)
Modello_MonodontaplotNotte2L_noDaynointtemp3<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone)+(1|Day),MonodontaplotNotte2L,family="poisson",REML=TRUE)
Modello_MonodontaplotNotte2L_noDaynointtemp4<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Boulder)+(1|Day),MonodontaplotNotte2L,family="poisson",REML=TRUE)
Modello_MonodontaplotNotte2L_noDaynointtemp5<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Boulder),MonodontaplotNotte2L,family="poisson",REML=TRUE)
Modello_MonodontaplotNotte2L_noDaynointtemp6<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone),MonodontaplotNotte2L,family="poisson",REML=TRUE)
Modello_MonodontaplotNotte2L_noDaynointtemp7<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Day),MonodontaplotNotte2L,family="poisson",REML=TRUE)
Modello_MonodontaplotNotte2L_noDaynointtemp8<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT,MonodontaplotNotte2L,family="poisson",REML=TRUE)
AIC(Modello_MonodontaplotNotte2L_noDaynointtemp1,Modello_MonodontaplotNotte2L_noDaynointtemp2,Modello_MonodontaplotNotte2L_noDaynointtemp3,Modello_MonodontaplotNotte2L_noDaynointtemp4,Modello_MonodontaplotNotte2L_noDaynointtemp5,Modello_MonodontaplotNotte2L_noDaynointtemp6,Modello_MonodontaplotNotte2L_noDaynointtemp7,Modello_MonodontaplotNotte2L_noDaynointtemp8)

#Night, S side
Modello_MonodontaplotNotte2S_noDaynointtemp1<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder),MonodontaplotNotte2S,family="poisson",REML=TRUE)
Modello_MonodontaplotNotte2S_noDaynointtemp2<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone/Boulder)+(1|Day),MonodontaplotNotte2S,family="poisson",REML=TRUE)
Modello_MonodontaplotNotte2S_noDaynointtemp3<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone)+(1|Day),MonodontaplotNotte2S,family="poisson",REML=TRUE)
Modello_MonodontaplotNotte2S_noDaynointtemp4<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Boulder)+(1|Day),MonodontaplotNotte2S,family="poisson",REML=TRUE)
Modello_MonodontaplotNotte2S_noDaynointtemp5<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Boulder),MonodontaplotNotte2S,family="poisson",REML=TRUE)
Modello_MonodontaplotNotte2S_noDaynointtemp6<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Zone),MonodontaplotNotte2S,family="poisson",REML=TRUE)
Modello_MonodontaplotNotte2S_noDaynointtemp7<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT+(1|Day),MonodontaplotNotte2S,family="poisson",REML=TRUE)
Modello_MonodontaplotNotte2S_noDaynointtemp8<-glmmTMB(TOT~Temp+LITUNLIT+MOON+MOON:LITUNLIT,MonodontaplotNotte2S,family="poisson",REML=TRUE)
AIC(Modello_MonodontaplotNotte2S_noDaynointtemp1,Modello_MonodontaplotNotte2S_noDaynointtemp2,Modello_MonodontaplotNotte2S_noDaynointtemp3,Modello_MonodontaplotNotte2S_noDaynointtemp4,Modello_MonodontaplotNotte2S_noDaynointtemp5,Modello_MonodontaplotNotte2S_noDaynointtemp6,Modello_MonodontaplotNotte2S_noDaynointtemp7,Modello_MonodontaplotNotte2S_noDaynointtemp8)
anova(Modello_MonodontaplotNotte2S_noDaynointtemp1,Modello_MonodontaplotNotte2S_noDaynointtemp4,Modello_MonodontaplotNotte2S_noDaynointtemp5)
anova(Modello_MonodontaplotNotte2S_noDaynointtemp1,Modello_MonodontaplotNotte2S_noDaynointtemp2,Modello_MonodontaplotNotte2S_noDaynointtemp8)


####underline the position of every individual in the plots####
#read excel file
Scogliopos<-read_excel("Posizionesuscoglio2.xlsx")
#subset data by side of the cliff in two new database
ScoglioposL<-subset(Scogliopos,Scogliopos$Side=="L")
ScoglioposS<-subset(Scogliopos,Scogliopos$Side=="S")
#work whit L side data, subset data by moon condition at night
ScoglioposLNEW<-subset(ScoglioposL,ScoglioposL$MOON=="NEW")
ScoglioposLFULL<-subset(ScoglioposL,ScoglioposL$MOON=="FULL")
ScoglioposLgNEW<-subset(ScoglioposLNEW,ScoglioposLNEW$Time=="Tramonto"|ScoglioposLNEW$Time=="Notte1"|ScoglioposLNEW$Time=="Notte2"|ScoglioposLNEW$Time=="Mattina")
ScoglioposLgNEW
ScoglioposLgFULL<-subset(ScoglioposLFULL,ScoglioposLFULL$Time=="Tramonto"|ScoglioposLFULL$Time=="Notte1"|ScoglioposLFULL$Time=="Notte2"|ScoglioposLFULL$Time=="Mattina")
ScoglioposSNEW<-subset(ScoglioposS,ScoglioposS$MOON=="NEW")
ScoglioposSFULL<-subset(ScoglioposS,ScoglioposS$MOON=="FULL")
ScoglioposSgNEW<-subset(ScoglioposSNEW,ScoglioposSNEW$Time=="Tramonto"|ScoglioposSNEW$Time=="Notte1"|ScoglioposSNEW$Time=="Notte2"|ScoglioposSNEW$Time=="Mattina")
ScoglioposSgFULL<-subset(ScoglioposSFULL,ScoglioposSFULL$Time=="Tramonto"|ScoglioposSFULL$Time=="Notte1"|ScoglioposSFULL$Time=="Notte2"|ScoglioposSFULL$Time=="Mattina")
#create plot, every point is an individual
library(ggbeeswarm)
ggplot(
  ScoglioposSgNEW,
  aes(
    x = LITUNLIT,
    y = Level,
    color = Time,
    shape = Position,
    group = Time     
  )
) +
  geom_quasirandom(
    width = 0.15,       
    dodge.width = 0.7,  
    groupOnX = TRUE,
    varwidth = FALSE,
    size = 3
  ) +
  coord_cartesian(ylim = c(-5, 5)) +
  scale_y_continuous(breaks = seq(-5, 5, 1)) +
  scale_color_manual(
    name = "OBST",
    breaks = c("Mattina", "Tramonto", "Notte1", "Notte2"),
    labels = c("Morning", "Sunset", "Dusk", "Night"),
    values = c("gold", "darkorange2", "blue", "black")
  ) +
  scale_shape_manual(
    name = "Position",  breaks = c("OW", "UW"),
    labels = c("AW", "BW"),
    values = c("OW" = 18, "UW" = 4)
  ) +
  labs(
    title = "Seaward side",
    tag = "↑ 4 cm",
    subtitle = "New moon",
    x = "ALAN",
    y = "Vertical position of individuals"
  ) +
  theme_sjplot2(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

ggplot(
  ScoglioposLgFULL,
  aes(
    x = LITUNLIT,
    y = Level,
    color = Time,
    shape = Position,
    group = Time     # <--- aggiungi questo
  )
) +
  geom_quasirandom(
    width = 0.20,       # controlla la larghezza orizzontale
    dodge.width = 1,  # separa visivamente i gruppi colore
    groupOnX = TRUE,
    varwidth = FALSE,
    size = 3
  ) +
  coord_cartesian(ylim = c(-5, 5)) +
  scale_y_continuous(breaks = seq(-5, 5, 1)) +
  scale_color_manual(
    name = "OBST",
    breaks = c("Mattina", "Tramonto", "Notte1", "Notte2"),
    labels = c("Morning", "Sunset", "Dusk", "Night"),
    values = c("gold", "darkorange2", "blue", "black")
  ) +
  scale_shape_manual(
    name = "Position",  breaks = c("OW", "UW"),
    labels = c("AW", "BW"),
    values = c("OW" = 18, "UW" = 4)
  ) +
  labs(
    title = "Landward side",
    tag = "↑ 4 cm",
    subtitle = "Full moon",
    x = "ALAN",
    y = "Vertical position of individuals"
  ) +
  theme_sjplot2(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
ggplot(
  ScoglioposSgFULL,
  aes(
    x = LITUNLIT,
    y = Level,
    color = Time,
    shape = Position,
    group = Time     # <--- aggiungi questo
  )
) +
  geom_quasirandom(
    width = 0.15,       # controlla la larghezza orizzontale
    dodge.width = 0.8,  # separa visivamente i gruppi colore
    groupOnX = TRUE,
    varwidth = FALSE,
    size = 3
  ) +
  coord_cartesian(ylim = c(-5, 5)) +
  scale_y_continuous(breaks = seq(-5, 5, 1)) +
  scale_color_manual(
    name = "OBST",
    breaks = c("Mattina", "Tramonto", "Notte1", "Notte2"),
    labels = c("Morning", "Sunset", "Dusk", "Night"),
    values = c("gold", "darkorange2", "blue", "black")
  ) +
  scale_shape_manual(
    name = "Position",  breaks = c("OW", "UW"),
    labels = c("AW", "BW"),
    values = c("OW" = 18, "UW" = 4)
  ) +
  labs(
    title = "Seaward side",
    tag = "↑ 4 cm",
    subtitle = "Full moon",
    x = "ALAN",
    y = "Vertical position of individuals"
  ) +
  theme_sjplot2(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
ggplot(
  ScoglioposLgNEW,
  aes(
    x = LITUNLIT,
    y = Level,
    color = Time,
    shape = Position,
    group = Time     # <--- aggiungi questo
  )
) +
  geom_quasirandom(
    width = 0.15,       # controlla la larghezza orizzontale
    dodge.width = 0.7,  # separa visivamente i gruppi colore
    groupOnX = TRUE,
    varwidth = FALSE,
    size = 3
  ) +
  coord_cartesian(ylim = c(-5, 5)) +
  scale_y_continuous(breaks = seq(-5, 5, 1)) +
  scale_color_manual(
    name = "OBST",
    breaks = c("Mattina", "Tramonto", "Notte1", "Notte2"),
    labels = c("Morning", "Sunset", "Dusk", "Night"),
    values = c("gold", "darkorange2", "blue", "black")
  ) +
  scale_shape_manual(
    name = "Position",  breaks = c("OW", "UW"),
    labels = c("AW", "BW"),
    values = c("OW" = 18, "UW" = 4)
  ) +
  labs(
    title = "Landward side",
    tag = "↑ 4 cm",
    subtitle = "New moon",
    x = "ALAN",
    y = "Vertical position of individuals"
  ) +
  theme_sjplot2(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )



####comparison whit precedent study in laboratoty condition####
#read excel
Chelazzi<- read_excel("Verticalmovement.xlsx")
#subset to prepare the database 
ChelazzilatoL<-subset(Chelazzi,Chelazzi$Side=="L")
#plot
ggplot(data = ChelazzilatoL, aes(x = Light, y = Value)) +
  geom_violin(outlier.shape = NA, width = .3, aes(color = Light)) +
  geom_dotplot(aes(fill = Moon), binaxis = 'y', stackdir = 'center', dotsize = 0.8, position = position_dodge(width = 0.15)) +
  labs(
    x = "ALAN", y = "Movement (cm)"
  ) +
  theme(legend.position = "right") +
  scale_color_manual(values = c("black", "darkgoldenrod3")) +
  scale_fill_manual(values = c("grey70", "red")) +  # scegli i colori per 'Moon'
  annotate("text", x = c(1,2), y = c(-15,-15), 
           label = c("n=10","n=15"))

#anova
Chelazzi.aovl<-aov(Value~Light,data=ChelazzilatoL)
summary(Chelazzi.aovl)
#Brown-Forsythe Test
bf.test(Value~Light,data=ChelazzilatoL)






