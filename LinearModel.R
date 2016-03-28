setwd("/Users/sandeep/Downloads/")

dat<-na.omit(read.table("dioxin.csv",sep=',',header=T))
#dat<-read.table("dioxin.csv",sep=',',header=T)
dat$DIOX<-log(dat$DIOX)


#dat.df<-na.omit(dat)
for(i in 3:8){
  dat[,i] <- as.factor(dat[,i])
}

#Block, active variable data subset.
dat.ba<-dat[,2:8]
names(dat.ba)

pairs(dat.ba,panel=panel.smooth)
pairs.panels(dat.ba)#draws scatterplots, histograms, and shows correlations 


#Block ,active & Measured variable data Subset.

dat.blk<-dat[,2:12]
names(dat.blk)


#library(tree)
#model<-tree(DIOX~.,data=dat)
#plot(model)
#text(model)
#plot(LOAD,DIOX)

boxplot(dat$O2COR~dat$LOAD,data = dat)

#Model
attach(dat)
model1 = lm(DIOX~PLANT+TIME+LAB+OXYGEN+LOAD+PRSEK)
summary(model1)

model2 = lm(DIOX~PLANT+TIME+LAB+LOAD+OXYGEN)
summary(model2)

anova(model1,model2)

#model12<-update(model1,~.-PRSEK)


#Excersice 3

modela<-lm(DIOX~TIME+LAB+PLANT+O2COR+NEFFEKT+QRAT)
summary(modela)


modelb<-update(modela,~. -QRAT)
summary(modelb)

#simple additve model 
m1<-lm(DIOX~1)
add1(m1,scope = c("PLANT","TIME","LAB","O2COR","NEFFEKT","QRAT"),test="F")

m1<-update(m1,~.+PLANT)
summary(m1)

add1(m1,scope = c("TIME","LAB","O2COR","NEFFEKT","QRAT"),test="F")


m1<-update(m1,~.+NEFFEKT)
summary(m1)

add1(m1,scope = c("TIME","LAB","O2COR","QRAT"),test="F")

m1<-update(m1,~.+O2COR)
summary(m1)

add1(m1,scope = c("TIME","LAB","QRAT"),test="F")

m1<-update(m1,~.+TIME)
summary(m1)

add1(m1,scope = c("LAB","QRAT"),test="F")

m1<-update(m1,~.+LAB)
summary(m1)

add1(m1,scope = c("QRAT"),test="F")

#final model
#fm<-lm(DIOX~TIME*LAB*PLANT*O2COR*NEFFEKT*QRAT*QROEG*TOVN*TROEG*POVN*CO2*CO*SO2*HCL*H2O)

fm0<-lm(DIOX~1)
summary(fm0)

add1(fm0,scope = c("TIME","LAB","PLANT","O2COR","NEFFEKT","QRAT","QROEG","TOVN","TROEG","POVN","CO2","CO","SO2","HCL","H2O"),test="F")

fm1<-update(fm0,~.+PLANT)
summary(fm1)

add1(fm1,scope = c("TIME","LAB","O2COR","NEFFEKT","QRAT","QROEG","TOVN","TROEG","POVN","CO2","CO","SO2","HCL","H2O"),test="F")

fm2<-update(fm1,~.+TROEG)
summary(fm2)

add1(fm2,scope = c("TIME","LAB","O2COR","NEFFEKT","QRAT","QROEG","TOVN","POVN","CO2","CO","SO2","HCL","H2O","PLANT:TROEG"),test="F")

fm3<-update(fm2,~.+LAB)
summary(fm3)

add1(fm3,scope = c("TIME","O2COR","NEFFEKT","QRAT","QROEG","TOVN","POVN","CO2","CO","SO2","HCL","H2O","PLANT:TROEG","PLANT:LAB","TROEG:LAB","PLANT:TROEG"),test="F")

fm4<-update(fm3,~.+H2O)
summary(fm4)

add1(fm4,scope = c("TIME","LAB","O2COR","NEFFEKT","QRAT","QROEG","TOVN","POVN","CO2","CO","SO2","HCL",
                  "PLANT:TROEG","PLAT:LAB","TROEG:LAB","PLANT:TROEG","PLANT:H2O","TROEG:H2O","LAB:H2O"),test="F")

fm5<-update(fm4,~.+NEFFEKT)
summary(fm5)

add1(fm5,scope = c("TIME","LAB","O2COR","QRAT","QROEG","TOVN","POVN","CO2","CO","SO2","HCL",
                  "PLANT:TROEG","PLAT:LAB","TROEG:LAB","PLANT:TROEG","PLANT:H2O","TROEG:H2O","LAB:H2O",
                   "PLANT:NEFFEKT","LAB:NEFFEKT","TROEG:NEFFEKT","H2O:NEFFEKT"),test="F")

fm6<-update(fm5,~.+TIME)
summary(fm6)

fm7<-update(fm6,~.-TROEG)
summary(fm7)

add1(fm7,scope = c("LAB","O2COR","QRAT","QROEG","TOVN","POVN","CO2","CO","SO2","HCL","TIME","LAB","O2COR","QRAT","QROEG","TOVN","POVN","CO2","CO","SO2","HCL",
                   "PLAT:LAB","PLANT:H2O","LAB:H2O",
                   "PLANT:NEFFEKT","LAB:NEFFEKT","H2O:NEFFEKT","PLANT:TIME","LAB:TIME","NEFFEKT:TIME"),test="F")

fm<-update(fm7,~.+HCL)
summary(fm7)

par(mfrow=c(1,2))
plot(fm)


add1(fm,scope = c("LAB","O2COR","QRAT","QROEG","TOVN","POVN","CO2","CO","SO2"),test="F")


par(mfrow=c(2,2))
plot(model14)

im<-lm(DIOX~1)
summary(im)
im<-add1(im,scope=~TIME*LAB*PLANT*O2COR*NEFFEKT*QRAT*QROEG*TOVN*TROEG*POVN*CO2*CO*SO2*HCL*H2O,test="F")
#model13 = update(model12,~. -PLANT)
#summary(model13)

#model14 = update(model12,~. -PLANT:TIME)
#summary(model14)
  


