##REGRESS?O MULTIPLA - METRICAS X AGRICULTURA + PASTAGEM  ## Dianne Silva ##

library(vegan)
library(ape)
library(ade4)
library(car)
library(AICcmodavg)
library(MXM)
#NODF

NODF<-read.table("Data/nodf.csv",header = TRUE,sep = ",")
a <- NODF[-6,]
agr.pas.nodf <- lm(NODF ~ agricultura + pastagem, data = a)
summary(agr.pas.nodf)
drop1(agr.pas.nodf,test="Chisq")

#H2

H2<-read.table("Data/H2.csv",header = TRUE,sep = ",")
aa <- H2[1:44,] 
agr.pas.H2 <- lm(H2 ~ agricultura + pastagem, data = aa)
summary(agr.pas.H2)
drop1(agr.pas.H2,test="Chisq")

#Q
Q<-read.table("Data/mod.csv",header = TRUE,sep = ",")
b <- Q[-3,]
agr.pas.Q <- lm(Q ~ agricultura + pastagem, data = b)
summary(agr.pas.Q)
drop1(agr.pas.Q,test="Chisq")

#dens
den<-read.table("Data/den.csv",header = TRUE,sep = ",")
plot(den$dens_link~den$agricultura)
eeee <- den[-38,]
eee <- eeee[-45,]
ee <- eee[-47,]
plot(ee$dens_link~ee$agricultura)

agr.pas.den <- lm(dens_link ~ agricultura + pastagem, data = ee)
summary(agr.pas.den)
drop1(agr.pas.den,test="Chisq")



#link
link<-read.table("Data/num_link.csv",header = TRUE,sep = ",")
link2 <- link[-37,]
agr.pas.link <- lm(num_link ~ agricultura + pastagem, data = link2)
summary(agr.pas.link)
drop1(agr.pas.link,test="Chisq")


#numero sp
num_sp<-read.table("Data/num_sp.csv",header = TRUE,sep = ",")
d <- num_sp[-6,]
agr.pas.num <- lm(numero_sp ~ agricultura + pastagem, data = d)
summary(agr.pas.num)
drop1(agr.pas.num,test="Chisq")
        
        
# I' Moran

#corrigido Murilo_2024_04_04
#NODF
xy<-read.table("Data/xy.csv",header = TRUE,sep = ",")
xyNODF <- xy[-6,]
dist.xyNODF <- as.matrix(1/dist(xyNODF, method="euclidean"))

agr.nodf <- lm(NODF ~ agricultura+pastagem, data = a)
#pas.nodf <- lm(NODF ~ pastagem, data = a)
resNOFagr <- residuals(agr.nodf)
#resNODFpas <- residuals(pas.nodf)

resNODF1 <- Moran.I(resNOFagr,dist.xyNODF,scaled=TRUE)
resNODF1
# resNODF2 <- Moran.I(resNODFpas,dist.xyNODF,scaled=TRUE)
# resNODF2



#H2
xy<-read.table("Data/xy.csv",header = TRUE,sep = ",")
xyH2 <- xy[1:44,]
dist.xyH2 <- as.matrix(1/dist(xyH2, method="euclidean"))

agr.H2 <- lm(H2 ~ agricultura+pastagem, data = aa)
# pas.H2 <- lm(H2 ~ pastagem, data = aa)
resH2agr <- residuals(agr.H2)
# resH2pas <- residuals(pas.H2)

resH2p <- Moran.I(resH2agr,dist.xyH2,scaled=TRUE)
resH2p
# resHpp <- Moran.I(resH2pas,dist.xyH2,scaled=TRUE)
# resHpp


#Q
xy<-read.table("Data/xy.csv",header = TRUE,sep = ",")
xyQ <- xy[-3,]
dist.xyQ <- as.matrix(1/dist(xyQ, method="euclidean"))

agr.Q <- lm(Q ~ agricultura+pastagem, data = b)
# pas.Q <- lm(Q ~ pastagem, data = b)
resQagr <- residuals(agr.Q)
# resQpas <- residuals(pas.Q)

resQ1 <- Moran.I(resQagr,dist.xyQ,scaled=TRUE)
resQ1
# resQ2 <- Moran.I(resQpas,dist.xyQ,scaled=TRUE)
# resQ2


#numero de links
xy<-read.table("Data/xy.csv",header = TRUE,sep = ",")
xylink <- xy[-37,]
dist.xylink <- as.matrix(1/dist(xylink, method="euclidean"))

agr.link <- lm(num_link ~ agricultura+pastagem, data = link2)
# pas.link <- lm(num_link ~ pastagem, data = link2)
reslinkagr <- residuals(agr.link)
# reslinkpas <- residuals(pas.link)

reslink1 <- Moran.I(reslinkagr,dist.xylink,scaled=TRUE)
reslink1
# reslink2 <- Moran.I(reslinkpas,dist.xylink,scaled=TRUE)
# reslink2




#Densidade de link
xy<-read.table("Data/xy.csv",header = TRUE,sep = ",")
eeee <- xy[-38,]
eee <- eeee[-45,]
xyden <- eee[-47,]

dist.xyden <- as.matrix(1/dist(xyden, method="euclidean"))


agr.den <- lm(dens_link ~ agricultura+pastagem, data = ee)
# pas.den <- lm(dens_link ~ pastagem, data = ee)
resdenagr <- residuals(agr.den)
# resdenpas <- residuals(pas.den)

resden1 <- Moran.I(resdenagr,dist.xyden,scaled=TRUE)
resden1
# resden2 <- Moran.I(resdenpas,dist.xyden,scaled=TRUE)
# resden2


dados <- read.table("Data/reg_500.csv",header = TRUE,sep = ",",row.names = 1)   
#DENSIDADE DE LINK
#RETIRA 38 46 49)
eeee <- dados[-38,]
eee <- eeee[-45,]
ee <- eee[-47,]
dens <-lm(ee$dens_link ~ ee$uso)
resdenagr <- residuals(dens)
resden11 <- Moran.I(resdenagr,dist.xyden,scaled=TRUE)
resden11

dens <-lm(ee$dens_link ~ ee$uso)        



#Numero de sp
xy<-read.table("Data/xy.csv",header = TRUE,sep = ",")
xysp <- xy[-6,]
dist.xysp <- as.matrix(1/dist(xysp, method="euclidean"))


agr.sp <- lm(numero_sp ~ agricultura+pastagem, data = d)
# pas.sp <- lm(numero_sp ~ pastagem, data = d)
resspagr <- residuals(agr.sp)
# ressppas <- residuals(pas.sp)

ressp1 <- Moran.I(resspagr,dist.xysp,scaled=TRUE)
ressp1
# ressp2 <- Moran.I(ressppas,dist.xysp,scaled=TRUE)
# ressp2




# #feito pela Dianne
# #NODF
# xy<-read.table("xy.csv",header = TRUE,sep = ",")
# xyNODF <- xy[-6,]
# x<-xyNODF$lat
# y<-xyNODF$long
# k<-5
# dist.xNODF <- dist(x, method="manhattan",diag=TRUE, upper=FALSE)
# dist.yNODF <- dist(y, method="manhattan",diag=TRUE, upper=FALSE)
# dist.xyNODF <-as.matrix((dist.xNODF^2 + dist.yNODF^2)^0.5)
# 
# agr.nodf <- lm(NODF ~ agricultura, data = a)
# pas.nodf <- lm(NODF ~ pastagem, data = a)
# resNOFagr <- residuals(agr.nodf)
# resNODFpas <- residuals(pas.nodf)
# 
# resNODF1 <- Moran.I(resNOFagr,dist.xyNODF,scaled=TRUE)
# resNODF1
# resNODF2 <- Moran.I(resNODFpas,dist.xyNODF,scaled=TRUE)
# resNODF2
# vif(agr.pas.nodf)
        
        
        
# #H2
# xy<-read.table("xy.csv",header = TRUE,sep = ",")
# xyH2 <- xy[1:44,]
# x<-xyH2$lat
# y<-xyH2$long
# k<-5
# dist.xH2 <- dist(x, method="manhattan",diag=TRUE, upper=FALSE)
# dist.yH2 <- dist(y, method="manhattan",diag=TRUE, upper=FALSE)
# dist.xyH2 <-as.matrix((dist.xH2^2 + dist.yH2^2)^0.5)
# 
# agr.H2 <- lm(H2 ~ agricultura, data = aa)
# pas.H2 <- lm(H2 ~ pastagem, data = aa)
# resH2agr <- residuals(agr.H2)
# resH2pas <- residuals(pas.H2)
# 
# resH2p <- Moran.I(resH2agr,dist.xyH2,scaled=TRUE)
# resH2p
# resHpp <- Moran.I(resH2pas,dist.xyH2,scaled=TRUE)
# resHpp
# 
# vif(agr.pas.nodf)
        

# #Q
# xy<-read.table("xy.csv",header = TRUE,sep = ",")
# xyQ <- xy[-3,]
# x<-xyQ$lat
# y<-xyQ$long
# k<-5
# dist.xQ <- dist(x, method="manhattan",diag=TRUE, upper=FALSE)
# dist.yQ <- dist(y, method="manhattan",diag=TRUE, upper=FALSE)
# dist.xyQ <-as.matrix((dist.xQ^2 + dist.yQ^2)^0.5)
# 
# agr.Q <- lm(Q ~ agricultura, data = b)
# pas.Q <- lm(Q ~ pastagem, data = b)
# resQagr <- residuals(agr.Q)
# resQpas <- residuals(pas.Q)
# 
# resQ1 <- Moran.I(resQagr,dist.xyQ,scaled=TRUE)
# resQ1
# resQ2 <- Moran.I(resQpas,dist.xyQ,scaled=TRUE)
# resQ2
# 
# vif(agr.pas.nodf)
        
        
        
# #numero de links
# xy<-read.table("xy.csv",header = TRUE,sep = ",")
# xylink <- xy[-37,]
# x<-xylink$lat
# y<-xylink$long
# k<-5
# dist.xlink <- dist(x, method="manhattan",diag=TRUE, upper=FALSE)
# dist.ylink <- dist(y, method="manhattan",diag=TRUE, upper=FALSE)
# dist.xylink <-as.matrix((dist.xlink^2 + dist.ylink^2)^0.5)
# 
# agr.link <- lm(num_link ~ agricultura, data = c)
# pas.link <- lm(num_link ~ pastagem, data = c)
# reslinkagr <- residuals(agr.link)
# reslinkpas <- residuals(pas.link)
# 
# reslink1 <- Moran.I(reslinkagr,dist.xylink,scaled=TRUE)
# reslink1
# reslink2 <- Moran.I(reslinkpas,dist.xylink,scaled=TRUE)
# reslink2


        
        

# #Densidade de link
# xy<-read.table("xy.csv",header = TRUE,sep = ",")
# eeee <- dados[-38,]
# eee <- eeee[-45,]
# xyden <- eee[-47,]
# 
# x<-xyden$lat
# y<-xyden$long
# k<-5
# dist.xden <- dist(x, method="manhattan",diag=TRUE, upper=FALSE)
# dist.yden <- dist(y, method="manhattan",diag=TRUE, upper=FALSE)
# dist.xyden <-as.matrix((dist.xden^2 + dist.yden^2)^0.5)
# 
# agr.den <- lm(dens_link ~ agricultura, data = ee)
# pas.den <- lm(dens_link ~ pastagem, data = ee)
# resdenagr <- residuals(agr.den)
# resdenpas <- residuals(pas.den)
# 
# resden1 <- Moran.I(resdenagr,dist.xyden,scaled=TRUE)
# resden1
# resden2 <- Moran.I(resdenpas,dist.xyden,scaled=TRUE)
# resden2
# vif(agr.pas.nodf)
#    
# dados <- read.table("reg_500.csv",header = TRUE,sep = ",",row.names = 1)   
# #DENSIDADE DE LINK
# #RETIRA 38 46 49)
# eeee <- dados[-38,]
# eee <- eeee[-45,]
# ee <- eee[-47,]
# dens <-lm(ee$dens_link ~ ee$uso)
# resdenagr <- residuals(dens)
# resden11 <- Moran.I(resdenagr,dist.xyden,scaled=TRUE)
# resden11
# dens <-lm(ee$dens_link ~ ee$uso)        



# #Numero de sp
# xy<-read.table("xy.csv",header = TRUE,sep = ",")
# xysp <- xy[-6,]
# x<-xysp$lat
# y<-xysp$long
# k<-5
# dist.xsp <- dist(x, method="manhattan",diag=TRUE, upper=FALSE)
# dist.ysp <- dist(y, method="manhattan",diag=TRUE, upper=FALSE)
# dist.xysp <-as.matrix((dist.xsp^2 + dist.ysp^2)^0.5)
# 
# agr.sp <- lm(numero_sp ~ agricultura, data = d)
# pas.sp <- lm(numero_sp ~ pastagem, data = d)
# resspagr <- residuals(agr.sp)
# ressppas <- residuals(pas.sp)
# 
# ressp1 <- Moran.I(resspagr,dist.xysp,scaled=TRUE)
# ressp1
# ressp2 <- Moran.I(ressppas,dist.xysp,scaled=TRUE)
# ressp2
# vif(agr.pas.nodf)
        
        
        
