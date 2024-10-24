##REGRESS?O MULTIPLA - METRICAS X AGRICULTURA + PASTAGEM  ## Dianne Silva ##
#corrigido MSD_2024_04_04



library(vegan)
library(ape)
library(ade4)
library(car)
library(visreg)


#NODF - Nestedness
rm(list = ls())
NODF<-read.table("Data/nodf.csv",header = TRUE,sep = ",")
NODF$ID<-paste0("rede",NODF$X)

dados_conf <- read.table("Data/metric_results_2024_05_15.csv",header = TRUE,
                         sep = ";",row.names = 1)
colnames(dados_conf)<-paste0(colnames(dados_conf),"_conf")
dados_conf$ID<-paste0("rede",1:nrow(dados_conf))
head(dados_conf)

NODF<-merge(x=NODF,y=dados_conf,by="ID",all.x=TRUE)
NODF$ID
NODF<-NODF[order(NODF$X),]
NODF$ID
rownames(NODF)<-NODF$X

plot(sp~nsp_conf,data=NODF);abline(a=0,b=1)


a <- NODF[-c(4,48,39),]
agr.pas.nodf <- lm(NODF_conf ~ agricultura + 
                     pastagem, data = a)
summary(agr.pas.nodf)
drop1(agr.pas.nodf,test="Chisq")

shapiro.test(resid(agr.pas.nodf))
hist(resid(agr.pas.nodf))

plot(cooks.distance(agr.pas.nodf))
sort(cooks.distance(agr.pas.nodf),
     decreasing=TRUE)[1:3] #outliers 4, 48, & 39

plot(NODF_conf ~ pastagem, data = a)
plot(NODF_conf ~ agricultura , data = a)


#spatial
#NODF
xy<-read.table("Data/xy.csv",header = TRUE,sep = ",")
xyNODF <- xy[-c(4,48,39),]
dist.xyNODF <- as.matrix(1/dist(xyNODF, method="euclidean"))

resNOFagr <- residuals(agr.pas.nodf)

resNODF1 <- Moran.I(resNOFagr,dist.xyNODF,scaled=TRUE)
resNODF1




##############################
##############################

#Q _ Modularity
rm(list = ls())
Q<-read.table("Data/mod.csv",header = TRUE,sep = ",")
Q$ID<-paste0("rede",Q$X)

dados_conf <- read.table("Data/metric_results_2024_05_15.csv",header = TRUE,
                         sep = ";",row.names = 1)
colnames(dados_conf)<-paste0(colnames(dados_conf),"_conf")
dados_conf$ID<-paste0("rede",1:nrow(dados_conf))
head(dados_conf)

Q<-merge(x=Q,y=dados_conf,by="ID",all.x=TRUE)
Q$ID
Q<-Q[order(Q$X),]
Q$ID
rownames(Q)<-Q$X

plot(sp~nsp_conf,data=Q)

b <- Q[-3,]
agr.pas.Q <- lm(Q_conf ~ agricultura + pastagem, data = b)
summary(agr.pas.Q)
drop1(agr.pas.Q,test="F")

shapiro.test(resid(agr.pas.Q))
hist(resid(agr.pas.Q))

plot(cooks.distance(agr.pas.Q))
sort(cooks.distance(agr.pas.Q),
     decreasing=TRUE)[1:3] #3

vif(agr.pas.Q)
plot(Q_conf ~ agricultura, data = b)
plot(Q_conf ~ pastagem, data = b)

library(ggeffects)
pr <- predict_response(agr.pas.Q, "pastagem")
plot(pr, show_residuals = TRUE,show_ci = FALSE,
     dot_size = 3,alpha = 1,line_size = 2)



#spatial
#Q
xy<-read.table("Data/xy.csv",header = TRUE,sep = ",")
xyQ <- xy[-3,]
dist.xyQ <- as.matrix(1/dist(xyQ, method="euclidean"))

resQagr <- residuals(agr.pas.Q)

resQ1 <- Moran.I(resQagr,dist.xyQ,scaled=TRUE)
resQ1




##############################
##############################

library(adespatial)

#H2 - Trophic Specialization
rm(list = ls())
H2<-read.table("Data/H2.csv",header = TRUE,sep = ",")
H2$ID<-paste0("rede",H2$X)

dados_conf <- read.table("Data/metric_results_2024_05_15.csv",header = TRUE,
                         sep = ";",row.names = 1)
colnames(dados_conf)<-paste0(colnames(dados_conf),"_conf")
dados_conf$ID<-paste0("rede",1:nrow(dados_conf))
head(dados_conf)

H2<-merge(x=H2,y=dados_conf,by="ID",all.x=TRUE)
H2<-H2[order(H2$X),]
H2<-H2[!is.na(H2$X),]
rownames(H2)<-H2$X
dim(H2)
H2$ID

plot(sp~nsp_conf,data=H2)

aa <- H2[-c(38),] 
agr.pas.H2 <- lm(H2_conf ~ agricultura + pastagem, data = aa)
summary(agr.pas.H2)
drop1(agr.pas.H2,test="Chisq")

shapiro.test(resid(agr.pas.H2))
hist(resid(agr.pas.H2))

plot(cooks.distance(agr.pas.H2))
sort(cooks.distance(agr.pas.H2),
     decreasing=TRUE)[1:3] #38, 33, 42


#spatial
#H2
xy<-read.table("Data/xy.csv",header = TRUE,sep = ",")
xy <- xy[H2$X,]
xy <- xy[-38,]
dim(xy)
w <- as.matrix(1/dist(xy, method="euclidean"))
dim(w)

resH2agr <- resid(agr.pas.H2)
length(resH2agr)

resH2p <- Moran.I(resH2agr,w,scaled=TRUE)
resH2p

# #dbMEM
# dbmem1 <- dbmem(xyH2, MEM.autocor = "positive", silent = FALSE)
# R2adj<-RsquareAdj(agr.pas.H2)$adj.r.squared
# (res_select <- forward.sel(Y = aa$H2_conf,adjR2thresh=R2adj, 
#                            X = dbmem1, verbose = F))
# spat_mem <- as.matrix(dbmem1[, res_select$order])
# 
# agr.pas.H2_spat <- lm(H2 ~ agricultura + pastagem + spat_mem, data = aa)
# summary(agr.pas.H2_spat)
# 
# residuoH2_spat <- residuals(agr.pas.H2_spat)
# 
# resH2 <- Moran.I(residuoH2_spat,w,scaled=TRUE)
# resH2

##############################
##############################




# dens - Density of Links
rm(list = ls())
den<-read.table("Data/den.csv",header = TRUE,sep = ",")
den$X<-1:49
den$ID<-paste0("rede",den$X)

dados_conf <- read.table("Data/metric_results_2024_05_15.csv",header = TRUE,
                         sep = ";",row.names = 1)
colnames(dados_conf)<-paste0(colnames(dados_conf),"_conf")
dados_conf$ID<-paste0("rede",1:nrow(dados_conf))
head(dados_conf)

den<-merge(x=den,y=dados_conf,by="ID",all.x=TRUE)
den<-den[order(den$X),]
rownames(den)<-den$X
dim(den)
den$ID

plot(den$sp~den$nsp_conf);abline(a=0,b=1)

aa<-den[-c(43,6),]

agr.pas.den <- lm(dens_conf ~ agricultura + pastagem, data = aa)
summary(agr.pas.den)
drop1(agr.pas.den,test="F")

shapiro.test(resid(agr.pas.den))
hist(resid(agr.pas.den))

plot(cooks.distance(agr.pas.den))
sort(cooks.distance(agr.pas.den),
     decreasing=TRUE)[1:3] #43 & 6

#spatial
#Densidade de link
xy<-read.table("Data/xy.csv",header = TRUE,sep = ",")
xy <- xy[-c(43,6),]

dist.xyden <- as.matrix(1/dist(xy, method="euclidean"))

resdenagr <- residuals(agr.pas.den)

resden1 <- Moran.I(resdenagr,dist.xyden,scaled=TRUE)
resden1

##############################
##############################



# link - Links per species 
rm(list = ls())
link<-read.table("Data/num_link.csv",header = TRUE,sep = ",")
link$ID<-paste0("rede",link$X)

dados_conf <- read.table("Data/metric_results_2024_05_15.csv",header = TRUE,
                         sep = ";",row.names = 1)
colnames(dados_conf)<-paste0(colnames(dados_conf),"_conf")
dados_conf$ID<-paste0("rede",1:nrow(dados_conf))
head(dados_conf)

link<-merge(x=link,y=dados_conf,by="ID",all.x=TRUE)
link<-link[order(link$X),]
rownames(link)<-link$X
dim(link)
link$ID

plot(link$nsp_conf~link$sp)

link2 <- link[-c(33,17,2),]

agr.pas.link <- lm(link_conf ~ agricultura + pastagem, data = link2)
summary(agr.pas.link)
drop1(agr.pas.link,test="F")

shapiro.test(resid(agr.pas.link))
hist(resid(agr.pas.link))

plot(cooks.distance(agr.pas.link))
sort(cooks.distance(agr.pas.link),
     decreasing=TRUE)[1:3] #33, 17, 2


plot(link_conf ~ agricultura, data = link2)
plot(link_conf ~ pastagem, data = link2)

#spatial
#numero de links
xy<-read.table("Data/xy.csv",header = TRUE,sep = ",")
xylink <- xy[-c(33,17,2),]
w <- as.matrix(1/dist(xylink, method="euclidean"))

reslinkagr <- residuals(agr.pas.link)

reslink1 <- Moran.I(reslinkagr,w,scaled=TRUE)
reslink1

# #dbMEM
# dbmem1 <- dbmem(xylink, MEM.autocor = "positive", silent = FALSE)
# R2adj<-RsquareAdj(agr.pas.H2)$adj.r.squared   
# (res_select <- forward.sel(Y = link2$num_link,,adjR2thresh=R2adj,  
#                            X = dbmem1, verbose = F))
# spat_mem <- as.matrix(dbmem1[, res_select$order])
# 
# agr.pas.link_spat <- lm(num_link ~ agricultura + pastagem + 
#                           spat_mem, data = link2)
# summary(agr.pas.link_spat)
# 
# residuoH2_spat <- residuals(agr.pas.link_spat)
# 
# resH2 <- Moran.I(residuoH2_spat,w,scaled=TRUE)
# resH2


##############################
##############################



#numero sp - Species richness 
rm(list = ls())
num_sp<-read.table("Data/num_sp.csv",header = TRUE,sep = ",")
num_sp$ID<-paste0("rede",num_sp$X)

dados_conf <- read.table("Data/metric_results_2024_05_15.csv",header = TRUE,
                         sep = ";",row.names = 1)
colnames(dados_conf)<-paste0(colnames(dados_conf),"_conf")
dados_conf$ID<-paste0("rede",1:nrow(dados_conf))
head(dados_conf)

num_sp<-merge(x=num_sp,y=dados_conf,by="ID",all.x=TRUE)
num_sp<-num_sp[order(num_sp$X),]
rownames(num_sp)<-num_sp$X
dim(num_sp)
num_sp$ID

plot(num_sp$numero_sp~log(num_sp$nsp_conf+1))


d <- num_sp[-c(43,6,12),]

agr.pas.num <- lm(nsp_conf ~ agricultura + pastagem, data = d)
summary(agr.pas.num)
drop1(agr.pas.num,test="F")

shapiro.test(resid(agr.pas.num))
hist(resid(agr.pas.num))

plot(cooks.distance(agr.pas.num))
sort(cooks.distance(agr.pas.num),
     decreasing=TRUE)[1:3] #43, 6, 12

plot(numero_sp ~ agricultura, data = d)
plot(numero_sp ~ pastagem, data = d)
        
#spatial
#Numero de sp
xy<-read.table("Data/xy.csv",header = TRUE,sep = ",")
xysp <- xy[-c(43,6,12),]
w <- as.matrix(1/dist(xysp, method="euclidean"))

resspagr <- residuals(agr.pas.num)

ressp1 <- Moran.I(resspagr,w,scaled=TRUE)
ressp1

# #dbMEM
# dbmem1 <- dbmem(xysp, MEM.autocor = "positive", silent = FALSE)
# R2adj<-RsquareAdj(agr.pas.num)$adj.r.squared 
# (res_select <- forward.sel(Y = d$numero_sp, #adjR2thresh=R2adj, 
#                            X = dbmem1, verbose = F))
# spat_mem <- as.matrix(dbmem1[, res_select$order])
# 
# agr.pas.num_spat <- lm(numero_sp ~ agricultura + pastagem + 
#                           spat_mem, data = d)
# summary(agr.pas.num_spat)
# 
# resspagr_spat <- residuals(agr.pas.num_spat)
# 
# resH2 <- Moran.I(resspagr_spat,w,scaled=TRUE)
# resH2








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
        
        
        
