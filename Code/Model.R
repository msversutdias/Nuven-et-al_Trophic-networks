##REGRESSAO SIMPLES - METRICAS X USO DO SOLO ### Dianne Silva ###

# library(ggplot2)
# library(gridExtra)
library(car)
# library(lme4)
library(ape)
#library(nlme)
#library(lattice)
#library(raster)
#library(maps)

dados <- read.table("Data/reg_500.csv",header = TRUE,sep = ",",row.names = 1)
dados$X<-1:49

dados_conf <- read.table("Data/metric_results_2024_05_15.csv",header = TRUE,
                         sep = ";",row.names = 1)
colnames(dados_conf)<-paste0(colnames(dados_conf),"_conf")
dados_conf$ID<-paste0("rede",1:nrow(dados_conf))
head(dados_conf)

dados<-merge(x=dados,y=dados_conf,by="ID",all.x=TRUE)
dados<-dados[order(dados$X),]
rownames(dados)<-dados$X
dim(dados)
dados$ID

head(dados)
plot(NODF~NODF_conf,data=dados);abline(a=0,b=1)
plot(dens_link~dens_conf,data=dados);abline(a=0,b=1)
plot(H2~H2_conf,data=dados);abline(a=0,b=1)
plot(Q~Q_conf,data=dados);abline(a=0,b=1)
plot(numero_sp~nsp_conf,data=dados);abline(a=0,b=1)

##Retirei um (1) outliers para NODF, Q, numero de sp e especializao trofica.. normalidade dos residuos OK
##DENSIDADE DE LIGACAO retirei 3 outliers (38 46 49) de acordo com a distancia de cooks
#temos um problema para NUMERO DE LINKS..mesmo distancias de cooks n?o sendo maor que 1 pra nenhuma unidade amostral..os residuos da analises nao tem distribuio normal..mesmo tirando 1, 2 ou 3 outliers da analise ainda continua nao dando normal...e agora???


# ##########################################
# #Distancia de cooks
# 
# #DENSIDADE DE LIGACO
# 
# #fit the linear regression model to the dataset with outliers
# denss <- data.frame(x = c(dados$uso), y = c(dados$dens_link))
# 
# mod_dens <- lm(y ~ x, data = denss)#[-influential_obs,]
# summary(mod_dens)
# shapiro.test(resid(mod_dens))
# hist(resid(mod_dens))
# 
# #find Cook's distance for each observation in the dataset
# cooks_dens <- cooks.distance(mod_dens)
# 
# 
# # Plot Cook's Distance with a horizontal line at 4/n to see which observations
# #exceed this threshold
# n <- nrow(denss)
# plot(cooks_dens, main = "Cooks Distance for Influential Obs")
# abline(h = 4/n, lty = 2, col = "steelblue") # add cutoff line
# 
# #identify influential points
# influential_obs <- as.numeric(names(cooks_dens)[(cooks_dens > (4/n))])
# 
# #define new data frame with influential points removed
# outliers_removed1 <- denss[-influential_obs, ]
# 
# mod_dens2 <- lm(y ~ x, data = outliers_removed1)#[-influential_obs,]
# summary(mod_dens2)
# shapiro.test(resid(mod_dens2))
# hist(resid(mod_dens2))
# 
# 
# #create scatterplot with outliers present
# outliers_present <- ggplot(data = denss, aes(x = x, y = y)) +
#   geom_point() +
#   geom_smooth(method = lm) +
#   ylim(-32, 26) + 
#   ylab("Link density") + xlab("Anthropogenic impact gradient") +
#   ggtitle("Outliers Present")
# 
# #create scatterplot with outliers removed
# outliers_removed <- ggplot(data = outliers_removed1, aes(x = x, y = y)) +
#   geom_point() +
#   geom_smooth(method = lm) +
#   ylim(-32, 26) + 
#   ylab("Link density") + xlab("Anthropogenic impact gradient") +
#   ggtitle("Outliers Removed")
# 
# #plot both scatterplots side by side
# gridExtra::grid.arrange(outliers_present, outliers_removed, ncol = 2)
# 
# 
# ################
# #Distancia de cooks
# 
# #NUMERO DE LINK - nao apresentou distancia maior que 1, porem n?o tem distribuicao normal
# 
# #fit the linear regression model to the dataset with outliers
# num_linkk <- data.frame(x = c(dados$uso), y = c(dados$numero_link))
# 
# mod_link <- lm(y ~ x, data = num_linkk)
# summary(mod_link)
# shapiro.test(resid(mod_link))
# hist(resid(mod_link))
# 
# 
# #find Cook's distance for each observation in the dataset
# cooks_link <- cooks.distance(mod_link)
# 
# # Plot Cook's Distance with a horizontal line at 4/n to see which observations
# #exceed this thresdhold
# n <- nrow(num_linkk)
# plot(cooks_link, main = "Cooks Distance for Influential Obs")
# abline(h = 0.07, lty = 2, col = "steelblue") # add cutoff line
# 
# #identify influential points
# influential_obs <- as.numeric(names(cooks_link)[(cooks_link > 0.07)])
# influential_obs
# 
# # #define new data frame with influential points removed
# outliers_removed1 <- num_linkk[-influential_obs, ]
# mod_link2 <- lm(y ~ x, data = outliers_removed1)
# summary(mod_link2)
# shapiro.test(resid(mod_link2))
# hist(resid(mod_link2))
# 
# #create scatterplot with outliers present
# outliers_present <- ggplot(data = num_linkk, aes(x = x, y = y)) +
#   geom_point() +
#   geom_smooth(method = lm) +
#   ylim(-3, 1) + 
#   ylab("Number of links") + xlab("Anthropogenic impact gradient") +
#   ggtitle("Outliers Present")
# 
# #create scatterplot with outliers removed
# outliers_removed <- ggplot(data = outliers_removed1, aes(x = x, y = y)) +
#   geom_point() +
#   geom_smooth(method = lm) +
#   ylim(-3, 1) + 
#   ylab("Number of links") + xlab("Anthropogenic impact gradient") +
#   ggtitle("Outliers Removed")
# 
# #plot both scatterplots side by side
# gridExtra::grid.arrange(outliers_present, outliers_removed, ncol = 2)





####################################################




###Regressao simples

#NODF
#RETIRA 46 
aaa <- dados[-c(6),]
NODF <-lm(aaa$NODF ~ aaa$uso)
summary(NODF)
plot(aaa$NODF ~ aaa$uso,
     pch=21,bg="gray",cex=2,
     ylab="Nestedness (sesNODF)",
     xlab="Land use gradient (%)")
#abline(NODF)

plot(cooks.distance(NODF))
sort(cooks.distance(NODF),
     decreasing=TRUE)[1:3]#line 6,44, 24
#Normalidade dos res?duos

shapiro.test(NODF$residuals)
hist(x = NODF$residuals, col = 'gray', xlab = 'Res?duos', ylab = 'Q',
     probability = TRUE) 
lines(density(NODF$residuals))

#Homocedasticidade dos res?duos
plot(x = aaa$uso,y = NODF$residuals, col = 'gray',pch = 19,
     xlab = 'uso', ylab = "ResiduosNODF")

#bartlett.test(list(dados$Q,dados$uso))

# I' Moran OK
xy<-read.table("Data/xy.csv",header = TRUE,sep = ",")
xy <- xy[-c(6),]
y<-xy$lat

x<-xy$long

# Calculando a matriz de distancia euclidiana entre as localidades
dist.xy <-as.matrix(dist(cbind(x,y)))
w<-1/(dist.xy)
diag(w)<-0

residuoNODF <- residuals(NODF)

resNODF <- Moran.I(residuoNODF,w,scaled=TRUE)
resNODF


################################


#MODULARIDADE Q
#RETIRA 3 
bb <- dados[-c(3),]

Q <-lm(bb$Q ~ bb$uso)
summary(Q)
drop1(Q,test = "F")
plot(bb$Q ~ bb$uso,pch=21,bg="gray",cex=2,
     ylab="Modularity (sesQ)",
     xlab="Land use gradient (%)")
abline(Q,lwd=3)

plot(cooks.distance(Q))
sort(cooks.distance(Q),
     decreasing = TRUE)[1:3]#line 3 & 32


#Normalidade dos res?duos
shapiro.test(Q$residuals)
hist(x = Q$residuals, col = 'gray', xlab = 'Res?duos', ylab = 'Q',
     probability = TRUE) 
lines(density(Q$residuals))

#Homocedasticidade dos res?duos
plot(x = bb$uso,y = Q$residuals, col = 'gray',pch = 19,
     xlab = 'uso', ylab = "Res?duosQ")

#I'Moran OK
xy<-read.table("Data/xy.csv",header = TRUE,sep = ",")
xy <- xy[-c(3),]
y<-xy$lat

x<-xy$long

# Calculando a matriz de distancia euclidiana entre as localidades (D^2 = X^2 + Y^2) 
dist.xy <-as.matrix(dist(cbind(x,y)))
w<-1/(dist.xy)
diag(w)<-0


residuoQ <- residuals(Q)

resQ <- Moran.I(residuoQ,dist.xy,scaled=TRUE)
resQ



################################


#NUMERO DE LINKS
cc <- dados[-c(33,2,1),]

num_link <-lm(cc$link_conf ~ cc$uso)
summary(num_link)
drop1(num_link,test="F")
plot(cc$link_conf ~ cc$uso,pch=21,bg="gray",cex=2,
     ylab="Number of links",
     xlab="Land use gradient (%)")
#abline(num_link)

plot(cooks.distance(num_link))
sort(cooks.distance(num_link),
     decreasing = TRUE)[1:3]#line 33, 2, 1


#Normalidade dos res?duos
shapiro.test(num_link$residuals)
hist(x = num_link$residuals, col = 'gray', xlab = 'Res?duos', ylab = 'Q',
     probability = TRUE) 
lines(density(num_link$residuals))

#Homocedasticidade dos res?duos
plot(x = cc$uso,y = num_link$residuals, col = 'gray',pch = 19,
     xlab = 'uso', ylab = "Res?duosNODF")


#I'Moran
xy<-read.table("Data/xy.csv",header = TRUE,sep = ",")
xy <- xy[-c(33,2,1),]

y<-xy$lat

x<-xy$long


# Calculando a matriz de distancia euclidiana entre as localidades (D^2 = X^2 + Y^2) 
dist.xy <-as.matrix(dist(cbind(x,y)))
w<-1/(dist.xy)
diag(w)<-0

residuonum_link <- residuals(num_link)

resnum_link <- Moran.I(residuonum_link,dist.xy,scaled=TRUE)
resnum_link




#################################

#NUMERO DE ESPECIE
#RETIRA 6
dd <- dados[-c(6,43),]
dd$numero_sp_log<-log10(dd$nsp_conf+1)

LMnum_sp <-lm(dd$numero_sp_log ~ dd$uso)
summary(LMnum_sp)
drop1(LMnum_sp,test="F")
plot(dd$numero_sp_log ~ dd$uso,pch=21,bg="gray",cex=2,
     ylab="Number of species (log[x+1])",
     xlab="Land use gradient (%)")
abline(LMnum_sp)

plot(cooks.distance(LMnum_sp))
sort(cooks.distance(LMnum_sp),
     decreasing = TRUE)[1:3]#line 6, 43 & 12


#Normalidade dos res?duos
shapiro.test(LMnum_sp$residuals)
hist(x = LMnum_sp$residuals, col = 'gray', xlab = 'Res?duos', ylab = 'Q',
     probability = TRUE) 
lines(density(LMnum_sp$residuals))

#Homocedasticidade dos res?duos
plot(x = dd$uso,y = LMnum_sp$residuals, col = 'gray',pch = 19,
     xlab = 'uso', ylab = "ResiduosLMnum_sp")


#I'Moran - DEU CORRELACAO ESPACIAL
xy<-read.table("Data/xy.csv",header = TRUE,sep = ",")
xy <- xy[-c(6,43),]
y<-xy$lat

x<-xy$long

# Calculando a matriz de distancia euclidiana entre as localidades
dist.xy <-as.matrix(dist(cbind(x,y)))
w<-1/(dist.xy)
diag(w)<-0

residuonum_sp <- residuals(LMnum_sp)

resnum_sp <- Moran.I(residuonum_sp,dist.xy,scaled=TRUE)
resnum_sp

#dbMEM
library(adespatial)
library(vegan)
dbmem1 <- dbmem(cbind(x,y), MEM.autocor = "positive", silent = FALSE)
R2adj<-RsquareAdj(LMnum_sp)$adj.r.squared
(res_select <- forward.sel(Y = dd$numero_sp_log,#adjR2thresh = R2adj,
                             X = dbmem1, verbose = F))
spat_mem <- as.matrix(dbmem1[, res_select$order])

LMnum_sp_spat <-lm(dd$numero_sp_log ~ dd$uso + spat_mem)
summary(LMnum_sp_spat)
drop1(LMnum_sp_spat,test="F")
residuonum_sp_spat <- residuals(LMnum_sp_spat)

resnum_sp <- Moran.I(residuonum_sp_spat,dist.xy,scaled=TRUE)
resnum_sp


###############################




#DENSIDADE DE LINK
ee <- dados[-c(43,28),]

LMdens_link <-lm(ee$dens_conf ~ ee$uso)
summary(LMdens_link)
drop1(LMdens_link,test="F")
plot(ee$dens_conf ~ ee$uso,pch=21,bg="gray",cex=2,
     ylab="Link density",
     xlab="Land use gradient (%)")

plot(cooks.distance(LMdens_link))
sort(cooks.distance(LMdens_link),
     decreasing = TRUE)[1:3]#line 43, 28 & 6


#Normalidade dos res?duos
shapiro.test(LMdens_link$residuals)
hist(x = LMdens_link$residuals, col = 'gray', xlab = 'Res?duos', ylab = 'Q',
     probability = TRUE) 
lines(density(LMdens_link$residuals))


#Homocedasticidade dos res?duos
plot(x = ee$uso,y = LMdens_link$residuals, col = 'gray',pch = 19,
     xlab = 'uso', ylab = "ResiduosNODF")


#I'Moran -  DEU CORRELA??O ESPACIAL
xy<-read.table("Data/xy.csv",header = TRUE,sep = ",")
xy <- xy[-c(43,28),]

y<-xy$lat

x<-xy$long

# Calculando a matriz de distancia euclidiana entre as localidades
dist.xy <-as.matrix(dist(cbind(x,y)))
w<-1/(dist.xy)
diag(w)<-0

residuodens_link <- residuals(LMdens_link)

resdens_link <- Moran.I(residuodens_link,dist.xy,scaled=TRUE)
resdens_link

# #dbMEM
# library(adespatial)
# dbmem1 <- dbmem(cbind(x,y), MEM.autocor = "positive", silent = FALSE)
# R2adj<-RsquareAdj(LMdens_link)$adj.r.squared   
# (res_select <- forward.sel(Y = ee$dens_link, adjR2thresh=R2adj, 
#                            X = dbmem1, verbose = F))
# spat_mem <- as.matrix(dbmem1[, res_select$order])
# 
# LMnum_sp_spat <-lm(ee$dens_link ~ ee$uso + spat_mem)
# summary(LMnum_sp_spat)
# residuonum_sp_spat <- residuals(LMnum_sp_spat)
# 
# resnum_sp <- Moran.I(residuonum_sp_spat,dist.xy,scaled=TRUE)
# resnum_sp




##############################

#ESPECIALIZACAO TROFICA H2
a <- dados[-c(38,46,49),] 
H2<-lm(a$H2_conf ~a$uso)
summary(H2)
drop1(H2,test="F")

plot(cooks.distance(H2))
sort(cooks.distance(H2),
     decreasing = TRUE)[1:3]#line 38, 46 & 49

plot(a$H2_conf ~a$uso,pch=21,bg="gray",cex=2,
     ylab=expression("Specialization H[2]'"),
     xlab="Land use gradient (%)")
#abline(H2)

#Normalidade dos res?duos
shapiro.test(resid(H2))
hist(x = resid(H2), col = 'gray', xlab = 'Res?duos', ylab = 'Q',
     probability = TRUE) 
lines(density(resid(H2)))


#Homocedasticidade dos res?duos
plot(x = a$uso,y = resid(H2), col = 'gray',pch = 19,
     xlab = 'uso', ylab = "Res?duosNODF")



#I'Moran
#para H2
xy<-read.table("Data/xy.csv",header = TRUE,sep = ",")
xy <- xy[-c(38,46,49),]
y<-xy$lat
x<-xy$long

# Calculando a matriz de distancia euclidiana entre as localidades
dist.xy <-as.matrix(dist(cbind(x,y)))
w<-1/(dist.xy)
diag(w)<-0


residuoH2 <- residuals(H2)

resH2 <- Moran.I(residuoH2,w,scaled=TRUE)
resH2

# #dbMEM
# dbmem1 <- dbmem(cbind(x,y), MEM.autocor = "positive", silent = FALSE)
# R2adj<-RsquareAdj(H2)$adj.r.squared
# (res_select <- forward.sel(Y = a$H2, adjR2thresh=R2adj, 
#                            X = dbmem1, verbose = F))
# spat_mem <- as.matrix(dbmem1[, res_select$order])
# 
# H2_spat<-lm(a$H2 ~a$uso + spat_mem)
# summary(H2_spat)
# 
# residuoH2_spat <- residuals(H2_spat)
# 
# resH2 <- Moran.I(residuoH2_spat,w,scaled=TRUE)
# resH2

##########################################################



#making Figure 2

jpeg("Output/Figure2.jpeg",
     width = 1600,height = 800,
     res = 200,pointsize = 10)
par(mfrow=c(2,3),
    mar=c(5,5,1,1),cex.lab=1.4,las=1)
#nodf
plot(aaa$NODF_conf ~ aaa$uso,
     pch=21,bg="gray",cex=1.5,
     ylab=expression("Nestedness (NODF"[ses]*")"),
     xlab="")

#modularidade
plot(bb$Q_conf ~ bb$uso,pch=21,bg="gray",cex=1.5,
     ylab=expression("Modularity (Q"[ses]*")"),
     xlab="")
abline(Q,lwd=3)


#especialização trofica
plot(a$H2_conf ~a$uso,pch=21,bg="gray",cex=1.5,
     ylab=expression("Specialization (H"[2] *"'" [SES] *  ")"),
     xlab="")

#number of species
plot(dd$numero_sp_log ~ dd$uso,pch=21,bg="gray",cex=1.5,
     ylab=expression("Number of species (log"[x+1]*")"),
     xlab="Land use gradient (%)")


#number of links
plot(cc$link_conf ~ cc$uso,pch=21,bg="gray",cex=1.5,
     ylab=expression( "Number of links"),
     xlab="Land use gradient (%)")
abline(num_link,lwd=3)

#density of links
plot(ee$dens_conf ~ ee$uso,pch=21,bg="gray",cex=1.5,
     ylab=expression( "Link density"),
     xlab="Land use gradient (%)")
dev.off()

