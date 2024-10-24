##REGRESS?O SIMPLES - METRICAS X USO DO SOLO ### Dianne Silva ###

library(ggplot2)
library(gridExtra)
library(car)
library(nlme)
library(lme4)
library(ape)
library(nlme)
library(lattice)
library(raster)
library(maps)

dados <- read.table("reg_500.csv",header = TRUE,sep = ",",row.names = 1)

##Retirei um (1) outliers para NODF, Q, numero de sp e especializa??o tr?fica.. normalidade dos res?duos OK
##DENSIDADE DE LIGA??O retirei 3 outliers (38 46 49) de acordo com a distancia de cooks
#temos um problema para NUMERO DE LINKS..mesmo distancias de cooks n?o sendo maor que 1 pra nenhuma unidade amostral..os res?duos da an?lises n?o tem distribui??o normal..mesmo tirando 1, 2 ou 3 outliers da an?lise ainda continua n?o dando normal...e agora???


##########################################
#Distancia de cooks

#DENSIDADE DE LIGA??O

#fit the linear regression model to the dataset with outliers
denss <- data.frame(x = c(dados$uso), y = c(dados$dens_link))

mod_dens <- lm(y ~ x, data = denss)
summary(mod_dens)
#find Cook's distance for each observation in the dataset
cooks_dens <- cooks.distance(mod_dens)

# Plot Cook's Distance with a horizontal line at 4/n to see which observations
#exceed this thresdhold
n <- nrow(denss)
plot(cooks_dens, main = "Cooks Distance for Influential Obs")
abline(h = 4/n, lty = 2, col = "steelblue") # add cutoff line

#identify influential points
influential_obs <- as.numeric(names(cooks_dens)[(cooks_dens > (4/n))])

#define new data frame with influential points removed
outliers_removed1 <- denss[-influential_obs, ]

#create scatterplot with outliers present
outliers_present <- ggplot(data = denss, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = lm) +
  ylim(-32, 26) + 
  ylab("Link density") + xlab("Anthropogenic impact gradient") +
  ggtitle("Outliers Present")

#create scatterplot with outliers removed
outliers_removed <- ggplot(data = outliers_removed1, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = lm) +
  ylim(-32, 26) + 
  ylab("Link density") + xlab("Anthropogenic impact gradient") +
  ggtitle("Outliers Removed")

#plot both scatterplots side by side
gridExtra::grid.arrange(outliers_present, outliers_removed, ncol = 2)


################
#Distancia de cooks

#NUMERO DE LINK - n?o apresentou distancia maior que 1, porem n?o tem distribui??o normal

#fit the linear regression model to the dataset with outliers
num_linkk <- data.frame(x = c(dados$uso), y = c(dados$numero_link))

mod_link <- lm(y ~ x, data = num_linkk)
summary(mod_link)

#find Cook's distance for each observation in the dataset
cooks_link <- cooks.distance(mod_link)

# Plot Cook's Distance with a horizontal line at 4/n to see which observations
#exceed this thresdhold
n <- nrow(num_linkk)
plot(cooks_link, main = "Cooks Distance for Influential Obs")

abline(h = 4/n, lty = 2, col = "steelblue") # add cutoff line

#identify influential points
influential_obs <- as.numeric(names(cooks_link)[(cooks_link > (4/n))])

#define new data frame with influential points removed
outliers_removed1 <- num_linkk[-influential_obs, ]

#create scatterplot with outliers present
outliers_present <- ggplot(data = num_linkk, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = lm) +
  ylim(-3, 1) + 
  ylab("Number of links") + xlab("Anthropogenic impact gradient") +
  ggtitle("Outliers Present")

#create scatterplot with outliers removed
outliers_removed <- ggplot(data = outliers_removed1, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = lm) +
  ylim(-3, 1) + 
  ylab("Number of links") + xlab("Anthropogenic impact gradient") +
  ggtitle("Outliers Removed")

#plot both scatterplots side by side
gridExtra::grid.arrange(outliers_present, outliers_removed, ncol = 2)


####################################################

###Regress?o simples

#NODF
#RETIRA 6 
aaa <- dados[-6,]
NODF <-lm(aaa$NODF ~ aaa$uso)
summary(NODF)
plot(aaa$NODF ~ aaa$uso,pch=21,bg="gray",cex=2)
abline(NODF)

plot(cooks.distance(NODF))
#Normalidade dos res?duos

shapiro.test(NODF$residuals)
hist(x = NODF$residuals, col = 'gray', xlab = 'Res?duos', ylab = 'Q',
     probability = TRUE) 
lines(density(NODF$residuals))

#Homocedasticidade dos res?duos
plot(x = aaa$uso,y = NODF$residuals, col = 'gray',pch = 19,
     xlab = 'uso', ylab = "Res?duosNODF")

#bartlett.test(list(dados$Q,dados$uso))

# I' Moran OK
xy<-read.table("xy.csv",header = TRUE,sep = ",")
xy <- xy[-6,]
x<-xy$lat

y<-xy$long

k<-5  #N?mero de classes de dist?ncia

#Matriz de distancias entre coordenadas X das localidades
dist.x <- dist(x, method="manhattan",diag=TRUE, upper=FALSE)
# Matriz de distancias entre coordenadas Y das localidades
dist.y <- dist(y, method="manhattan",diag=TRUE, upper=FALSE)
# Calculando a matriz de distancia euclidiana entre as localidades (D^2 = X^2 + Y^2) 
dist.xy <-as.matrix((dist.x^2 + dist.y^2)^0.5)
w<-1/(dist.xy)
diag(w)<-0

residuoNODF <- residuals(NODF)

resNODF <- Moran.I(residuoNODF,w,scaled=TRUE)
resNODF

################################


#MODULARIDADE Q
#RETIRA 3 
bb <- dados[-3,]
#bb <- bbb[-31,]
Q <-lm(bb$Q ~ bb$uso)
summary(Q)
plot(bb$Q ~ bb$uso,pch=21,bg="gray",cex=2)
abline(Q)

#Normalidade dos res?duos
shapiro.test(Q$residuals)
hist(x = Q$residuals, col = 'gray', xlab = 'Res?duos', ylab = 'Q',
     probability = TRUE) 
lines(density(Q$residuals))

#Homocedasticidade dos res?duos
plot(x = bb$uso,y = Q$residuals, col = 'gray',pch = 19,
     xlab = 'uso', ylab = "Res?duosQ")

bartlett.test(list(bb$Q ~ bb$uso))

#I'Moran OK
xy<-read.table("xy.csv",header = TRUE,sep = ",")
xy <- xy[-3,]
x<-xy$lat

y<-xy$long

k<-5  #N?mero de classes de dist?ncia

#Matriz de distancias entre coordenadas X das localidades
dist.x <- dist(x, method="manhattan",diag=TRUE, upper=FALSE)
# Matriz de distancias entre coordenadas Y das localidades
dist.y <- dist(y, method="manhattan",diag=TRUE, upper=FALSE)
# Calculando a matriz de distancia euclidiana entre as localidades (D^2 = X^2 + Y^2) 
dist.xy <-as.matrix((dist.x^2 + dist.y^2)^0.5)

residuoQ <- residuals(Q)

resQ <- Moran.I(residuoQ,dist.xy,scaled=TRUE)
resQ

#NUMERO DE LINKS
#retira 37, 49, 10
cccc <- dados[-10,]
ccc <- cccc[-36,]
cc <- ccc[-47,]

num_link <-lm(cc$numero_link ~ cc$uso)
summary(num_link)
plot(cc$numero_link ~ cc$uso,pch=21,bg="gray",cex=2)
abline(num_link)

#Normalidade dos res?duos
shapiro.test(num_link$residuals)
hist(x = num_link$residuals, col = 'gray', xlab = 'Res?duos', ylab = 'Q',
     probability = TRUE) 
lines(density(num_link$residuals))

#Homocedasticidade dos res?duos
plot(x = cc$uso,y = num_link$residuals, col = 'gray',pch = 19,
     xlab = 'uso', ylab = "Res?duosNODF")

bartlett.test(list(cc$numero_link, cc$uso))

#I'Moran
xy<-read.table("xy.csv",header = TRUE,sep = ",")
xyyyy <- dados[-10,]
xyy <- xyyyy[-36,]
xy <- xyy[-47,]

x<-xy$lat

y<-xy$long

k<-5  #N?mero de classes de dist?ncia

#Matriz de distancias entre coordenadas X das localidades
dist.x <- dist(x, method="manhattan",diag=TRUE, upper=FALSE)
# Matriz de distancias entre coordenadas Y das localidades
dist.y <- dist(y, method="manhattan",diag=TRUE, upper=FALSE)
# Calculando a matriz de distancia euclidiana entre as localidades (D^2 = X^2 + Y^2) 
dist.xy <-as.matrix((dist.x^2 + dist.y^2)^0.5)

residuonum_link <- residuals(num_link)

resnum_link <- Moran.I(residuonum_link,dist.xy,scaled=TRUE)
resnum_link




#################################

#NUMERO DE ESPECIE
#RETIRA 6
dd <- dados[-6,]
#num_sp <-lmer(numero_sp_log ~ uso + (1|lat) + (1|long),data=dd)

LMnum_sp <-lm(dd$numero_sp_log ~ dd$uso)
summary(LMnum_sp)
mod.final1<-step(LMnum_sp)
extractAIC(LMnum_sp)

#Normalidade dos res?duos
shapiro.test(LMnum_sp$residuals)
hist(x = LMnum_sp$residuals, col = 'gray', xlab = 'Res?duos', ylab = 'Q',
     probability = TRUE) 
lines(density(LMnum_sp$residuals))

#Homocedasticidade dos res?duos
plot(x = dd$uso,y = LMnum_sp$residuals, col = 'gray',pch = 19,
     xlab = 'uso', ylab = "Res?duosLMnum_sp")

bartlett.test(list(dd$numero_sp_log,dd$uso))

#I'Moran - DEU CORRELA??O ESPACIAL
xy<-read.table("xy.csv",header = TRUE,sep = ",")
xy <- xy[-6,]
x<-xy$lat

y<-xy$long

k<-5  #N?mero de classes de dist?ncia

#Matriz de distancias entre coordenadas X das localidades
dist.x <- dist(x, method="manhattan",diag=TRUE, upper=FALSE)
# Matriz de distancias entre coordenadas Y das localidades
dist.y <- dist(y, method="manhattan",diag=TRUE, upper=FALSE)
# Calculando a matriz de distancia euclidiana entre as localidades (D^2 = X^2 + Y^2) 
dist.xy <-as.matrix((dist.x^2 + dist.y^2)^0.5)
w<-1/(dist.xy)
diag(w)<-0

residuonum_sp <- residuals(LMnum_sp)

resnum_sp <- Moran.I(residuonum_sp,dist.xy,scaled=TRUE)
resnum_sp

#dbMEM
library(adespatial)
dbmem1 <- dbmem(cbind(x,y), MEM.autocor = "positive", silent = FALSE)
(res_select <- forward.sel(Y = dd$numero_sp_log, 
                             X = dbmem1, verbose = F))
spat_mem <- as.matrix(dbmem1[, res_select$order])

LMnum_sp_spat <-lm(dd$numero_sp_log ~ dd$uso+spat_mem)
summary(LMnum_sp_spat)
residuonum_sp_spat <- residuals(LMnum_sp_spat)

resnum_sp <- Moran.I(residuonum_sp_spat,dist.xy,scaled=TRUE)
resnum_sp


## Deu correla??o espacial...agora preciso controlar essa varia??o...incluindo ela no modelo

LMEnum_sp <- lme(numero_sp_log ~ uso, data = dd, random = ~1|lat/long)
summary(LMEnum_sp)

anova(LMnum_sp, LMEnum_sp)
#AIC  LM=-42  LME=112

#PLOTs
plot(numero_sp_log ~ uso, data = dd, random = ~1|lat + long)#n?o sei se esta certo
plot(effects::effect(mod = num_sp,"uso"))
shapiro.test(num_sp$residuals)
plot(dd$numero_sp_log ~ dd$uso,pch=21,bg="gray",cex=2)
abline(num_sp)


###############################

#DENSIDADE DE LINK
#RETIRA 38 46 49)
eeee <- dados[-38,]
eee <- eeee[-45,]
ee <- eee[-47,]

LMdens_link <-lm(ee$dens_link ~ ee$uso)
summary(LMdens_link)
mod.final1<-step(LMdens_link)
extractAIC(LMdens_link)

#Normalidade dos res?duos
shapiro.test(LMdens_link$residuals)
hist(x = LMdens_link$residuals, col = 'gray', xlab = 'Res?duos', ylab = 'Q',
     probability = TRUE) 
lines(density(LMdens_link$residuals))


#Homocedasticidade dos res?duos
plot(x = ee$uso,y = LMdens_link$residuals, col = 'gray',pch = 19,
     xlab = 'uso', ylab = "Res?duosNODF")

bartlett.test(list(ee$dens_link,ee$uso))

#I'Moran -  DEU CORRELA??O ESPACIAL
xy<-read.table("xy.csv",header = TRUE,sep = ",")
xyyyy <- dados[-38,]
xyy <- xyyyy[-45,]
xy <- xyy[-47,]

x<-xy$lat

y<-xy$long

k<-5  #N?mero de classes de dist?ncia

#Matriz de distancias entre coordenadas X das localidades
dist.x <- dist(x, method="manhattan",diag=TRUE, upper=FALSE)
# Matriz de distancias entre coordenadas Y das localidades
dist.y <- dist(y, method="manhattan",diag=TRUE, upper=FALSE)
# Calculando a matriz de distancia euclidiana entre as localidades (D^2 = X^2 + Y^2) 
dist.xy <-as.matrix((dist.x^2 + dist.y^2)^0.5)

residuodens_link <- residuals(LMdens_link)

resdens_link <- Moran.I(residuodens_link,dist.xy,scaled=TRUE)
resdens_link

## Deu correla??o espacial...agora preciso controlar essa varia??o...incluindo ela no modelo

LMEdens_link <- lme(dens_link ~ uso, data = ee, random = ~1|lat/long)#n?o sei se esta certo
summary(LMEdens_link)

anova(LMdens_link, LMEdens_link)
#AIC  LM=-42  LME=112



##############################

#ESPECIALIZA??O TROFICA H2
a <- dados[1:44,] 
H2<-lm(a$H2 ~a$uso)
summary(H2)
plot(a$H2 ~a$uso,pch=21,bg="gray",cex=2)
abline(dens)

#Normalidade dos res?duos
shapiro.test(H2$residuals)
hist(x = H2$residuals, col = 'gray', xlab = 'Res?duos', ylab = 'Q',
     probability = TRUE) 
lines(density(H2$residuals))


#Homocedasticidade dos res?duos
plot(x = a$uso,y = H2$residuals, col = 'gray',pch = 19,
     xlab = 'uso', ylab = "Res?duosNODF")

bartlett.test(list(a$H2, a$uso))


#I'Moran
#para H2
xy<-read.table("xy.csv",header = TRUE,sep = ",")
a <- xy[1:44,]
xx<-a$lat
yy<-a$long

k<-5  #N?mero de classes de dist?ncia

dist.xx <- dist(xx, method="manhattan",diag=TRUE, upper=FALSE)
dist.yy <- dist(yy, method="manhattan",diag=TRUE, upper=FALSE)
dist.xyxy <-as.matrix((dist.xx^2 + dist.yy^2)^0.5)
xyxy<-dist(a) #Matriz de dist?ncia euclidiana
incremento<-max(a)/k ## Estimativa do incremento, para o calculo do correlograma

residuoH2 <- residuals(H2)

resH2 <- Moran.I(residuoH2,dist.xyxy,scaled=TRUE)
resH2



##########################################################

