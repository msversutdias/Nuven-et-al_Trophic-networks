#corrected by MSD_2024_05_14
library(bipartite)

#data(Safariland)
#web <- Safariland
#web[web>1] <- 1
#res<-computeModules(web,steps = 1e6)
#plotModuleWeb(res)
#nested(web,method = "NODF")

##iniciando
arq<-sort(dir("dados2"))
#arq<-sort(dir("D:/DOUTORADO/Escrita ARTIGOS/Artigo_Redes/Análises/land_rede_quantitativo/data"))

trabalho=as.numeric(unlist(lapply(strsplit(arq,"[.]"),function(x){x[1]})))


# resu<-data.frame(trabalho=1:max(trabalho),
#                 modQ=rep(NA,max(trabalho)),
#                 nested=rep(NA,max(trabalho)),
#                 dim=NA,richItem=NA)

resu<-data.frame(trabalho=paste0("rede",resu$trabalho),
                 NODF=rep(NA,length(arq)),
                 NODF_p=rep(NA,length(arq)),
                 H2=rep(NA,length(arq)),
                 H2_p=rep(NA,length(arq)),
                 Q=rep(NA,length(arq)),
                 Q_p=rep(NA,length(arq)),
                 nsp=rep(NA,length(arq)),
                 link=rep(NA,length(arq)),
                 con=rep(NA,length(arq)),
                 dens=rep(NA,length(arq)))

                 



rownames(resu)<-resu$trabalho
head(resu)

warnings()

calculoz<-function(obsNODF,nullNODF){
  #juntar todos os valores (obs,nulls)
  tt<-c(obsNODF,nullNODF)
  #zscor
  tt.s<-(tt-mean(tt))/sd(tt)
  #hist(tt)
  
  #calcular absoluto
  tt.ss<-abs(tt.s)
  p<-length(tt.ss[tt.ss>=tt.ss[1]])/length(tt.ss)
  
  return(c(z=tt.s[1],p=p))
}

observedMetrics<-list(obsNODF=NA,obsQ=NA,obsH2=NA,obslink=NA,
     obsnsp=NA,obsdens=NA,obscon=NA)


for(i in trabalho){
  #i=7  # revisar planilha 9,35,36,38
  web <- read.table(paste("dados2",arq[i], sep = "/"), header = TRUE,sep = ",",
                    row.names = 1)
  #Is it only fish?
  print(colnames(web))
  
  webb <- decostand(web, "pa")
  #dfun(web)$dprime -> a
  #median(a)
  #calculo metricas
  (obsNODF <- nested(webb,method = "NODF"))
  (obsH2<- networklevel(web, index="H2"))
  (obsQ <- metaComputeModules(webb)@likelihood)
  (obslink<-grouplevel(webb, index="mean number of links"))
  (obsnsp <-networklevel(webb, index="number of species"))
  (obscon <-networklevel(webb, index="connectance"))
  (obsdens <-networklevel(webb, index="linkage density"))
  
  observedMetrics$obsNODF[i]<-obsNODF
  observedMetrics$obsH2[i]<-obsH2
  observedMetrics$obsQ[i]<-obsQ
  observedMetrics$obslink[i]<-obslink[1]
  observedMetrics$obsnsp[i]<-obsnsp[1]
  observedMetrics$obscon[i]<-obscon
  observedMetrics$obsdens[i]<-obsdens
  

  #H2fun(web, H2_integer=FALSE)$H2_max

  #null model
  nulls <- nullmodel (web, N=499, method =4)
  nullspa <- nullmodel (webb, N=499, method =4)
  nullNODF <-sapply(nullspa, networklevel,index="NODF")
  nullH2 <-sapply(nulls, networklevel,index="H2")#abundance data
  # nulllink <- sapply(nullspa, grouplevel,index="mean number of links")
  # nullcon <- sapply(nullspa, networklevel,index="connectance")#abundance data
  # nulldens<- sapply(nullspa, networklevel,index="linkage density")
  nullQ <- sapply(nullspa, function(x){
    metaComputeModules(x)@likelihood })


  #z and pvalues
  #NODF
  (toto <- calculoz(obsNODF,nullNODF))
  resu$NODF[i]=toto[1]
  resu$NODF_p[i]=toto[2]
  rm(toto)

  #H2
  (toto <- calculoz(obsH2,nullH2))
  resu$H2[i]=toto[1]
  resu$H2_p[i]=toto[2]
  rm(toto)

  #Q
  (toto <- calculoz(obsQ,nullQ))
  resu$Q[i]=toto[1]
  resu$Q_p[i]=toto[2]
  rm(toto)

  #number of links
  resu$link[i]=obslink[1]

  #connectance
  resu$con[i]=obscon

  #linkage density
  resu$dens[i]=obsdens

  #nspecies
  resu$nsp[i]=obsnsp[1]


  print(i)
  rm(web,webb,nulls,nullspa,nullH2,nullNODF,obsNODF,
     obsQ, obsnsp,nullQ,obscon,obsdens,obslink,obsH2,i)
}

resu


write.table(x = resu,file = "output/metric_results_2024_05_15.csv",
          sep = ";",row.names = FALSE)
 
#summary of observed values
lapply(observedMetrics, summary)
lapply(observedMetrics, sd)

round(cor(as.data.frame(observedMetrics)),2)





# library(vegan)
# ###regress?o### uso do solo x metricas
# land <- read.table("reg_2500.csv",header = TRUE,sep = ",",row.names = 1)
# land<-decostand(land, method="standardize")
# 
# plot(land$NODF~land$uso,pch=20,bg="gray",cex=2)
# 
# mod<-lm(land$NODF~land$uso)
# summary(mod)
# abline(mod)
# 
# #vif
# library(car)
# resuvif <- vif(lm(land$linksp~land$agriculture_pasture + land$natural_vegetation + land$urban_use, data=land))
# 
# 
# 
# #analse de residuo
# #shapiro.test(residuals(mod))
# 
# residuals(mod)->e
# 
# plot(e,bas.sub$Tamanho)
# 
# shapiro.test(e)
# 
# #### Modelo linear multiplo ####
# ## Carregando uma base de dados.
# reg <- read.table("regressao.csv", header = TRUE, sep = ",", row.names = 1)
# reg2 <-reg[complete.cases(reg),]
# 
# ## Visualizando uma descrição da base de dados state.x77.
# ?state.x77
# 
# ## Visualizando uma parte desta base de dados.
# head(reg)
# 
# ## Visualizando a matrix de correlação linear entre todas as variáveis
# ## presentes na base de dados.
# cor(reg)
# 
# ## Visualizando, graficamente, a correlação entre todas as variáveis.
# pairs(reg)
# 
# ## Modelo inicial, quais variáveis presentes nesta base de dados
# ## influenciam diretamente o tempo de vida da população dos 50 estados
# ## americanos (USA)
# # Inspecao visual
# 
# dotchart(sample(reg$no))
# dotchart(sample(reg$agriculture_pasture))
# dotchart(sample(reg$natural_vegetation))
# dotchart(sample(log(reg$urban_use+1)))
# 
# ###
# 
# mod4 <- lm(reg$NODF ~ reg$agriculture_pasture + reg$natural_vegetation + 
#              log(reg$urban_use+1))
# 
# ## Visualizando o sumário completo do modelo.
# 
# summary(mod4)
# plot(mod4)
# 
# par(mfrow = c(2,2))
# 
# layout(matrix(c(1,1,1,1,1,2,2,2,2,3,3,3,3,
#                 4,4,4,4,4,5,5,5,5,6,6,6,6,
#                 7,7,7,7,7,8,8,8,8,9,9,9,9), byrow = TRUE, ncol = 13))
# 
# #### Plot NODF ####
# par(mar = c(2,4,2,0))
# plot(reg$NODF ~ reg$agriculture_pasture, pch=21, bg="#56565655",cex = 1,
#      main = "Farming", ylab = "Nestedness ", xlab = NA)
# abline(mod4, lwd = 3, col = "#333333bb")
# 
# par(mar = c(2,0.5,2,0.5))
# plot(reg$NODF ~ reg$natural_vegetation, pch=21, bg="#56565655",cex=1,
#      main = "Natural vegetation", ylab = NA, xlab = NA, yaxt='n')
# par(mar = c(2,0,2,0.5))
# plot(reg$NODF ~ reg$urban_use, pch=21, bg="#56565655",cex=1, 
#      main = "Urbanization", ylab = NA, xlab = NA, yaxt='n')
# 
# #### Plot Q ####
# 
# par(mar = c(2,4,2,0))
# plot(reg$Q ~ reg$agriculture_pasture, pch=21, bg="#56565655",cex = 1,
#       ylab = "Modularity ", xlab = NA)
# abline(mod4, lwd = 3, col = "#333333bb")
# 
# par(mar = c(2,0.5,2,0.5))
# plot(reg$Q ~ reg$natural_vegetation, pch=21, bg="#56565655",cex=1,
#      ylab = NA, xlab = NA, yaxt='n')
# par(mar = c(2,0,2,0.5))
# plot(reg$Q ~ reg$urban_use, pch=21, bg="#56565655",cex=1, 
#       ylab = NA, xlab = NA, yaxt='n')
# 
# ### Plot H2 ###
# 
# par(mar = c(2,4,2,0))
# plot(reg$H2 ~ reg$agriculture_pasture, pch=21, bg="#56565655",cex = 1,
#      ylab = "Specialization ", xlab = NA)
# abline(mod4, lwd = 3, col = "#333333bb")
# 
# par(mar = c(2,0.5,2,0.5))
# plot(reg$H2 ~ reg$natural_vegetation, pch=21, bg="#56565655",cex=1,
#       ylab = NA, xlab = NA, yaxt='n')
# par(mar = c(2,0,2,0.5))
# plot(reg$H2 ~ reg$urban_use, pch=21, bg="#56565655",cex=1, 
#       ylab = NA, xlab = NA, yaxt='n')
# 
# 
# #Dados de Venter uso do solo
# 
# #uso da terra 2009 e sum?rio
# #install.packages("raster")
# #install.packages("sp")
# library(raster)
# library(sp)
# LUse<-raster('HFP2009_wgs84.grd')
# plot(LUse)
# 
# #poe$LUse <- extract(LUse, poe[,c("longitude","latitude")])
# coord <- read.table("coordenadas.csv",header = TRUE,sep = ",", row.names = 1)
# 
# 
# #o que fazer com múltiplas coordenadas?
# coord<-data.frame(trabalho=unique(trabalho),
#            lat=tapply(coord$lat,trabalho,mean),
#            long=tapply(coord$long,trabalho,mean))
#            
# 
# #install.packages("rgdal")
# 
# library(rgdal)
# library(sp)
# bas = readOGR(paste0(getwd(),"/hybas_sa_lev12_v1c"),layer = "hybas_sa_lev12_v1c")
# 
# #bas = readOGR(paste0(getwd(),"/Bacias_Hidrogr?ficas_Ottocodificada_N?vel_1"),layer = "Bacias_Hidrogr?ficas_Ottocodificada_N?vel_1")
# 
# 
# p<-data.frame(long=c(-47.88,-40),lat=c(-15.79,-17.79))
# p<-coord
# coordinates(p) <- ~long+lat
# proj4string(p) <- proj4string(bas)
# maps::map(region="brazil")
# 
# plot(p,add=T)
# # symbols(crime$media, crime$neste, circles=crime$tamanho)
# 
# #retirar somente as bacias onde há pontos
# bas.sub <- bas[!is.na(over(bas, geometry(p))), ] 
# bas.sub$trabalho=as.vector(unlist(over(bas.sub,p)))
# plot(bas.sub)
# 
# #extrair a informação do raster em toda a bacia
# bas.sub$media=extract(LUse, bas.sub,fun=median)
# bas.sub$sd=extract(LUse, bas.sub,fun=sd)
# head(bas.sub)
# 
# #ordenando resu para juntar com bas.sub
# resu<-resu[bas.sub$trabalho,]
# bas.sub@data <- data.frame(bas.sub@data,resu)
# bas.sub@data[,c("trabalho","trabalho.1")]
# 
# par(mfrow=c(1,2))
# plot(LUse,ylim=c(-40,10),xlim=c(-90,-30))
# plot(bas.sub,add=T)
# plot(p,add=T)
# 
# s <- as.vector(bas.sub$media/(max(bas.sub$media,na.rm=TRUE)))
# 
# grad <- gray(s[complete.cases(s)])
# plot(bas.sub,col=grad[length(grad):1])
# dev.off()
# 
# 
# plot(bas.sub$H2~bas.sub$media,pch=20,bg="gray",cex=2)
# H2<-lm(H2~media,bas.sub@data)
# summary(H2)
# abline(H2)
# 
# 
# 
# #Dependencia espacial
# 
# install.packages("ncf")
# 
# library(ncf)
# 
# xy<-read.table("xyexemplo2.txt", header=TRUE)
# 
# x<-xy$X
# 
# y<-xy$Y
# 
# k<-5  #N?mero de classes de dist?ncia
# 
# xy<-dist(xy) #Matriz de dist?ncia euclidiana
# 
# incremento<-max(xy)/k ## Estimativa do incremento, para o calculo do correlograma
# 
# morani<-correlog(x,y,e,increment=incremento)
# 
# morani
# 
# plot.correlog(morani)
# 
# 
# residuals(mod)
# confint(mod)
# plot(fitted(mod),residuals(mod),xlab="Valores Ajustados",ylab="Res?duos")
# abline(h=0)
# windows()
# plot(Mod,residuals(mod),xlab="Mod",ylab="media")
# abline(h=0)
# mod$residuals
# mod$fitted.values
# 
# Moran.I(mod,residuals,scaled=TRUE)
# 
# median(Mod)
# 
# var.test(residuals(mod)[Mod$media>13],residuals(mod)
#            [media$mod<13])
# 
# 
# data(resu)
# 
