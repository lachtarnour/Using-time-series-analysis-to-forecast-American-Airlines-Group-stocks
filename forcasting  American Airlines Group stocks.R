library(dplyr)
library(tidyr)
library(forecast)
library(ggplot2)
library(urca)
library(astsa)
library(caschrono)
load(file = "C:/Users/nourl/Downloads/a.RData")
first_date <-  Sys.Date()-365
last_date <- Sys.Date()
library(yfR)
###importer les données 
dt <- yf_get(tickers = "AAL", 
             first_date, last_date, 
             freq_data = 'daily') %>%
  mutate(freq = 'daily')

dt=dt%>%dplyr::select(ref_date,price_open,
                      price_high,price_low,price_close,
                      price_adjusted)%>%
  tidyr::pivot_longer(cols = 2:6)%>%rename(date=ref_date)


dt=mutate(dt,name=plyr::mapvalues(name,
                                  from=c("ref_date","price_open",
                                         "price_high","price_low","price_close",
                                         "price_adjusted"),
                                  to=c("date","open","high","low",
                                       "close","adjusted")))

dt%>%head(2)


###visualisation des series
p <- ggplot(dt, aes(x=date, y=value,color=name,group=name)) +
  geom_line() + 
  xlab("")+ylab("")+theme_bw()
p+theme(legend.position = 'top',legend.title = element_blank())
#il n'y a pas une tres grande differences entre les courbes on selectionne le prix ajusté

zz=dt%>%filter(name=='adjusted')%>%dplyr::select(value)%>%
  tibble::deframe()


zz%>%ggtsdisplay(theme = theme_bw())
#interpretation:n"est pas stationnaire ; le probleme est que phi1 est proche de 1


##hypothese : il peut s'agir d'un marche aleatoire 
zz%>%diff(difference=1)%>%ggtsdisplay(theme = theme_bw())
zz%>%lag.plot(9,c(3,3))
##verification de l'hypothese de marche aleatoire

#test de stationarité : sans tendance et sans decalage
t1=zz%>%diff(differences=1)%>%ur.df(type="none",lags=3)
summary(t1)
#interpretation:
#une seule variable significatif pour delta(W(t)) avec w(t)=delta(x(t))
#statistique du test est inferieur tau1 à 1% donc H0 a rejetée : w(t) est stationnaire

##test bruit blanc : Box_test
t2=data.frame('p'=1:30,'pvaleur'=sapply(1:30,function(i) Box.test(diff(zz,differences=1),lag=i,type="Ljung-Box")$p.value))
##interpretation
#les p valeurs sont superieurs tous a 5% : les variables sont independantes : c'est un bruit blanc 
#c'est un SARIMA(0,1,0)(0,0,0)0
#(1-B)x(t)=z(t)

#estimation des parametres
library(astsa)
fit1=sarima(xdata = zz,p = 0,d = 1,q = 0,P = 0,D = 0,Q = 0,S = 0,details = F,no.constant = T)
fit1
#log de vraissemblance est negative les données peuvent s'aguster avec le modele SARIMA
fit1$fit$loglik
#intervalle de confiance de %95 de x(t) 
sigma=fit1$fit$sigma2
IC=c(last(zz)-1.96*sqrt(sigma),last(zz)+1.96*sqrt(sigma))
fit1$fit$aic
fit1$fit$arma
sdres=fit1$fit$residuals/sqrt(sigma)
ggtsdisplay(sdres,lag.max=30,theme=theme_bw())
#ce modele ne peut pas predire car c'est aleatoire : equivalent a rnorm(1,sqrt(sigma))



###outliers
x=tsoutliers(zz,lambda = NULL)
x$index
zz[x$index]=x$replacements

###transformation
lam1=BoxCox.lambda(zz,method = "loglik",lower = 0,upper = 10)
lam1
lam2=BoxCox.lambda(zz,method = "guerrero",lower = 0,upper = 10)
lam2
m1=auto.arima(zz,lambda =lam1 )
m2=auto.arima(zz,lambda=lam2)
m1
m2
#vraissemblance positive
modele=m1
###analyse des residus
res=modele$residuals
stdres=res/sqrt(modele$sigma2)
ggtsdisplay(stdres,lag.max=30,theme = theme_bw())
##test de stationarité : sans tendance et sans decalage 
t1=ur.df(stdres,type = "none")
summary(t1)
#c'est stationnaire   
##test de bruit blanc : Box Pierce
t2=data.frame('lag'=1:30,'pvaleur'=sapply(1:30,function(i) Box.test(stdres,lag=i,type="Ljung-Box")$p.value))
t2
t2$padj=p.adjust(t2$pvaleur,method = "bonferroni")
t2
p<-ggplot(t2,aes(x = lag ,y = padj))+geom_line()+geom_point(size=2)+geom_hline(yintercept = 0.05,linetype = "dashed",col="blue")+geom_hline(yintercept = 0.0,linetype = "dashed",col="blue")+theme_bw()
p+annotate(geom = "text",x = 30,y = 0.06,label="5%")
#les p valeur sont superieur à 5% donc il sont independantes et par suite il s'agit d'un bruit blanc 


t_stat(modele)
#p valeur < 0.05 => H0 rejeter variable significatif
#p valeur > 0.05 => H0 accepter variable non significatif

#transformation boxcox
zz=ts(BoxCox(zz,lambda = lam1))
zz%>%diff(lag=5)%>%autoplot()+theme_bw()
#test stationarité sans drift sans tendance 
t1=zz%>%diff(lag=5)%>%urca::ur.df(type="none",selectlags = 'AIC')
urca::summary(t1)
### ADF avec drift
t2=zz%>%diff(lag=5)%>%urca::ur.df(type="drift",selectlags = 'AIC')
urca::summary(t2)

## ADF avec drift et tendance 
t3=zz%>%diff(lag=5)%>%urca::ur.df(type="trend",selectlags = 'AIC')
urca::summary(t3)

t2@testreg$adj.r.squared
t1@testreg$adj.r.squared
t3@testreg$adj.r.squared

### visualisation
zz%>%diff(lag=5)>%>ggtsdisplay()
orders=list(p=1,d=c(0,1),q=c(0,1,2,3,4),P=c(0,1,2),D=0,Q=0,T=c=5)
all_orders=expand.grid(orders$p,orders$d,orders$q,orders$P,orders$D,orders$Q, orders$T)

colnames(all_orders)=c("p","d","q","P","D","Q","T")
dim(all_orders)


modelss=vector('list',nrow(all_orders))
all_orders=as.matrix(all_orders)
all_orders
for(i in 1:nrow(all_orders)){
  cat('\r',i,'/',length(models),sep='')
  od=all_orders[i,1:3]
  od_s=all_orders[i,4:6]
  TT=all_orders[i,7]
  modelss[[i]]=try(Arima(zz,order=od,
                        seasonal=list(order=od_s,period=TT),
                        lambda=NULL,
                        include.drift = T))
}

### remove missing values
aux=unlist(lapply(modelss, length))
aux
models = modelss[aux>1]
length(models)

###calcul aic pour les differents modeles 
aic=rep(NA,length(models))
aic
for(i in 1:length(models)){
  aic[i]=as.numeric(models[[i]]$aic)
  }
aic=as.numeric(aic)

### Selecting models with only significant coefficients
x=rep(NA,length(models))
x
for(i in 1:length(models)){
  x[i]=try(prod(caschrono::t_stat(models[[i]])[2,]<=0.05))
}
models=models[which(x==1)]
length(models)

### Residual Analysis

x=rep(NA,length(models))
for(i in 1:length(models)){
  cat("\r",i,"/",length(models),sep="")
  m=models[[i]]
  tt=armaselect(m$residuals,nbmod = 4)
  x[i]=sum(rowSums(tt[,1:2])==0)
}
table(x)
models=models[which(x==1)]
length(models)

### Extracting AIC 

aic=unlist(lapply(models, function(x) x$aic))

j=order(aic,decreasing = F)

best_models=models[j[1:5]]


best_models[[1]]$aic

### Plotting best model
best_models[[1]]%>%forecast(h=20,level=95,lambda=NULL)%>% autoplot()+theme_bw()

### Estimation Prediction error
length(zz)
length(zz_tr)
zz_train=zz[1:710]#75% des données
zz_val=zz[711:757]
length(zz_train)
length(zz)-length(zz_train)

best_mod_tr<-fore_best_mod<-vector('list',5)
for(i in 1:5){
  od=best_models[[i]]$arma[c(1,6,2)]
  od_s=best_models[[i]]$arma[c(3,7,4)]
  TT=best_models[[i]]$arma[5]
  best_mod_tr[[i]]=Arima(zz_train,order = od,seasonal = list(order=od_s,period=TT),lambda = NULL)
  fore_best_mod[[i]]=forecast(best_mod_tr[[i]],h=length(zz)-length(zz_train),level=95,lambda=NULL)
}

simple_fore<-vector('list',3)
simple_fore[[1]]=forecast::naive(zz_train,h = length(zz)-length(zz_train),level=95)
simple_fore[[2]]=forecast::snaive(zz_train,h = length(zz)-length(zz_train),level=95)
simple_fore[[3]]=forecast::meanf(zz_train,h = length(zz)-length(zz_train),level=95)
simple_fore

error_p<-vector('list',8)
for(i in 1:8){
  if(i<=5)  error_p[[i]]=forecast::accuracy(zz_val,fore_best_mod[[i]]$mean)
  if(i>5)   error_p[[i]] =forecast::accuracy(zz_val,simple_fore[[i-5]]$mean)
}
names(error_p)=c(paste0('mod',1:5),'naive','snaive','mean')
error_p

#modele mod4 nous donne la meilleur MSE

names(error_p)=c(paste0('mod',1:5),'naive','snaive','mean')
#transformer en une matrice 
error_p_dt=plyr::ldply(error_p)                                                     

error_p_dt=error_p_dt[,1:7]
error_p_dt=error_p_dt%>%tidyr::pivot_longer(cols = 2:7)
error_p_dt
colnames(error_p_dt)[1]='model'

#graphique
error_p_dt$model=factor(error_p_dt$model,levels=unique(error_p_dt$model))
p<-ggplot(error_p_dt,aes(x=model,y=value,group = 1))+
  geom_point(size=3)+geom_line()+xlab('')+ylab('')+
  facet_wrap(vars(name),ncol=2,scales = 'free')+theme_bw()
p

#best modele selon les differents criteres 
b_model=best_mod_tr[[5]]

#modele 3 prediction
x_date=as.Date(time(zz)) #date
x_fitted=c(b_model$fitted,fore_best_mod[[5]]$mean) # fitted and point forecast
x_observed=zz # data
x_95lower=c(rep(NA,length(zz_train)),fore_best_mod[[5]]$lower) # lower bound
x_95upper=c(rep(NA,length(zz_train)),fore_best_mod[[5]]$upper) # upper bound

## data
d_pred=data.frame(date=x_date,observed=x_observed,
                  fitted=x_fitted,lower95=x_95lower,
                  upper95=x_95upper)

library(reshape2)
d_pred_w=reshape2::melt(d_pred,measure.vars = 2:3,id.vars = c(1,4:5))

p<- ggplot(data = d_pred_w, aes(x = date,y = value,col=variable)) +
  geom_line() + geom_ribbon(aes(ymin = lower95, ymax = upper95),col="black", alpha = .25) +
  ggtitle(as.character(b_model))

p+theme_bw()+scale_color_manual(labels = c("Data", "Fitted"),values = c("blue", "red"))+
  theme(legend.title = element_blank(),legend.position = "bottom")




