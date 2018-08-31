

library('rmarkdown')
library('boot')
library(foreign)
library(data.table)
library('ggplot2')
library('tseries')
library('forecast')
library('lmtest')
library('nlme')
library('simstudy')
library('portes')
library('gridExtra')
library('kableExtra')
setwd("C:/Users/jakek/Documents/intrepidinsight/blog") 
knitr::opts_chunk$set(echo = TRUE)


## prep google trends data
g_trend_hcare<-data.table(read.csv("blog_maximize/homelessness_gtrends.csv", stringsAsFactors = FALSE))
g_trend_hcare<-g_trend_hcare[,date:=as.Date(as.character(date), "%m/%d/%Y")][,week_start := cut(date, "week", start.on.monday=FALSE)]
setkey(g_trend_hcare, date )
together<-g_trend_hcare
class(together$date)<-"Date"
setkey(together,week_start)

set.seed(9991)
together<-together[,lambda:=1/160*search_index^2]
together<-addCorGen(dtOld=together, idvar="week_start", dist="poisson", param1 = "lambda", corstr="cs", rho=0.3, nvars=1, cnames = "tweets")
together<-together[,lambda:=search_index/11-1]
together<-addCorGen(dtOld=together, idvar="week_start", dist="poisson", param1 = "lambda", corstr="ar1", rho=0.1,nvars=1, cnames = "instagram")
together<-together[,lambda:=search_index/(5+7*instagram)+ search_index/40]
together<-addCorGen(dtOld=together, idvar="week_start", dist="poisson", param1 = "lambda", corstr="ar1", rho=0.3,nvars=1, cnames = "blog")
together<-together[,lambda:=1.3*search_index]
together<-addCorGen(dtOld=together, idvar="week_start", dist="poisson", param1 = "lambda", corstr="ar1", rho=0.3,nvars=1, cnames = "email")
together$email<-rpois(nrow(together), 860)+together$email
together$directmail<-rpois(nrow(together), 500)

together$tweets<-together$tweets+rpois(nrow(together), 5)

## Time Series declaration - not strictly neccessary
ts_searches<-tsclean(ts(together$search_index, frequency=52))
ts_tweets<-tsclean(ts(together$tweets, frequency=52))
ts_instagram<-tsclean(ts(together$instagram, frequency=52))
ts_directmail<-tsclean(ts(together$directmail, frequency=52))
ts_blog<-tsclean(ts(together$blog, frequency=52))
ts_email<-tsclean(ts(together$email, frequency=52))
ts_united<-ts.union(ts_searches,ts_instagram, ts_directmail, ts_blog, ts_email, ts_tweets)

ggplot() +
  geom_line(data = together, aes(x = date, y = search_index)) + ylab('Google Search Index Score')+xlab("Date")
autoplot(ts_united, facets=TRUE)+ xlab('') +ylab('')

engage.lm<-lm(search_index ~  tweets +instagram+ blog + directmail + email, data=together)
summary(engage.lm)

together$olsresid<-engage.lm$residuals
together<-together[,week_num:=.I]
ggplot(data=together, aes(x=week_num, y=olsresid)) + geom_point()

dwtest(engage.lm)
fn <- function(obj,lags){
  test.stat <- numeric(length(lags))
  for (i in 1:length(lags))
    test.stat[i] <- -sum(diff(obj,lag=lags[i])^2)/sum(obj^2)
  test.stat
}
portest(engage.lm, lags=1:10, test = "other", fn = fn,innov.dist= "bootstrap")
portest(engage.lm$residuals, lags=1:10)

reg_arma<-auto.arima(together[,"search_index"], xreg=together[,c("tweets", "instagram", "blog", "directmail", "email")], stepwise=FALSE,approx=FALSE)
summary(reg_arma)

checkresiduals(reg_arma)
portest(reg_arma$residuals, lags=1:10)

fn <- function(obj,lags){
  test.stat <- numeric(length(lags))
  for (i in 1:length(lags))
    test.stat[i] <- -sum(diff(obj,lag=lags[i])^2)/sum(obj^2)
  test.stat
}

portest(reg_arma$residuals, lags=1:10, test = "other", fn = fn,innov.dist= "bootstrap")

aic_bic_res<-NULL
for (x in 1:5){
  combos<-combn(c( "tweets", "instagram", "blog", "directmail", "email"),x)
  for (vect in 1:ncol(combos)) {
    temp<-Arima(together[,"search_index"], order=c(0,0,1), xreg=together[,combos[,vect], with=FALSE])
    aic_bic_res<-data.table(rbind(aic_bic_res,cbind(paste0(combos[,vect], collapse=","),temp$aic, temp$bic,temp$aicc, sqrt(mean(temp$residuals^2)))))
  }
}

colnames(aic_bic_res)<-c("Variables", "AIC","BIC","AICc", "RMSE")
setkey(aic_bic_res, BIC, AICc, Variables)

kable(aic_bic_res[min(aic_bic_res$AICc)==AICc | min(aic_bic_res$BIC)==BIC]) %>%
  kable_styling(bootstrap_options = "striped", full_width = F)

aic_bic_res<-NULL
together<-together[,blog_tweets:=blog*tweets][,blog_instagram:=instagram*blog][, blog_email:=blog*email][, tweets_instagram:=tweets*instagram][,tweets_email:=tweets*email][, instagram_email:=instagram*email]
for (x in 0:6){
  combos<-combn(c( "blog_tweets", "blog_instagram", "blog_email", "tweets_instagram", "tweets_email", "instagram_email"),x)
  for (vect in 1:ncol(combos)) {
    temp<-Arima(together[,"search_index"], order=c(0,0,1), xreg=together[,c(combos[,vect],"instagram", "email", "tweets","blog"), with=FALSE])
    aic_bic_res<-data.table(rbind(aic_bic_res,cbind(paste0(combos[,vect], collapse=","),temp$aic, temp$bic,temp$aicc, sqrt(mean(temp$residuals^2)))))
  }
}

colnames(aic_bic_res)<-c("Variables", "AIC","BIC","AICc", "RMSE")
setkey(aic_bic_res, BIC, AIC, Variables)

kable(aic_bic_res[min(aic_bic_res$AICc)==AICc | min(aic_bic_res$BIC)==BIC]) %>%
  kable_styling(bootstrap_options = "striped", full_width = F)

setkey(together, week_start)
##together$residuals<-reg_arma$residuals

p1<-ggplot(together, aes(x=tweets, y=search_index))+ geom_point() +geom_smooth()
p2<-ggplot(together, aes(x=instagram, y=search_index))+ geom_point() +geom_smooth()
p3<-ggplot(together, aes(x=email, y=search_index))+ geom_point()+geom_smooth()
p4<-ggplot(together, aes(x=blog, y=search_index))+ geom_point()+geom_smooth()
grid.arrange(
  p1,
  p2,
  p3,
  p4
  
)

together<-together[,root_tweet:=(tweets)^(1/2)][, email2:=email*email]
aic_bic_res<-NULL
for (x in 0:4){
  combos<-combn(c("email", "tweets","root_tweet","email2"),x)
  for (vect in 1:ncol(combos)) {
    temp<-Arima(together[,"search_index"], order=c(0,0,1), xreg=together[,c(combos[,vect],"instagram", "blog", "blog_instagram", "tweets_instagram"), with=FALSE])
    aic_bic_res<-data.table(rbind(aic_bic_res,cbind(paste0(combos[,vect], collapse=","),temp$aic, temp$bic, temp$aicc, sqrt(mean(temp$residuals^2)))))
  }
}

colnames(aic_bic_res)<-c("Variables", "AIC","BIC", "AICc", "RMSE")
setkey(aic_bic_res, BIC,AICc, Variables)



kable(aic_bic_res[min(aic_bic_res$AICc)==AICc | min(aic_bic_res$BIC)==BIC]) %>%
  kable_styling(bootstrap_options = "striped", full_width = F)

reg_normal<-Arima(together[,"search_index"], order=c(0,0,1), xreg=together[,c("email", "root_tweet", "instagram", "blog", "blog_instagram", "tweets_instagram")])
summary(reg_normal)

# function for modifying data, courtesy of Gabriel Mohana:
adstock <- function(x, rate=0){
  return(as.numeric(stats::filter(x=x, filter=rate, method="recursive")))
}

#Preliminary OLS just to get starting values for optimization routine:
test.lm<-lm(search_index ~ email+root_tweet+instagram+blog+blog_instagram+tweets_instagram, data=together)

# Optimization routine
fit_nls<-nls(search_index ~ a+b1*adstock(email,b2)+c1*adstock(tweets, c2)^(1/2)+d1*adstock(instagram,d2)+e1*adstock(blog,e2)
             +f1*adstock(instagram,d2)*adstock(blog,e2) +g1*adstock(instagram,d2)*adstock(tweets,c2), data=together, start=list(a=                              coef(test.lm)["(Intercept)"], b1=coef(test.lm)["email"], b2 = 0.5, c1=coef(test.lm)["root_tweet"], c2=0.5,                                        d1=coef(test.lm)["instagram"] , d2=0.5, e1=coef(test.lm)["blog"], e2=0.5, f1=coef(test.lm)["blog_instagram"],
                                                                                                                                g1=coef(test.lm)["tweets_instagram"]   ), nls.control(maxiter = 1000), algorithm = "port", 
             upper=c(Inf, Inf, 1,Inf, 1, Inf, 1, Inf, 1, Inf, Inf), lower=c(-Inf, -Inf, 0,-Inf, 0, -Inf, 0, -Inf, 0, -Inf, -Inf))
summary(fit_nls)

together_mod<-together
together_mod$email<-adstock(together_mod$email,unlist(coef(fit_nls))[3])
together_mod$tweets<-adstock(together_mod$tweets,unlist(coef(fit_nls))[5])
together_mod<-together_mod[,root_tweet:=(tweets)^(1/2)]
together_mod$instagram<-adstock(together_mod$instagram,unlist(coef(fit_nls))[7])
together_mod$blog<-adstock(together_mod$blog,unlist(coef(fit_nls))[9])
together_mod<-together_mod[,blog_instagram:=instagram*blog]
together_mod<-together_mod[,tweets_instagram:=instagram*tweets]
reg_adstock<-Arima(together_mod[,"search_index"], order=c(0,0,1), xreg=together_mod[,c("email", "root_tweet", "instagram", "blog", "blog_instagram", "tweets_instagram")])
summary(reg_adstock)

upper <- fitted(reg_adstock) + 1.96*sqrt(reg_adstock$sigma2)
lower <- fitted(reg_adstock) - 1.96*sqrt(reg_adstock$sigma2)

ggplot() + 
  geom_ribbon(aes(x=together$date,ymin=lower, ymax=upper), fill = "darkseagreen2")+
  geom_line(aes(x=together$date, y = together$search_index), color = "red", show.legend = TRUE) +
  geom_line(aes(x=together$date,y=as.numeric(reg_adstock$fitted)), color = "blue")+
  
  xlab('Date') +
  ylab('Google Search Index for homelessness')+
  ggtitle("Final Engagement Model with MA(1) Errors: Fitted vs Actual")

########### ALL OF THIS SECTION IS MAXIMIZATION - FINDING THE VALUES OF THE OUTREACH CHANNELS THAT MAXIMIZE ENGAGEMENT (SUBJECT TO BUDGET CONSTRAINT)
ad_rates<-coef(fit_nls)[seq(3,9, by=2)]
exo_values<-together_mod[nrow(together_mod),c("email", "tweets", "instagram", "blog")]*ad_rates

fr <- function(x) {      email <- x[1]
tweets <- x[2]
instagram <- x[3]
blog<- x[4]
return(-(reg_adstock$coef["intercept"] + reg_adstock$coef["email"]*(email +exo_values$email)+ reg_adstock$coef["root_tweet"]*(tweets+exo_values$tweets)^(1/2) + reg_adstock$coef["instagram"]*(instagram+exo_values$instagram) +reg_adstock$coef["blog"]*(blog+exo_values$blog)+ reg_adstock$coef["blog_instagram"]*(blog+exo_values$blog)*(instagram+exo_values$instagram)+reg_adstock$coef["tweets_instagram"]*(tweets+exo_values$tweets)*(instagram+exo_values$instagram)))
}


sol1.0<-constrOptim(c(1,1,1,1), fr, NULL, ui=rbind(c(-2,-100,-110,-120),
                                                   c(1,0,0,0),    
                                                   c(0,1,0,0),
                                                   c(0,0,1,0),
                                                   c(0,0,0,1)),  
                    ci=c(-5000,0, 0,0,0))


forecast_matrix<-exo_values+sol1.0$par
forecast_matrix<-forecast_matrix[, root_tweet:=(tweets)^(1/2)][,blog_instagram:=blog*instagram][, tweets_instagram:=tweets*instagram]
exo_values<-exo_values[, root_tweet:=(tweets)^(1/2)][,blog_instagram:=blog*instagram][, tweets_instagram:=tweets*instagram]

## final forecast using optimal values
forecast(reg_adstock, xreg=exo_values[,c("email", "root_tweet", "instagram", "blog", "blog_instagram", "tweets_instagram")])

#######################################

