library(fpp3)

setwd("~/Desktop/Forecasting R studio")

#Women unemployment -- SARIMA model approach
#a.	Provide a brief overview of your series and specifically discuss 
#why one would wish to forecast it. For example, please discuss what
#type of decision(s) might be made as a result of the forecasting outcome.

#Ans: Unemployment rate in women,monthly data,units is percentage,non-seasonally 
#adjustments(THEORY)
#There  are many students who are graduating in may ,looking for jobs
#which add to labour market as unemployed while they are looking for job.



#b.There will be a holdout period that is used to compare actual data to 
#forecasted values. Please restrict your sample by excluding the last four 
#observations. After restricting your sample, if the data are transformed 
#in any way prior to testing (e.g. a Box-Cox transformation), please 
#discuss why.

temp=readr::read_csv("Final Data.csv")
unemployment=temp%>%mutate(DATE=yearmonth(mdy(DATE)))%>%
  as_tsibble(index=DATE)
unemploymentHOLD=unemployment%>%filter_index(~ "2022 June")


#Original Plot
unemploymentHOLD%>%autoplot(Unemployment)+ylab("Women Unemployment Rate")+ggtitle("Original Data Plot")

#checking for Transformations
unemploymentHOLD%>%autoplot(log(Unemployment))+ggtitle("Logarithmic Transformations")+ylab("Women Unemployment Rate")

#Box-Cox Transformations
lambdaWN=unemploymentHOLD%>%features(Unemployment,guerrero)%>%
  pull(lambda_guerrero)
lambdaWN
unemploymentHOLD%>%autoplot(box_cox(Unemployment,lambdaWN))+ylab("Women Unemployment Rate")+ggtitle("Box-Cox Transformations")

#Explaination:

# We are not taking any transformations because both box-cox and log doesn't 
#have much impact on the volatility and we do not anticipate any non-linear 
#growth in the future women unemployment rates.


#c. Discuss whether or not your data appear to be generated from a 
#covariance stationary model. If your forecasting method requires a
#transformation is necessary, please discuss which transformation you 
#used and why. A unit root test will be necessary for this part if you use 
#SARIMA-based methods or vector autoregressions.

# Check for stationarity
unemploymentHOLD%>%features((Unemployment),unitroot_ndiffs)
unemploymentHOLD%>%features((Unemployment),unitroot_nsdiffs)
#Explaination: 

#The uniroot test determines stationarity in a series and indicates if
#differencing is necessary or not. After using features function, we can observe
#that the uniroot_ndiffs value is 1 i.e there is a need of
#non seasonal differencing, so we need to set d=1.
#The uniroot_nsdiffs value used for determining the need of
#seasonal differencing is 0, which means no seasonal differencing is required 
#and we can set D=0.


#d.Again, please do not choose a seasonally adjusted series if you can 
#avoid it. Describe whether the following elements appear present in   
#your data:
# i. Seasonality
#ii. Deterministic trends
#iii. Other variation (anything other than trend/seasonality)

#STL decomposition:

unemploymentHOLD%>%model(STL(Unemployment~season(window="periodic")+trend(window=21)))%>%
  components()%>%autoplot()
#Explaination:

#From STL decomposition,we can observe that there is no proper trend in our 
#dataset.As there are many peaks which rise or fall.


unemploymentHOLD%>%gg_subseries(Unemployment)

#There  are many students who are graduating in may ,looking for jobs
#which add to labour market as unemployed while they are looking for job.
#Women not getting proper appraisal as compared to men---reasons are they leave
#job and add to the unemployment pool.

 
#Explaination:
#Part left(by sarthak and bhargav ram)



#e.For this part, select any forecasting method, except those related to 
#sample means, naïve methods, and exponential smoothing. As
#examples, you can select SARIMA methods, vector autoregressions, 
#multivariate regression models, neural nets, etc.
#Amongst the class of models you selected, start by validating a single 
#model. For example, for a VAR, discuss selection of lags, whether     
#certain variables were differenced, why you selected the series you 
#chose, etc. For SARIMA methods, we will have carefully used Box-
#  Jenkins, which would entail the following

#i.	Provide a correlogram plot of the appropriate series (ACF/PACF). Describe 
#in detail which models you feel are appropriate for the transformed data.
#Estimate several models. Record the SIC/AIC values.
unemploymentHOLD%>%gg_tsdisplay(difference(Unemployment),lag_max = 72,
                                plot_type = "partial")


#From ACF Plot, we see that every 12th lag is statistically significant.
# We also observe that many coefficients at non-seasonal lags 
#(lags 2 to 7,lags 15,17 etc) are statistically significant,with a grouping of 
#coefficients that are both positive and negative.
#From PACF Plot, a similar pattern is observed relative to ACF Plot, where 
#initial sample PACF coefficients tend to be even larger. 
#At seasonal lags, only the coefficients at lag 12 to 48 are 
#statistically significant.A variety of models seem possible. 
#We are considering P=4 and Q=0 as the true PACF at every 12th lag after 
#lag 48  should be zero.
#At non-seasonal lags, a stronger case could be made for a autoregressive
#component, where the true ACF would be non-zero at all lags. 
#a reasonably large number of sample PACF coefficients seemed statistically 
#important.we are considering p=10 and q=0,as we can see non-seasonal lags at 
#lag 10 is statistically significant.

#guess model

models=unemploymentHOLD%>%model(ARIMA((Unemployment)~0+pdq(10,1,0)+PDQ(4,0,0)))
report(models)
#models%>%gg_tsresiduals(lag_max=72)
#Output:AIC=1113.19   AICc=1113.96   BIC=1180.13

models=unemploymentHOLD%>%model(m0=ARIMA((Unemployment)~0+pdq(10,1,0)+PDQ(4,0,0)),
      m1=ARIMA((Unemployment)~0+pdq(6,1,1)+PDQ(2,0,1)),
      m2=ARIMA((Unemployment)~0+pdq(6,1,2)+PDQ(1,0,1)),
      m3=ARIMA((Unemployment)~0+pdq(4,1,1)+PDQ(2,0,1)),
      m4=ARIMA((Unemployment)~0+pdq(2,1,2)+PDQ(1,0,1)),
      m5=ARIMA((Unemployment)~0+pdq(2,1,1)+PDQ(2,0,1)))

glance(models)%>%select(.model,AIC,BIC)%>%as.data.frame()

#ii.	Perform diagnostic checks on your residuals. Provide a plot of the 
#correlogram of the residuals and describe why the fitted model is appropriate.


#Final model : 
FINALmodels=unemploymentHOLD%>%model(ARIMA((Unemployment)~0+pdq(2,1,2)+PDQ(1,0,1)))
report(FINALmodels)
FINALmodels%>%gg_tsresiduals(lag_max=72)
#output:AIC=1104.497,BIC= 1135.738

#f.For your model from part (e), conduct your forecast and produce a 
#graph that contains the out of sample forecast for your chosen model  
#with the 95% confidence intervals about your forecast, along with the 
#actual observations of your series (which, as before, should contain a 
#few pre-forecast period observations). At this stage, please discuss how 
#the forecast would affect decision making and whether or not decisions 
#were correctly made on the basis of the eventual realizations of your 
#data.
augment(FINALmodels)%>%features(.innov,ljung_box,lag=48,dof=6)
qchisq(0.95,42)
#The ljung-box test resulted in a p value of 0.999 which is very high and 
#ljung-box statistic is 19.4 which is less than 95% critical value for a 
#chisquare(42) distribution.Hence, we fail to reject null hypothesis 
#(19.4<58.12404)and comprehend that there is no evidence of residual correlation.

FINALmodels%>%forecast(h=4)%>%
  autoplot(unemployment%>%filter_index("2020 Oct"~"2022 Oct"),level=95)


FINALmodels%>%forecast(h=4)%>%
  as.data.frame()%>%select(DATE,.mean)

#Explaination:As per our forecast,The plot we obtain is very close to the original
#data points and therefore,we can conclude that the model we chose is accurate 
#enough.

#g. Please select a second model using a technique other than what was 
#employed in parts e-f. Please estimate your new model and use it to 
#forecast for the four time periods originally excluded from your sample. 
#Compare the accuracy of the alternative method to your first selected 
#model. Choose one of the two models to proceed to part (h).
## Second model-ETS Methods

# Fit a simple exponential smoothing model
fit=unemploymentHOLD%>%model(ETS(Unemployment~error("M")+trend("N")+season("A")))
report(fit)

#As there is no trend in our sample we will put trend as None and our seasonality
#is constant over the period of time.Therefore,It is additive seasonality.
#Unemployment rate cannot be negative so we will choose multiplicative error.


# Forecasting with simple exponential smoothing

foreETS=fit%>%forecast(h=4)
foreETS$.mean
foreETS%>%autoplot(unemployment%>%filter_index("2020 Oct"~"2022 Oct"),level=95)


# Evaluating forecasting accuracy:

forecastsFINAL1=forecast(fit,h=4)
accuracy(forecastsFINAL1,unemployment)%>%select(.model,RMSE)%>%as.data.frame()

#Output:.model                                               RMSE
#ETS(Unemployment ~ error("M") + trend("N") + season("A")) 0.2582601

forecastsFINAL2=forecast(FINALmodels,h=4)
accuracy(forecastsFINAL2,unemployment)%>%select(.model,RMSE)%>%as.data.frame()

#Output:                                                  .model      RMSE
# ARIMA((Unemployment) ~ 0 + pdq(2, 1, 2) + PDQ(1, 0, 1))         0.2107771


#h.In the last stage, please re-estimate your selected model for the entire 
#sample. Please provide a new 6-step ahead forecast for your series. Please 
#comment on how the forecast would affect interested parties.

#We will choose ARIMA model over ETSmodel because of lower RMSE value (0.210 
#as compared to 0.2582).As we can say that lower the RMSE value better the model.



FINALmodels%>%forecast(h=6)%>%
  autoplot(unemployment%>%filter_index("2020 Jan"~"2022 Oct"),level=95)


FINALmodels%>%forecast(h=6)%>%
  as.data.frame()%>%select(DATE,.mean)










