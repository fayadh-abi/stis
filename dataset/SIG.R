library(usdm)
library(lmtest)
library(rio)
data.sig <- read.csv("sig.csv")
data.sig
model <- lm(log(wisatawan)~Inflasi+log(kamar.hotel)+rm+objek,data = data.sig)
summary(model)
shapiro.test(model$residuals) # asumsi normaltas residual
bptest(model) # uji heteroskedastisitas
vifcor(data.sig[,3:6]) # asumsi multikolinearitas
car::durbinWatsonTest(model) # uji durbin watson autokorelasi
