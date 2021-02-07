PROBLEMA CAPITULO 3 PROBLEMA 12

install.packages(c("vctrs","maxLik","miscTools","car","carData"))
CONTROL=c(213,214,204,208,212,200,207)
T2=c(76,85,74,78,82,75,82)
T3=c(57,67,55,64,61,63,63)
T4=c(84,82,85,92,87,79,90)
df=data.frame(CONTROL=CONTROL,T2=T2,T3=T3,T4=T4)
df
df=data.frame(CONTROL=CONTROL,T2=T2,T3=T3,T4=T4)
df=stack(df)
df
names(df)=c("Y","Trat")
df

boxplot(Y~Trat,data=df)
modelo=aov(Y~Trat,data=df)
summary(modelo)
tk=TukeyHSD(modelo)
tk
plot(tk)
1-pf(1.559,3,24)
qqnorm(modelo$residuals)
qqline(modelo$residuals)
shapiro.test(modelo$residuals)


library("car")
leveneTest(Y~Trat,data=df)
plot(modelo$residuals)
plot(modelo$fitted.values,modelo$residuals)
abline(h=0)
