library(rpart)
mcycle = read.csv(
"C:/Users/Pandu/Desktop/Pandu_ course/CASE_STUDY/Accl_Time/accltime.csv",header = TRUE)
mcycle
summary(mcycle)
structure(mcycle)
plot(Accel~Times,data=mcycle)
mct <- rpart(accel ~ times, data=mcycle)
Plot(mct)