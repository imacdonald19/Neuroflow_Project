mood = read.csv("mood.csv")
stress = read.csv("stress.csv")
rumin = read.csv("ruminatory_stress.csv")
sleep =  read.csv("sleep.csv")

#Mood
mood_mean=mean(mood$value)
mood_var=var(mood$value)
mood_sd = sqrt(mood_var)
mood_num = c(1:length(mood$value))

plot(mood_num, mood$value, xlab="Observation Number", 
     ylab = "Mood Value", main = "Mood over Time", ylim=c(1,5),
     type="o", lty=1)
mood_lm = lm(mood$value ~ mood_num)
abline(mood_lm, col="blue", lty=2)
summary(mood_lm)

mood_ma = filter(mood$value, rep(1/5, 5), sides=1)
plot(mood_num, mood$value, xlab="Observation Number", 
     ylab = "Mood Value", main = "Mood over Time", ylim=c(1,5), type='o')
lines(mood_num, mood_ma, col="red", lty=2)

#Stress
stress_mean=mean(stress$value)
stress_var=var(stress$value)
stress_sd = sqrt(stress_var)
stress_num = c(1:length(stress$value))
stress_numsq = stress_num^2

plot(stress_num, stress$value, xlab="Observation Number", 
     ylab = "Stress Value", main = "Stress over Time", ylim=c(1,5),
     type="o", lty=1)
stress_lm = lm(stress$value ~ stress_num)
abline(stress_lm, col="blue", lty=2)
summary(stress_lm)

plot(stress_num, stress$value, xlab="Observation Number", 
     ylab = "Stress Value", main = "Stress over Time", ylim=c(1,5),
     type="o", lty=1)
stress_lm2 = lm(stress$value ~ stress_num + stress_numsq)
lines(stress_num, fitted(stress_lm2), col="darkgreen", lty=2)
summary(stress_lm2)

plot(stress_num, stress$value, xlab="Observation Number", 
     ylab = "Stress Value", main = "Stress over Time", ylim=c(1,5),
     type="o", lty=1)
stress_ma = filter(stress$value, rep(1/5, 5), sides=1)
lines(stress_num, stress_ma, col="red", lty=2)


#Ruminatory Stress
rumin_mean=mean(rumin$value)
rumin_var=var(rumin$value)
rumin_sd = sqrt(rumin_var)
rumin_num = c(1:length(rumin$value))

plot(rumin_num, rumin$value, xlab="Observation Number", 
     ylab = "Ruminatory Stress Value", main = "Ruminatory Stress over Time",
     ylim=c(1,5), type="o", lty=1)
rumin_lm = lm(rumin$value ~ rumin_num)
abline(rumin_lm, col="blue", lty=2)
summary(rumin_lm)

plot(rumin_num, rumin$value, xlab="Observation Number", 
     ylab = "Ruminatory Stress Value", main = "Ruminatory Stress over Time",
     ylim=c(1,5), type="o", lty=1)
rumin_ma =filter(rumin$value, rep(1/5, 5), sides=1)
lines(rumin_num, rumin_ma, col="red", lty=2)


#Sleep
sleep_mean=mean(sleep$value)
sleep_var=var(sleep$value)
sleep_sd = sqrt(sleep_var)
sleep_num = c(1:length(sleep$value))

plot(sleep_num, sleep$value, xlab="Observation Number", 
     ylab = "Sleep Value", main = "Sleep over Time", ylim=c(1,5), 
     type="o", lty = 1)
sleep_lm = lm(sleep$value ~ sleep_num)
abline(sleep_lm, col="blue", lty=2)

plot(sleep_num, sleep$value, xlab="Observation Number", 
     ylab = "Sleep Value", main = "Sleep over Time", ylim=c(1,5), 
     type="o", lty = 1)
sleep_ma = filter(sleep$value, rep(1/5, 5), sides=1)
lines(sleep_num, sleep_ma, col ="red", lty=2)

