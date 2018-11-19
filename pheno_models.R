

library(mgcv)
library(tidyverse)
library(dplyr)

data <- data.frame(doy = 1:120,
                   evi = 0 + (1 / (1 + exp(- 0.15 * (1:120 - 80)))) + rnorm(120, 0, 0.1))


ggplot(data, aes(x = doy, y = evi)) +
  geom_point()

# Double-logistic



fit <- nls(evi ~ b1 + (b2 / (1 + exp(- b3 * (doy - b4)))),
           start = list(b1 = min(data$evi), 
                        b2 = max(data$evi), 
                        b3 = 0.15, 
                        b4 = round(mean(data[which(data$evi > median(data$evi)), "doy"]), 0)),
           data = data)

round(coef(fit), 2)

data$predict <- predict(fit)

ggplot(data, aes(x = doy, y = evi)) +
  geom_point() +
  geom_line(aes(x = doy, y = predict))


# Splines

fit <- gam(evi ~ s(doy), data = data)

data$predict <- predict(fit)

summary(fit)


ggplot(data, aes(x = doy, y = evi)) +
  geom_point() +
  geom_line(aes(x = doy, y = predict))

gam.check(fit)

ggplot(data, aes(doy, evi)) + geom_point() + geom_smooth(method = "gam", formula = y ~s(x))

###### PLOT BASIS FUNCTIONS 

plot.gam(fit)
model_matrix <- predict(fit, type = "lpmatrix")
plot(evi ~ doy, data=data)
abline(h = 0)

# plot two 
lines(data$doy, model_matrix[, "s(doy).1"], type = "l", lty = 2)
lines(data$doy, model_matrix[, "s(doy).2"], type = "l", lty = 2)

# plot all 
plot(evi ~ doy, data=data)
abline(h = 0)
x_new <- seq(0, max(data$doy), length.out = 100)
y_pred <- predict(fit, data.frame(doy = x_new)) 
matplot(data$doy, model_matrix[,-1], type = "l", lty = 2, add = T)
lines(y_pred ~ x_new, col = "red", lwd = 2)

# Check: https://stackoverflow.com/questions/14207250/determining-derivatives-from-gam-smooth-object

newDF <- with(data, data.frame(doy = unique(doy)))
B <- predict(fit,  newDF, type = "response", se.fit = TRUE)

eps <- 1e-7
X0 <- predict(fit, newDF, type = 'lpmatrix')

newDFeps_p <- newDF + eps

X1 <- predict(fit, newDFeps_p, type = 'lpmatrix')

Xp <- (X0 - X1) / eps

fd_d1 <- Xp %*% coef(fit)

ggplot(data) +
  geom_point(aes(x = doy, y = evi)) +
  geom_line(aes(x = doy, y = predict)) +
  geom_line(data = data.frame(doy = 1:120, deriv1 = fd_d1),
            aes(x = doy, y = deriv1 * 10), col = "red")

which.min(fd_d1)# + base value = doy 


fd_d1[which.min(fd_d1[,2]),1]

