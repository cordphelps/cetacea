
library(gridExtra)
library(ggplot2)



plot1 <- ggplot(data = nino) + geom_line(aes(y = pop3SantaCruz, x = Time), colour = "blue")  +
				geom_line(aes(y = ccSantaCruzREL, x = Time), colour = "red") +
				ylim(0,250)
plot2 <- ggplot(data = nino) + geom_line(aes(y = pop4SantaRosa, x = Time), colour = "blue") + 
				geom_line(aes(y = ccSantaRosaREL, x = Time), colour = "red") +
				ylim(0,250)
plot3 <- ggplot(data = nino) + geom_line(aes(y = pop5SanMiguel, x = Time), colour = "blue") + 
				geom_line(aes(y = ccSanMiguelREL, x = Time), colour = "red") +
				ylim(0,750)
plot4 <- ggplot(data = nino) + geom_line(aes(y = pop2Anacapa, x = Time), colour = "blue") + 
				geom_line(aes(y = ccAnacapaREL, x = Time), colour = "red") +
				ylim(0,250)
grid.arrange(plot1, plot2, plot3, plot4)


# weather effects

ggplot(data = nino) + geom_line(aes(y = ccChannelABSOLUTE, x = Time), colour = "blue")  +
				geom_line(aes(y = ccChannelABSOLUTE, x = Time), colour = "blue") +
				ylim(0,800)

# create a 'time series' object in R
timeSeries.ts <- ts(nino$ccChannelABSOLUTE, frequency=1)
plot(timeSeries.ts)

# is there a tendancy for the cc than lags by one year to be the same?
acf(timeSeries.ts)$acf[2]  # 0.2413305 = correlation coefficient on the first lag
acf(timeSeries.ts, type=c("covariance"))$acf[2] # 2368.71

lagged.df <- data.frame(timeSeries.ts[1:99], timeSeries.ts[2:100])

plot5 <- ggplot(data = nino) + geom_line(aes(y = ccChannelABSOLUTE, x = Time), colour = "blue") + 
				ylim(0,800)

plot6 <- ggplot(lagged.df, aes(timeSeries.ts.1.99., timeSeries.ts.2.100., color = timeSeries.ts.2.100.)) +
				geom_point(shape = 19, size = 3, show.legend = FALSE) +
				ylim(0,800)

grid.arrange(plot5, plot6, ncol = 2, nrow = 1)

# extract the spectrum
spectrum(lagged.df$timeSeries.ts.1.99., log = c("no"))
spectrum(lagged.df$timeSeries.ts.1.99., span=15, log = c("no")) # smoothing with a moving average of 15 adjacent spikes

spectrum(lagged.df$timeSeries.ts.2.100., span=15, log = c("no"))

spectrum(nino$pop2Anacapa, span=15, log = c("no"), main="Anacapa")
spectrum(nino$pop5SanMiguel, span=15, log = c("no"), main="San Miguel")
spectrum(nino$pop3SantaCruz, span=15, log = c("no"), main="Santa Cruz")
spectrum(nino$pop4SantaRosa, span=15, log = c("no"), main="Santa Rosa")




