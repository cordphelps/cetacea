cetacea
================

``` r
library(gridExtra)
library(ggplot2)
```

``` r
library(RCurl)
```

    ## Loading required package: bitops

``` r
source.url <- c("https://raw.githubusercontent.com/cordphelps/cetacea/master/V81.csv")
nino <- read.csv(source.url, header=TRUE, row.names=NULL)
```

``` r
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
```

![](cetacea_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
timeSeries.ts <- ts(nino$ccChannelABSOLUTE, frequency=1)
lagged.df <- data.frame(timeSeries.ts[1:99], timeSeries.ts[2:100])

plot5 <- ggplot(data = nino) + geom_line(aes(y = ccChannelABSOLUTE, x = Time), colour = "blue") + 
                ylim(0,800)

plot6 <- ggplot(lagged.df, aes(timeSeries.ts.1.99., timeSeries.ts.2.100., color = timeSeries.ts.2.100.)) +
                geom_point(pch = 1, size = 3, show.legend = FALSE, position = position_jitterdodge(dodge.width = 0.75, jitter.height = 0.75) ) +
                ylim(0,800)

grid.arrange(plot5, plot6, ncol = 2, nrow = 1)
```

![](cetacea_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
# is there a tendency for the cc that lags by one year to be the same?
# Introductory Time Series with R page 34
acf(timeSeries.ts, type=c("covariance"))$acf[2]
```

![](cetacea_files/figure-markdown_github/unnamed-chunk-5-1.png)

    ## [1] 3397.627

``` r
spectrum(nino$pop2Anacapa, span=15, log = c("no"), main="Anacapa")
```

![](cetacea_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
spectrum(nino$pop5SanMiguel, span=15, log = c("no"), main="San Miguel")
```

![](cetacea_files/figure-markdown_github/unnamed-chunk-6-2.png)

``` r
spectrum(nino$pop3SantaCruz, span=15, log = c("no"), main="Santa Cruz")
```

![](cetacea_files/figure-markdown_github/unnamed-chunk-6-3.png)

``` r
spectrum(nino$pop4SantaRosa, span=15, log = c("no"), main="Santa Rosa")
```

![](cetacea_files/figure-markdown_github/unnamed-chunk-6-4.png)
