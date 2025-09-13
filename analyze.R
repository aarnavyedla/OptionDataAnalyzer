remove_outliers_df = function(df, cols, multiplier = 1.5) {

	outlier_rows = rep(FALSE, nrow(df))

	for (col in cols) {
		if (is.numeric(df[[col]])) {
      		Q1 = quantile(df[[col]], 0.25, na.rm = TRUE)
      		Q3 = quantile(df[[col]], 0.75, na.rm = TRUE)
      		IQR = Q3 - Q1

      		lower_bound = Q1 - multiplier * IQR
      		upper_bound = Q3 + multiplier * IQR

      		outlier_rows = outlier_rows | (df[[col]] < lower_bound | df[[col]] > upper_bound)
    		}

		else {
      		warning(paste('Column', col, 'is not numeric — skipped.'))
    		}
  	}

	cleaned_df = df[!outlier_rows, ]
	return(cleaned_df)
}

args = commandArgs(trailingOnly=TRUE)

input = args[1]
p1out = args[2]
p2out = args[3]
p3out = args[4]
p4out = args[5]
p5out = args[6]
p6out = args[7]
p7out = args[8]
p8out = args[9]

data = read.csv(input)

data['diffbs'] = (data$bsprice/data$actprice-1)*100
data['diffmc'] = (data$mcprice/data$actprice-1)*100
data['moneyness'] = data$stockprice/data$strikeprice

data = remove_outliers_df(data, cols = c('actprice', 'bsprice', 'mcprice'))
data = remove_outliers_df(data, cols = c('diffbs', 'diffmc'), multiplier = 3.5)

time = data$timetoexpiry
actprices = data$actprice
bsprices = data$bsprice
mcprices = data$mcprice
iv = data$impliedvol
stock = data$stockprice
strike = data$strikeprice
pdiffbs = data$diffbs
pdiffmc = data$diffmc

#bs prices against act prices
png(p1out, width = 800, height = 600)
plot(actprices, bsprices, type = 'p', col='red', main = 'BS Prices vs. Actual Prices')
abline(lm(bsprices~actprices), col = 'black')
invisible(dev.off())

#mc prices against act prices
png(p2out, width = 800, height = 600)
plot(actprices, bsprices, type = 'p', col='green', main = 'BS Prices vs. Actual Prices')
abline(lm(mcprices~actprices), col = 'black')
invisible(dev.off())

#bs/mc error boxplot
png(p3out, width = 800, height = 600)
boxplot(pdiffbs, pdiffmc, names=c('BS Error', 'MC Error'), main = 'Percent Error from Actual Prices')
invisible(dev.off())

#plotting mc error against bs error
png(p4out, width = 800, height = 600)
plot(pdiffbs, pdiffmc, xlab='BS Error', ylab='MC Error', main = 'MC Error vs. BS Error')
invisible(dev.off())

#histogram of bs error
png(p5out, width = 800, height = 600)
hist(pdiffbs, breaks=30, main='Black-Scholes Errors')
invisible(dev.off())

#histogram of mc error
png(p6out, width = 800, height = 600)
hist(pdiffmc, breaks=30, main='Monte Carlo Errors')
invisible(dev.off())

#qq plot of bs error against normal dist
png(p7out, width = 800, height = 600)
qqnorm(pdiffbs, main = 'BS Error Normal QQ Plot', ylab = 'BS Quantiles'); qqline(pdiffbs, col='red')
invisible(dev.off())

#qq plot of mc error against normal dist
png(p8out, width = 800, height = 600)
qqnorm(pdiffmc, main = 'MC Error Normal QQ Plot', ylab = 'MC Quantiles'); qqline(pdiffmc, col='green')
invisible(dev.off())

#creating lm modeling ac = bs+mc

model = lm(actprices~bsprices+mcprices)
summary_text = capture.output(summary(model)$coefficients, summary(model)$r.squared)

cat('Linear Model Estimating Actprices using BSprices and MC prices Coefficients: \n')
print(summary(model)$coefficients)


cat('R-squared: ')
cat(summary(model)$r.squared)
