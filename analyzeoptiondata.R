analyze<-function(data){

	data['diffbs'] = percentchange(data$actprice,data$bsprice)
	data['diffmc'] = percentchange(data$actprice,data$mcprice)
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

	par(mfrow=c(2,4))

	#mc prices against act prices
	plot(actprices, bsprices, type = 'p', col='red')
	abline(lm(bsprices~actprices), col = 'black')

	#mc prices against act prices
	plot(actprices, bsprices, type = 'p', col='green')
	abline(lm(mcprices~actprices), col = 'black')

	#bs/mc error boxplot
	boxplot(pdiffbs, pdiffmc, names=c('BS Error', 'MC Error'))

	#plotting mc error against bs error
	plot(pdiffbs, pdiffmc, xlab="BS Error", ylab="MC Error")

	#histogram of bs error
	hist(pdiffbs, breaks=30, main="Black-Scholes Errors")

	#histogram of mc error
	hist(pdiffmc, breaks=30, main="Monte Carlo Errors")

	#qq plot of bs error against normal dist
	qqnorm(pdiffbs); qqline(pdiffbs, col="red")

	#qq plot of mc error against normal dist
	qqnorm(pdiffmc); qqline(pdiffmc, col="green")

	#creating lm modeling ac = bs+mc

	model = lm(actprices~bsprices+mcprices)
	print(summary(model))

	#creating volatility surface
	surface = make_vs(remove_outliers_df(data, cols = 'timetoexpiry'))
	plot_vs_plotly(surface)
}

remove_outliers_df <- function(df, cols, multiplier = 10) {
  
	outlier_rows <- rep(FALSE, nrow(df))  # track which rows have outliers
  
	for (col in cols) {
		if (is.numeric(df[[col]])) {
      		Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
      		Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
      		IQR <- Q3 - Q1
      
      		lower_bound <- Q1 - multiplier * IQR
      		upper_bound <- Q3 + multiplier * IQR
      
      		outlier_rows <- outlier_rows | (df[[col]] < lower_bound | df[[col]] > upper_bound)
    		} 

		else {
      		warning(paste("Column", col, "is not numeric — skipped."))
    		}
  	}
  
	cleaned_df <- df[!outlier_rows, ]
	return(cleaned_df)
}

percentchange <- function(c1, c2){
	
	newc<-vector(mode = 'list', length = length(c1))

	for (i in 1:length(c1)){

		newc[i]<-(c2[i]/c1[i]-1)*100		

	}

	unlist(newc)
	
}

skewsmirk<- function(data){

	par(mfrow=c(1,2))

	data['moneyness'] = data$stockprice/data$strikeprice
	data = data[data$moneyness>0.8 & data$moneyness<1.2,]

	calls <- data[data$is_call == 1, ]
	puts  <- data[data$is_call == 0, ]	

	plot(calls$moneyness, calls$impliedvol, col = "blue", pch = 16, xlab = "Moneyness (S/K)", ylab = "Implied Volatility", main = "Volatility Smile / Skew Calls")
	#points(puts$moneyness, puts$impliedvol, col = "red", pch = 16)
	plot(puts$moneyness, puts$impliedvol, col = "red", pch = 16, xlab = "Moneyness (S/K)", ylab = "Implied Volatility", main = "Volatility Smile / Skew Puts")
	
	legend("topright", legend = c("Call","Put"),
       col = c("blue","red"), pch = 16)

}

make_vs <- function(df,
                             underlying_col = "stockprice",
                             strike_col = "strikeprice",
                             maturity_col = "timetoexpiry",
                             iv_col = "impliedvol",
                             moneyness_range = c(0.8, 1.2),   # grid moneyness range
                             maturity_range = NULL,           # default from data
                             moneyness_len = 50,
                             maturity_len = 50,
                             loess_span = 0.25) {

  # Basic checks
	df[maturity_col] = df[maturity_col]/365
	
 	required <- c(underlying_col, strike_col, maturity_col, iv_col)
 	if (!all(required %in% names(df))) stop("Dataframe must contain columns: ", paste(required, collapse=", "))

  # Compute moneyness S/K (use numeric underlying if vector or scalar)
 	S <- df[[underlying_col]]
 	K <- df[[strike_col]]
  # If S is single value repeated, still fine
 	moneyness <- as.numeric(S) / as.numeric(K)

 	df2 <- data.frame(
  		moneyness = moneyness,
  		maturity = as.numeric(df[[maturity_col]]),
    		iv = as.numeric(df[[iv_col]])
  	)

  # Remove rows with missing/invalid values
  	df2 <- df2[is.finite(df2$moneyness) & is.finite(df2$maturity) & is.finite(df2$iv), ]
  	if (nrow(df2) == 0) stop("No valid rows in data after cleaning.")

  # Optional: limit to a reasonable band to avoid illiquid extreme strikes
  	if (!is.null(moneyness_range)) {
    		df2 <- df2[df2$moneyness >= moneyness_range[1] & df2$moneyness <= moneyness_range[2], ]
  	}
  	if (nrow(df2) == 0) stop("No data in requested moneyness range.")

  # Define maturity range if not provided
  	if (is.null(maturity_range)) {
    		maturity_range <- range(df2$maturity, na.rm = TRUE)
  	}

  # Create prediction grid
  	m_grid <- seq(min(df2$moneyness), max(df2$moneyness), length.out = moneyness_len)
  	t_grid <- seq(min(df2$maturity), max(df2$maturity), length.out = maturity_len)
	grid <- expand.grid(moneyness = m_grid, maturity = t_grid)

  # Fit a LOESS surface: iv ~ moneyness + maturity
  # Use control to avoid errors on small data
	lo <- loess(iv ~ moneyness + maturity,
              data = df2,
              span = loess_span,
              control = loess.control(surface = "direct"))
  
  # Predictions
  	preds <- predict(lo, newdata = grid)
	
  
  # Clip negative or extremely small IVs
	preds[preds < 0] <- 0.001
  	#preds <- pmax(preds, min(df2$iv))
  	#preds <- pmin(preds, max(df2$iv))


  # If some NA in preds (extrapolation), we can fallback to nearest observed mean in vicinity
  	if (any(is.na(preds))) {
    # fill NA preds with nearest observed iv mean using simple approach
    		library_calc <- try(requireNamespace("FNN", quietly = TRUE), silent = TRUE)
    		if (library_calc) {
      # use k-nearest neighbor if FNN is available
      			library(FNN)
      			obs_xy <- as.matrix(df2[, c("moneyness", "maturity")])
      			grd_xy <- as.matrix(grid[, c("moneyness", "maturity")])
      			nn <- get.knnx(obs_xy, grd_xy[is.na(preds), , drop = FALSE], k = 3)
      			fill_idx <- which(is.na(preds))
      			for (i in seq_along(fill_idx)) {
        			idxs <- nn$nn.index[i, ]
        			preds[fill_idx[i]] <- mean(df2$iv[idxs], na.rm = TRUE)
      			}
    		} 
		else {
      # simple fallback: use loess predict with larger span
      			lo2 <- loess(iv ~ moneyness + maturity, data = df2, span = min(1, loess_span * 2))
      			preds[is.na(preds)] <- predict(lo2, newdata = grid[is.na(preds), ])
    		}
  	}

  # Reshape into matrix for plotting
  	zmat <- matrix(preds, nrow = moneyness_len, ncol = maturity_len, byrow = FALSE)
  	rownames(zmat) <- round(m_grid, 4)
  	colnames(zmat) <- round(t_grid, 4)

  	result <- list(
    		df = df2,
    		loess_model = lo,
    		moneyness = m_grid,
    		maturity = t_grid,
    		iv_matrix = zmat
  	)
  	class(result) <- "volsurf"
  	return(result)
}

# Basic base-R 3D surface plot using persp()
plot_vs_persp <- function(vs, theta = 30, phi = 25, col = "lightblue", ticktype = "detailed") {
 	m <- vs$moneyness
 	t <- vs$maturity
 	z <- vs$iv_matrix

  # persp expects z with rows = x, cols = y
 	persp(x = m, y = t, z = z,
    		xlab = "Moneyness (S/K)", ylab = "Maturity (years)", zlab = "Implied Volatility",
        	theta = theta, phi = phi, expand = 0.6, col = col, ticktype = ticktype, border = NA)
}

plot_vs_plotly <- function(vs) {
 	if (!requireNamespace("plotly", quietly = TRUE)) {
   		stop("plotly not installed. Install with install.packages('plotly') to use this function.")
  	}
  	library(plotly)
  	plot_ly(x = vs$moneyness, y = vs$maturity*365, z = vs$iv_matrix) %>%
    	add_surface() %>%
    	layout(scene = list(xaxis = list(title = "Moneyness (S/K)"),
             yaxis = list(title = "Maturity (days)"),
             zaxis = list(title = "Implied Volatility")))
}
#plot_vs_plotly(make_vs(d5put))
