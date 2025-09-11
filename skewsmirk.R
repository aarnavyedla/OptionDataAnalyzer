args = commandArgs(trailingOnly = TRUE)

input = args[1]
p1out = args[2]
p2out = args[3]
print(input)
print(p1out)
print(p2out)
data = read.csv(input)

data['moneyness'] = data$stockprice/data$strikeprice
data = data[data$moneyness>0.8 & data$moneyness<1.2,]

calls <- data[data$is_call == 1, ]
puts  <- data[data$is_call == 0, ]

png(p1out, width = 800, height = 600)
plot(calls$moneyness, calls$impliedvol, col = "blue", pch = 16, xlab = "Moneyness (S/K)", ylab = "Implied Volatility", main = "Volatility Smile / Skew Calls")
dev.off()

png(p2out, width = 800, height = 600)
plot(puts$moneyness, puts$impliedvol, col = "red", pch = 16, xlab = "Moneyness (S/K)", ylab = "Implied Volatility", main = "Volatility Smile / Skew Puts")
dev.off()

#legend("topright", legend = c("Call","Put"),
   #col = c("blue","red"), pch = 16)