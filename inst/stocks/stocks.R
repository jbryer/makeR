require(quantmod)
require(ggplot2)
getSymbols(stocks,src=src)
results = data.frame(Open=numeric(), High=numeric(), Low=numeric(), 
					 Close=numeric(), Volume=numeric(), Adjusted=numeric(), Symbol=character())
for(s in stocks) {
	df = as.data.frame(get(s)[month])
	names(df) = c('Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted')
	df$Symbol = s
	results = rbind(results, df)
}
results$day = as.Date(row.names(results))
ggplot(results, aes(x=day, y=Close, colour=Symbol)) + geom_line()
ggsave(paste(month, '.png', sep=''))
