
count_na <- function(x) {sum(!is.na(x))}
count_na(ETSStufen_1000_clean$`2015`)

table <- apply(ETSStufen_1000_clean, 2, count_na)
death_count <- apply(Tote_1000, 2 ,sum)
death_count <- death_count/2
death_count <- c(0, death_count)
death_count
table <- as.data.frame(table)
table$death <- death_count
colnames(table) <- c("total", "death")
table$ratio <- table$death / table$total
table$year <- rownames(table)

library(ggplot2)

ggplot(data = table) +
geom_col(aes(x = year, y = total))


#-------------------------------------------------------------------------------
mort_status$observ_time

table(mort_status$status)
table(mort_status$observ_time)


table <- as.data.frame(c(2013:2022))
colnames(table) <- "year"
table$death <- 0
table$total <- 0
for (i in 10:2) {
	table$death[i-1] <- sum(mort_status$status[mort_status$observ_time == i])
	table$total[i-1] <- length(mort_status$status[mort_status$observ_time == i]) +
		table$total[i]
}


table$ratio <- table$death / table$total
table
death_count <- apply(Tote_1000, 2 ,sum)
table$entnommen <- c(death_count, 0, 0)

ggplot(data = table[1:9,]) +
	geom_col(aes(x = year, y = total), fill = "gray") +
	geom_col(aes(x = year, y = entnommen), fill = "black") +
	geom_col(aes(x = year, y = death), fill = "red") +
	scale_x_continuous(breaks = c(2013:2021))


