###########################Aging Init###########################################################
aging.props <- list(inc = params$aging_inc)

death_rates <- read.table("death.csv", header = TRUE)
colnames(death_rates) <- c("age","male","female")
death_rates <- add_row(death_rates, age = 100, male = 1, female = 1)

death.props <- list(f.rates = data.frame(age = death_rates$age, p = death_rates$female),
                    m.rates = data.frame(age = death_rates$age, p = death_rates$male),
                    max.age = 100)

age_table <- read.table("Age_Dist_Saarland_2017.csv", sep = ";")
age_table <- age_table[-1]
age_table[1] <- seq(from = 0, by = 1, length.out = nrow(age_table))
colnames(age_table) <- c("Age", "N")
# n people at age 89
n_89 <- age_table[90,2]
avg_death_p <- rowMeans(death_rates[,2:3]) # 0 - 100

plus_90_list <- c(n_89)
for (i in 90:100) {
  left <- tail(plus_90_list, n = 1)
  plus_90_list = c(plus_90_list, left - avg_death_p[i + 1] * left)
}
# remove age 89
plus_90_list <- plus_90_list[-1]

# remove last row (90+)
age_table <- age_table[-nrow(age_table),]
new_age_data <- data.frame(seq(90,100), plus_90_list)
colnames(new_age_data) <- colnames(age_table)
age_table <- rbind(age_table, new_age_data)
# smooth the distribution
#age_table$N <- sgolayfilt(age_table$N, n=9)
# add probabilities
age_table <- cbind(age_table, p = age_table$N/sum(age_table$N))
#age_table[c(1:12),]$p = 0

sample_pop <- function(n, prob) {
  k <- length(prob)
  q <- prob / rev(cumsum(rev(prob)))
  u <- matrix(runif(n*k), nrow = k, byrow = TRUE) < q
  apply(u, 2, function(x) match(TRUE, x)) - 1
}

init_age_dist <- sample_pop(params$initial_pop, age_table$p)

orange <- rgb(217,123,0, maxColorValue = 255)
cols = c("black", orange)
fac <- length(init_age_dist)*age_table$N/sum(age_table$N)

hist(init_age_dist, breaks = 100, include.lowest = TRUE, right = FALSE, 
     col = cols[2], plot = TRUE, xlab = "Age", ylab = "Count", main = "", xlim = c(0,100))
lines(0:100, fac, lwd = 2, lty = 1, col = cols[1], type = "s")
legend("topright", legend = c("Destatis", "Model sample"), col = cols, lty = c(1,1), bty = "n", 
       lwd = 2)