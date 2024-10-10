# Type "#" at start of line for comments, the line will not be treated as code
#Basic functions and operations
(1+1)/8

4*9^2

1 > 4

print("Hello World")

log10(10000)

sqrt(81)

log(exp(2))

child <- 1
child
animals <- c("cat", "dog", "rat", "cow")
animals

#Central tendency and visualization
africa_short <- read.csv("C:/Users/nguye/OneDrive - Hitotsubashi University/africa_short.csv")
africa_short <- read.csv("https://raw.githubusercontent.com/hknd23/QuantIR/refs/heads/main/Data/africa_short.csv")


summary(africa_short)
head(africa_short)

boxplot(africa_short$PopulationDens, main = "Population Density", ylab = "Density")
boxplot(africa_short$WomenInParl, main = "Women in Parliament", ylab = "per cent")


boxplot(africa_short$WomenInParl~africa_short$Conflict, main = "Women in Parliament", xlab = "Conflict Onset", ylab = "per cent")


mean(africa_short$WomenInParl)

median(africa_short$HIV, na.rm = TRUE)

polity_scores <- as.factor(africa_short$POLITYDemocracy)

summary(polity_scores)


hist(africa_short$POLITYDemocracy, main = "Polity Scores for 15 African Countries",xlim = c(-10,10), ylim = c(0,4),xlab = "Polity", ylab = "Count")


plot(africa_short$HIV~africa_short$WomenInParl, main = "Women in Parliament and HIV", xlab = "Women in Prlment", ylab = "HIV Rate")


plot(africa_short$HIV~africa_short$WomenInParl, main = "Women in Parliament and HIV", xlab = "Women in Prlment", ylab = "HIV Rate", col = factor(africa_short$Conflict),  pch = 19)
legend("topleft",
       legend = levels(factor(africa_short$Conflict)),
       pch = 19,
       col = factor(africa_short$Conflict))
barplot(table(africa_short$Conflict), main = "Conflict 15 African Countries",xlab = "Onset", ylab = "Count", col = c("steelblue1","red2"))