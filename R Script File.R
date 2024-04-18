
library(dplyr)
library(pwr)

## Data Preparation

data <- read.csv("IT Salary Survey EU  2020.csv")

data <- data[,c("Company.size",
                "Yearly.brutto.salary..without.bonus.and.stocks..in.EUR",
                "Total.years.of.experience",
                "Age",
                "Gender")]
colnames(data) <- c("Company Size", 
                    "Annual Salary", 
                    "Experience (Yrs)", 
                    "Age",
                    "Gender")
head(data)

data <- na.omit(data)
data$`Experience (Yrs)` <- as.numeric(data$`Experience (Yrs)`)
data <- data %>%
  filter(`Company Size` != "")
unique(data$`Company Size`)

#GPower
 pwr.anova.test(k = 5 , 
                f = 0.1 , 
                sig.level = 0.05 , 
                power = 0.80 )

summary(data)

Q1 <- quantile(data$`Annual Salary`, .25)
Q3 <- quantile(data$`Annual Salary`, .75)
IQR <- IQR(data$`Annual Salary`)
no_outliers <- subset(data, 
                      data$`Annual Salary` > (Q1 - 1.5*IQR) & 
                        data$`Annual Salary` < (Q3 + 1.5*IQR))

Q1 <- quantile(data$`Experience (Yrs)`, .25, na.rm = T)
Q3 <- quantile(data$`Experience (Yrs)`, .75, na.rm = T)
IQR <- IQR(data$`Experience (Yrs)`, na.rm = T)
no_outliers <- subset(no_outliers, 
                      data$`Experience (Yrs)` > (Q1 - 1.5*IQR) & 
                        data$`Experience (Yrs)` < (Q3 + 1.5*IQR))
dim(no_outliers)

boxplot(no_outliers$`Annual Salary`, 
        main = "Boxplot for Annual Salary", 
        col = "steel blue",
        xlab = "Annual Salary",
        ylab = "Value")

#Scatter plot between age and salary
plot(no_outliers$Age, 
     no_outliers$`Annual Salary`, 
     main = "Age vs Annual Salary", 
     xlab = "Age", 
     ylab = "Annual Salary",
     pch = 17)

### QQ-Plots:

qqnorm(no_outliers$`Annual Salary`, pch = 1, frame = FALSE)
qqline(no_outliers$`Annual Salary`, col = "red", lwd = 2)

## Bartletts's Test For Variance

res <- bartlett.test(`Annual Salary` ~ `Company Size`, data = no_outliers)
res


## Mathematical Model

model <- lm(`Annual Salary` ~., data = no_outliers)
summary(model)

