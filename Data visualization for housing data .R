
d = read.csv("/Users/keerthim/Projects/jupyter/who_life_expect-1.csv",header=TRUE)

head(d)
dim(d)
str(d)
d$Country <- as.factor(d$Country)
d$Year <- as.factor(d$Year)
d$Status <- as.factor(d$Status)

str(d)
pairs(d,panel=panel.smooth,main="life expectancy")
summary(d)

#visualization of the distribution of the outcome 
par(mfrow=c(1,2))
hist(d$Life.expectancy,freq = FALSE)
life_expectancy <-d$Life.expectancy[!is.na(d$Life.expectancy)]
hist(life_expectancy,freq = FALSE)

#visualization of variables
require(gridExtra)
require(ggplot2)
par(mfrow=c(7,3))
p1 <- ggplot(d, aes(x = Life.expectancy)) +
  geom_histogram(binwidth = 2, fill = "lightblue", color = "black",aes(y = after_stat(density))) +
  labs(title = "Life Expectancy distribution", x = "Life Expectancy", y = "Density")

p2 <- ggplot(d, aes(x = Adult.Mortality)) +
  geom_histogram(binwidth = 2, fill = "lightblue", color = "black",aes(y = after_stat(density))) +
  labs(title = "Adult Mortality", x = "Adult Mortality", y = "Density")

p3 <- ggplot(d, aes(x = infant.deaths)) +
  geom_histogram(binwidth = 100, fill = "lightblue", color = "black",aes(y = after_stat(density))) +
  labs(title = "infant deaths ", x = "infant deaths ", y = "Density")

p4 <- ggplot(d, aes(x = Alcohol)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black",aes(y = after_stat(density))) +
  labs(title = "Alcohol", x = "Alcohol", y = "Density")

p5 <- ggplot(d, aes(x = percentage.expenditure)) +
  geom_histogram(binwidth = 800, fill = "lightblue", color = "black",aes(y = after_stat(density))) +
  labs(title = "percentage expenditure", x = "percentage expenditure", y = "Density")

p6 <- ggplot(d, aes(x = Hepatitis.B)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black",aes(y = after_stat(density))) +
  labs(title = "Hepatitis B", x = "Hepatitis B", y = "Density")

p7 <- ggplot(d, aes(x = Measles)) +
  geom_histogram(binwidth = 5000, fill = "lightblue", color = "black",aes(y = after_stat(density))) +
  labs(title = "Measles", x = "Measles", y = "Density")

p8 <- ggplot(d, aes(x = BMI)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black",aes(y = after_stat(density))) +
  labs(title = "BMI", x = "BMI", y = "Density")

p9 <- ggplot(d, aes(x = under.five.deaths)) +
  geom_histogram(binwidth = 100, fill = "lightblue", color = "black",aes(y = after_stat(density))) +
  labs(title = "under.five.deaths", x = "under.five.deaths", y = "Density")

p10 <- ggplot(d, aes(x = Polio)) +
  geom_histogram(binwidth = 2, fill = "lightblue", color = "black",aes(y = after_stat(density))) +
  labs(title = "Polio", x = "Polio", y = "Density")

p11 <-ggplot(d, aes(x = Total.expenditure)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black",aes(y = after_stat(density))) +
  labs(title = "Total.expenditure", x = "Total.expenditure", y = "Density")

p12 <- ggplot(d, aes(x = Diphtheria)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black",aes(y = after_stat(density))) +
  labs(title = "Diphtheria", x = "Diphtheria", y = "Density")

p13 <- ggplot(d, aes(x = HIV.AIDS)) +
  geom_histogram(binwidth = 2, fill = "lightblue", color = "black",aes(y = after_stat(density))) +
  labs(title = "HIV AIDS", x = "HIV AIDS", y = "Density")

p14 <- ggplot(d, aes(x = GDP)) +
  geom_histogram(binwidth = 5000, fill = "lightblue", color = "black",aes(y = after_stat(density))) +
  labs(title = "GDP", x = "GDP", y = "Density")

p15 <- ggplot(d, aes(x = Population)) +
  geom_histogram(binwidth = 80000000, fill = "lightblue", color = "black",aes(y = after_stat(density))) +
  labs(title = "Population", x = "Population", y = "Density")

p16 <- ggplot(d, aes(x = thinness..1.19.years)) +
  geom_histogram(binwidth = 2, fill = "lightblue", color = "black",aes(y = after_stat(density))) +
  labs(title = "thinness 1.19 years", x = "thinness 1.19 years", y = "Density")

p17 <- ggplot(d, aes(x = thinness.5.9.years)) +
  geom_histogram(binwidth = 2, fill = "lightblue", color = "black",aes(y = after_stat(density))) +
  labs(title = "thinness.5.9.years", x = "thinness.5.9.years", y = "Density")

p18 <- ggplot(d, aes(x = Income.composition.of.resources)) +
  geom_histogram(binwidth = 0.05, fill = "lightblue", color = "black",aes(y = after_stat(density))) +
  labs(title = "Income.composition.of.resources", x = "Income.composition.of.resources", y = "Density")

p19 <- ggplot(d, aes(x = Schooling)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black",aes(y = after_stat(density))) +
  labs(title = "Schooling", x = "Schooling", y = "Density")


p20 <- ggplot(d, aes(x = Country)) +
  geom_bar(fill = "skyblue", color = "black",width = 1.0)  + 
  labs(title = "Country", x = "Country", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1.0))
#missing data for the country
p21 <- ggplot(d, aes(x = Status)) +
  geom_bar(fill = "skyblue", color = "black")  + 
  labs(title = "Status", x = "Status", y = "Count")

grid.arrange(p1, p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18, nrow = 3, ncol = 7)

plot(d$Measles,d$Life.expectancy)
#select only numeric data
numeric_data <- d[sapply(d, is.numeric)]
Matrix <- cor(numeric_data,use = "pairwise.complete.obs")
#compute the correlation
life_expectancy_correlation <- Matrix["Life.expectancy",]
life_expectancy_correlation

#plotting the relation between Life expectancy and adult mortality , polio ,HIV AIDS

par(mfrow=c(3,1))
par(font=2,font.axis=2,font.lab=2)
plot( d$Adult.Mortality,d$Life.expectancy,
     xlab = "Adult Mortality Rate (per 1,000 people)",
     ylab = "Average Life Expectancy (Years)", 
     main = "Relationship between Life Expectancy and Adult Mortality",pch=20,col="skyblue",xaxt="n")
axis(1, at = seq(0,1000, by = 50), labels = seq(0, 1000, by = 50),las=2)

status_colors <- ifelse(d$Status == "Developed",'darkcyan', "coral")
par(font=2,font.axis=2,font.lab=2)
plot( d$Polio,d$Life.expectancy,
      xlab = "Polio Vaccination Rate (%)", 
      ylab = "Average Life Expectancy (Years)", 
      main = "Effect of Polio Vaccination on Life Expectancy",pch=20,col=status_colors,xaxt="n",
      yaxt="n",ylimt=c(0,120))
axis(1, at = seq(0,100, by = 10), labels = seq(0, 100, by = 10))
axis(2, at = seq(0,120, by = 10), labels = seq(0, 120, by = 10))
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
legend(18, 90, legend = c("Developed countries", "Developing countries"), 
       col = c("darkcyan", "coral"), pch = 20, bg = "white")

plot( d$Income.composition.of.resources,d$Life.expectancy,
      xlab = "Income composition of resources (rate)", 
      ylab = "Average Life Expectancy (Years)", 
      main = "Income composition of resources influence on Life Expectancy ",pch=20,col="lightblue",xaxt="n")
axis(1, at = seq(0,1,by = 0.1), labels = seq(0,1, by = 0.1))

#group the life expectancy, status and year
catagorical_data <- d %>% group_by(Year,Status) %>%
  summarise(Average_Life_Expectancy = mean(Life.expectancy, na.rm = TRUE)) %>%
  arrange(Year, Status)

catagorical_data <- tapply(catagorical_data$Average_Life_Expectancy, 
       list(catagorical_data$Year, catagorical_data$Status), mean)

rownames(catagorical_data)

barplot(catagorical_data,
        main=" Average life expectancy : developed and developing country",
        xlab="",
        ylab = "Average life expectancy (Years)",
        beside=TRUE,
        col=c("lightblue","darkblue"),
        ylim = c(0, max(catagorical_data) + 5)
        )
legend("topright", legend = c("Developed", "Developing"), fill = c("lightblue", "darkblue"))
axis(1, at = seq(2000, 2015, by = 1), labels = seq(2000, 2015, by = 1), las = 2)

boxplot(d$Adult.Mortality,ylab="Adult Mortality",
        main="Box plot for Adult Mortality",col="lightblue",outline =TRUE ,
        whisklty = 1,whiskcol = "darkblue",whisklwd = 2)


h=read.csv("/Users/keerthim/Projects/jupyter/housing-1.csv",header=TRUE)
head(h)
dim(h)
str(h)
h$AREA <- as.factor(h$AREA)
h$SALE_COND <- as.factor(h$SALE_COND)
h$PARK_FACIL <- as.factor(h$PARK_FACIL)
h$BUILDTYPE <- as.factor(h$BUILDTYPE)
h$UTILITY_AVAIL <- as.factor(h$UTILITY_AVAIL)
h$MZZONE <-as.factor(h$MZZONE)
h$STREET <- as.factor(h$STREET)
h$DATE_SALE <- as.Date(h$DATE_SALE,format = "%d-%m-%Y")
h$DATE_BUILD <- as.Date(h$DATE_BUILD,format = "%d-%m-%Y")
str(h)
summary(h)
#cleaning the data
h$PARK_FACIL <- gsub("Noo", "No", h$PARK_FACIL)
unique(h$PARK_FACIL)
unique(h$BUILDTYPE)
h$BUILDTYPE <- gsub("Comercial", "Commercial", h$BUILDTYPE)
h$BUILDTYPE <- gsub("Otherss", "Others", h$BUILDTYPE)
unique(h$PARK_FACIL)
unique(h$UTILITY_AVAIL)
h$UTILITY_AVAIL <- gsub("All Pub", "AllPub", h$UTILITY_AVAIL)
unique(h$STREET)
h$STREET <- gsub("Pavd", "Paved", h$STREET)
h$STREET <- gsub("NoAccess", "No Access", h$STREET)
unique(h$SALE_COND)
h$SALE_COND <- gsub("Ab Normal","AbNormal",h$SALE_COND)
h$SALE_COND <- gsub("Adj Land","AdjLand",h$SALE_COND)
h$SALE_COND <- gsub("Partiall","Partial",h$SALE_COND)
h$SALE_COND <- gsub("PartiaLl","Partial",h$SALE_COND)
unique(h$AREA)
h$
str(h)
summary(h)

h$AREA <- replace(h$AREA, h$AREA %in% c("Ana Nagar", "Ann Nagar"), "Anna Nagar")
h$AREA <- replace(h$AREA, h$AREA %in% c("Adyr"), "Adyar")
h$AREA <- replace(h$AREA, h$AREA %in% c("Chrompt","Chrmpet","Chormpet"), "Chrompet")
h$AREA <- replace(h$AREA, h$AREA %in% c("T Nagar"), "TNagar")
h$AREA <- replace(h$AREA, h$AREA %in% c("KK Nagar"), "KKNagar")
h$AREA <- replace(h$AREA, h$AREA %in% c("Karapakam"), "Karapakkam")

str(h)
summary(h)
#add the total cost to the data set
h$TOTAL_PRICE <- h$REG_FEE + h$COMMIS +h$SALES_PRICE
h$COMMIS_RATE <- round((h$COMMIS/(h$SALES_PRICE-h$REG_FEE))*100,2)

h$COMMIS_RATE <- round((h$COMMIS/(h$SALES_PRICE))*100,2)
#VISUALIZING FEW VARIABLES
require(gridExtra)
require(ggplot2)
#par(mfrow=c(7,3))
ggplot(h, aes(x = REG_FEE)) +
  geom_histogram(binwidth = 10000, fill = "lightblue", color = "black") +
  labs(title = "House Registration fee distribution", x = "House Registration fee", 
       y = "Frequency")

ggplot(h, aes(x = SALES_PRICE)) +
  geom_histogram(binwidth = 1000000, fill = "lightblue", color = "black") +
  labs(title = "House Sales Price distribution", x = "House Sales Price", y = "Frequency")

ggplot(h, aes(x = COMMIS_RATE)) +
  geom_histogram(binwidth = 0.1, fill = "lightblue", color = "black",aes(y = after_stat(density))) +
  labs(title = "House sale commision rate distribution", x = "commision rate ", y = "Density")

ggplot(h, aes(x = AREA)) +
  geom_bar(fill = "skyblue", color = "black",width = 0.5)  + 
  labs(title = "AREA", x = "AREA", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1.0))

ggplot(h, aes(x = SALE_COND)) +
  geom_bar(fill = "skyblue", color = "black",width = 0.3)  + 
  labs(title = "Sale Condition", x = "House Sale Condition (Present)", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1.0))

#Adding 
h$HOUSE_AGE <- round(as.numeric(h$DATE_SALE - h$DATE_BUILD) / 365.25, 1)

str(h)

summary(h)

#correlation between the predictors
#select only numeric data
numeric_data_housing <- h[sapply(h, is.numeric)]
Correlation_Matrix <- cor(numeric_data_housing,use = "pairwise.complete.obs")
#compute the correlation with total price
Sales_price_correlation <- Correlation_Matrix["SALES_PRICE",]
Correlation_Matrix["TOTAL_PRICE",]
Sales_price_correlation

#plotting the relation between House sale price commisition , house age, number of rooms
color_gradient <- colorRampPalette(c("deepskyblue", "cornflowerblue", "lightcoral"))
color_vector <- color_gradient(100)[as.numeric(cut((h$SALES_PRICE/83), breaks = 100))]
par(mar = c(5, 5, 4, 2) + 0.1,mgp = c(3, 1, 0))
plot( (h$COMMIS/83),(h$SALES_PRICE/83),
      ylab = "",
      xlab = "Sales Commission Fee ($)", 
      main = "Relationship between House Sales Price and Commission Amount",
      col = color_vector,
      pch=19,cex=1.5,bty="l",las=1)

grid(lty = "dotted", col = "gray")
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = rgb(0.95, 0.95, 0.95), border = NA)

points(jitter((h$COMMIS/83),factor=2), jitter((h$SALES_PRICE/83),factor=2), pch = 19, 
       col = color_vector, cex = 1.2)
grid(lty = "dotted", col = "gray")
mtext("House Sales Price ($)", side = 2, line = 4)
grid(lty = "dotted", col = "gray80")

summary(h)
str(h)

#plotting the relation between House sale price with registration fee
color_gradient <- colorRampPalette(c("powderblue", "lightsteelblue", "mistyrose"))
color_vector <- color_gradient(100)[as.numeric(cut(h$SALES_PRICE, breaks = 100))]
par(mar = c(5, 5, 4, 2) + 0.1,mgp = c(3, 1, 0))
plot( (h$REG_FEE/83),(h$SALES_PRICE/83),
      ylab = "",
      xlab = "House Registration Fee ($)", 
      main = "Relationship between House Sales Price and House Registration Fee",
      col = color_vector,
      pch=19,cex=1.5,bty="l",las=1)

grid(lty = "dotted", col = "gray")
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = rgb(0.95, 0.95, 0.95), border = NA)

points(jitter((h$REG_FEE/83),factor=2), jitter((h$SALES_PRICE/83),factor=2), pch = 19, 
       col = color_vector, cex = 1.2)
grid(lty = "dotted", col = "gray")
mtext("House Sales Price ($)", side = 2, line = 4)
grid(lty = "dotted", col = "gray80")


#plotting the relation between House sale price with House age
 house_data <- h %>% group_by(HOUSE_AGE) %>%
  summarise(Average_sales_price= mean(SALES_PRICE, na.rm = TRUE)) %>%
  arrange(HOUSE_AGE)

par(mar = c(5, 5, 4, 2) + 0.1,mgp = c(3, 1, 0))
plot( (house_data$HOUSE_AGE),(house_data$Average_sales_price/83),
      ylab = "",
      xlab = "Age of the House (Years)", 
      main = "Variation of House Sales Price with House Age",
      col = "skyblue",
      pch=19,cex=1.5,bty="l",las=1)

grid(lty = "dotted", col = "gray")
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = rgb(0.95, 0.95, 0.95), border = NA)

lines(jitter((house_data$HOUSE_AGE),factor=2), jitter((house_data$Average_sales_price/83),factor=2), pch = 19, 
       col = "skyblue", cex = 1.2)
points(jitter((house_data$HOUSE_AGE),factor=2), jitter((house_data$Average_sales_price/83),factor=2), pch = 19, 
       col = "darkblue", cex = 1)
grid(lty = "dotted", col = "gray")
mtext("Average House Sales Price ($)", side = 2, line = 4)
grid(lty = "dotted", col = "gray80")

#creating house age group and add it to the main data set
str(h)

h <- h %>%
  mutate(HOUSE_AGE_GROUP = case_when(
    HOUSE_AGE < 10 ~ "0-9 years",
    HOUSE_AGE < 20 ~ "10-19 years",
    HOUSE_AGE < 30 ~ "20-29 years",
    HOUSE_AGE < 40 ~ "30-39 years",
    HOUSE_AGE < 50 ~ "40-49 years",
    HOUSE_AGE < 60 ~ "50-59 years",
    HOUSE_AGE < 70 ~ "60-69 years",
    TRUE ~ "70+ years"
  ))
str(h)
h$HOUSE_AGE_GROUP <- as.factor(h$HOUSE_AGE_GROUP)
age_group_summary <- h %>%
  group_by(HOUSE_AGE_GROUP) %>%
  summarise(Count = n())

ggplot(age_group_summary, aes(x = HOUSE_AGE_GROUP, y = Count)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Number of Properties by House Age Group",
       x = "House Age Group",
       y = "Number of Properties") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

area_age_group_summary <- h %>%
  group_by(AREA, HOUSE_AGE_GROUP) %>%
  summarise(Count = n()) %>%
  ungroup()

unique(h$AREA)
  custom_colors <- c(
    "Karapakkam" = "#FFB6C1",  
    "Anna Nagar" = "#ADD8E6",   
    "Adyar" = "#C1E1C1",        
    "Velachery" = "#FBC4AB",    
    "Chrompet" = "#D8BFD8",     
    "KKNagar" = "#FFDAB9",     
    "TNagar" = "#E6E6FA"        
  )


ggplot(area_age_group_summary, aes(x = HOUSE_AGE_GROUP, y = Count, fill = AREA)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Number of Properties by House Age Group and Area",
       x = "House Age Group",
       y = "Number of Properties",
       fill = "Area") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
scale_fill_manual(values = custom_colors)


#finding the outliners
Q1 <- quantile(h$SALES_PRICE, 0.25, na.rm = TRUE)
Q3 <- quantile(h$SALES_PRICE, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

outliers <- h$SALES_PRICE[h$SALES_PRICE < lower_bound | h$SALES_PRICE > upper_bound]
outliers

#remove the outliners
house_data_no_outliers <- h[h$SALES_PRICE >= lower_bound & h$SALES_PRICE <= upper_bound, ]

#plotting the relation between House sale price with House age
house_data_with_no_outliers <- house_data_no_outliers %>% group_by(HOUSE_AGE) %>%
  summarise(Average_sales_price= mean(SALES_PRICE, na.rm = TRUE)) %>%
  arrange(HOUSE_AGE)

par(mar = c(5, 5, 4, 2) + 0.1,mgp = c(3, 1, 0))
plot( (house_data_with_no_outliers$HOUSE_AGE),(house_data_with_no_outliers$Average_sales_price/83),
      ylab = "",
      xlab = "Age of the House (Years)", 
      main = "Variation of House Sales Price with House Age",
      col = "skyblue",
      pch=19,cex=1.5,bty="l",las=1)

grid(lty = "dotted", col = "gray")
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = rgb(0.95, 0.95, 0.95), border = NA)

lines(jitter((house_data_with_no_outliers$HOUSE_AGE),factor=2), jitter((house_data_with_no_outliers$Average_sales_price/83),factor=2), pch = 19, 
      col = "skyblue", cex = 1.2)
points(jitter((house_data_with_no_outliers$HOUSE_AGE),factor=2), jitter((house_data_with_no_outliers$Average_sales_price/83),factor=2), pch = 19, 
       col = "darkblue")
grid(lty = "dotted", col = "gray")
mtext("Average House Sales Price ($)", side = 2, line = 4)
grid(lty = "dotted", col = "gray80")


ggplot(age_group_summary, aes(x = HOUSE_AGE_GROUP, y = Count)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Number of Properties by House Age Group",
       x = "House Age Group",
       y = "Number of Properties") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


unique(h$AREA)
custom_colors <- c(
  "Karapakkam" = "#FFB6C1",  
  "Anna Nagar" = "#ADD8E6",   
  "Adyar" = "#C1E1C1",        
  "Velachery" = "#FBC4AB",    
  "Chrompet" = "#D8BFD8",     
  "KKNagar" = "#FFDAB9",     
  "TNagar" = "#E6E6FA"        
)


ggplot(area_age_group_summary, aes(x = HOUSE_AGE_GROUP, y = Count, fill = AREA)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Number of Properties by House Age Group and Area",
       x = "House Age Group",
       y = "Number of Properties",
       fill = "Area") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = custom_colors)

















