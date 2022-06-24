# install packages & libraries
install.packages('dplyr')
install.packages('pastecs')
install.packages('corrplot')
install.packages('Hmisc')
install.packages('statmod')
install.packages('ggside')
install.packages('GGally')
install.packages('ggcorrplot')
install.packages('ggpubr')
install.packages('vcd')
install.packages('ggridges')

library('ggridges')
library(vcd)
library(ggside) 
library(dplyr)
library(ggplot2)
library(pastecs)
library(corrplot)
library(Hmisc)
library(ggstatsplot)
library(GGally)
library(ggpubr)
theme_set(theme_pubclean())

utils::install.packages(pkgs = "ggstatsplot")
#load dataset
sales_data <- read.csv('wishdataset.csv')

#explore the data

head(sales_data)
View(sales_data)


# get the structure
str(sales_data)

#no of rows & columns
dim(sales_data)

# There are 1573 rows and 43 columns, for the purpose of this analysis we will remove irrelavant variables,
#i.e for instance, variables that have unique values for each row, eg title.
# below is a list varaibles to be tested in this analysis.

#price,retail_price, uses_ads_boosts, rating,rating_count, product_color,
#product_variation_size id, origin_country.

#select the variables listed above;

sales_data1<-select(sales_data,units_sold, price, retail_price,
                    uses_ad_boosts, 
                    rating,rating_count, product_color,
                    product_variation_size_id,
                    origin_country)

sales_data2<- sales_data1 %>% 
  mutate(gap_price = price - retail_price)

View(sales_data2)


dim(sales_data2)# There are 1573 rows and 10 columns


# view missing number
is.na(sales_data1)
sum(is.na(sales_data1)) #there are no missing values

#DATA PREPARATION
#check the structure and split numeric/integer from categorical
str(sales_data1)

unique(sales_data1[c("origin_country")])

length(unique(sales_data1$product_color)) #102 product colour
length(unique(sales_data1$product_variation_size_id)) #107 product size
length(unique(sales_data1$merchant_name)) #10 merchant name
length(unique(sales_data1$origin_country)) #102 origin country

# find the top ten colors
# find the top ten sizes
# convert

#EXPLORING CATEGORICAL VARIABLES

#group by count of size
count_size<- final_data %>%
  group_by(product_variation_size_id) %>%
  tally() %>%
  arrange(desc(n))


#group by count of colour
count_col<- finaldata%>%
  group_by(product_color) %>%
  tally() %>%
  arrange(desc(n))

# select the most
# what product colours are most popluar on wish platform?
# What sizes are the users looking for the most?

size_to_include <-c('S','XS','M','XXS','L','XL','XXXS')
col_to_include <- c('black','white','yellow','blue','pink',
                    'red','green','grey','purple')

main_data<-filter(sales_data2,product_variation_size_id %in%
                    size_to_include)

final_data <- filter(main_data, product_color %in% 
                       col_to_include)

#COUNTRY
df_origin<- final_data %>%
  group_by(origin_country,product_variation_size_id,product_color) %>%
  summarise(counts=n())


ggdotchart(df_origin, x = "origin_country", y ="counts",
           color = "origin_country", palette = "jco", size = 3, 
           add = "segment", 
           add.params = list(color = "lightgray", size = 1.5),
           position = position_dodge(0.3),
           ggtheme = theme_pubclean())


#Product Color
df_col<- final_data %>%
  group_by(product_color,origin_country,product_variation_size_id) %>%
  summarise(counts=n())


ggplot(df_col, aes(x = product_color, y = counts)) +
  labs(
    title = " Top 9 Product Colours Purchased on Wish Platform",
    x = "Product Colour",
    y = "Counts)") +
  geom_bar(
    aes(color = product_color, fill =product_color),
    stat = "identity", position = position_stack()
  ) +
  
  scale_color_manual(values = c("#000000",'#0072B2','#009E73','#999999','#FF9999','#CC79A7','#FF0000','#FFFFFF','#E69F00'))+
  scale_fill_manual(values = c("#000000",'#0072B2','#009E73','#999999','#FF9999','#CC79A7','#FF0000','#FFFFFF','#E69F00')) 
  

#PRODUCT SIZE
ggplot(data = final_data) +
geom_bar(mapping = aes(x = product_variation_size_id, 
                       fill=product_variation_size_id))+
  labs(
    title = " Seven Most Popular Product Size Variation",
    x = "Product Size Variation Id",
    y = "Counts)",
    colour = "Size Id"
  )


#select all numeric data
sales_data2_num <- select(sales_data2,units_sold, price, retail_price,gap_price,
                          uses_ad_boosts, rating,rating_count)
View(sales_data2_num)


#DESCRIPTIVE STATISTICS

summary(sales_data2_num) # compute statistics
stat.desc(sales_data2_num)
lapply(sales_data2_num[,], sd) # compute all the sd
lapply(sales_data2_num[,], var) # compute all the sd


#Total sales per size
total_sales_size<- final_data %>% 
  group_by(product_variation_size_id) %>%
  summarise(total_sales= sum(units_sold),
            total_price= sum(price),
            total_rating= sum(rating),
            .groups='drop')

by__total_size<- total_sales_size%>% 
  group_by(total_sales)
by_total_size_desc<- by__total_size[order(by__total_size$total_sales,
                                          decreasing = TRUE),]


#Plot of total sales by product size
ggplot(by_total_size_desc, 
  aes(x = product_variation_size_id,
      y=total_sales))+
  geom_bar(stat='identity', fill='cornflowerblue')+
  
  geom_text(aes(label = total_sales), 
            vjust = -0.25) +
   
  labs(title = "Total Sales By Product Size", 
       subtitle = "seven most popular size as of the year 2020",
       x = "",
       y = "")
  

#Average sales per size
avg_sales_size<- final_data %>%
  group_by(product_variation_size_id) %>%
  summarise(mean_sales= mean(units_sold),
            mean_price= mean(price),
            mean_rating= mean(rating))

rnd_size<- avg_sales_size%>% mutate_if(is.numeric, ~round(., 1))


#average sales by size (Ordered)
by_size<- rnd_size %>% 
  group_by(mean_sales)
by_size_desc<- by_size[order(by_size$mean_sales, decreasing = TRUE),]


#Plot of avg sales by product size
ggplot(by_size_desc, 
       aes(x = product_variation_size_id,
           y=mean_sales))+
  geom_bar(stat='identity', fill='Darkgreen')+
  
  geom_text(aes(label = mean_sales), 
            vjust = -0.25) +
  
  labs(title = "Average Sales By Product Size", 
       subtitle = "seven most popular sizes as of June 2020",
       x = "",
       y = "")


#AGGREGATE BY COLOUR

total_sales_col<- final_data %>% group_by(product_color) %>%
  summarise(total_sales= sum(units_sold),
            total_price= sum(price),
            total_rating= sum(rating),
            .groups='drop')

by__total_col<- total_sales_col%>% 
  group_by(total_sales)

by_total_col_desc<- by__total_col[order(by__total_col$total_sales, decreasing = TRUE),]


#Plot of total sales by product size
ggplot(by_total_col_desc, 
       aes(x = product_color,
           y=total_sales))+
  geom_bar(stat='identity', fill='cornflowerblue')+
  
  geom_text(aes(label = total_sales), 
            vjust = -0.25) +
  
  labs(title = "Total Sales By Product Colour", 
       subtitle = "top nine colours as of the year 2020",
       x = "",
       y = "")


#Average sales per size
avg_sales_col<- final_data %>%
  group_by(product_color) %>%
  summarise(mean_sales= mean(units_sold),
            mean_price= mean(price),
            mean_rating= mean(rating))

rnd_col<- avg_sales_col%>% mutate_if(is.numeric, ~round(., 1))


#average sales by size (Ordered)
by_col<- rnd_col %>% 
  group_by(mean_sales)
by_col_desc<- by_col[order(by_col$mean_sales, decreasing = TRUE),]

#Plot of avg sales by product size
ggplot(by_col_desc, 
       aes(x = product_color,
           y=mean_sales))+
  geom_bar(stat='identity', fill='Darkgreen')+
  
  geom_text(aes(label = mean_sales), 
            vjust = -0.25) +
  
  labs(title = "Average Sales By Product Colour", 
       subtitle = "nine most popular colours as of the year 2020",
       x = "",
       y = "")

# COUNTRY

orig_to_include <-c('CN','GB','SG','US','VE')

final_data1 <- filter(final_data, origin_country %in% orig_to_include)

total_sales_orig<- final_data1 %>% group_by(origin_country) %>%
  summarise(total_sales= sum(units_sold),
            total_price= sum(price),
            total_rating= sum(rating),
            .groups='drop')

by__total_orig<- total_sales_orig%>% 
  group_by(total_sales)
by_total_orig_desc<- by__total_orig[order(by__total_orig$total_sales, decreasing = TRUE),]


#Plot for total sales by country
ggplot(by_total_orig_desc, 
       aes(x = origin_country,
           y=total_sales))+
  geom_bar(stat='identity', fill='cornflowerblue')+
  
  geom_text(aes(label = total_sales), 
            vjust = -0.25) +
  
  labs(title = "Total Sales By Country", 
       subtitle = "as of the year 2020",
       x = "Country",
       y = "Units Sold")


#Average sales per country
avg_sales_orig<- final_data1 %>%
  group_by(origin_country) %>%
  summarise(mean_sales= mean(units_sold),
            mean_price= mean(price),
            mean_rating= mean(rating))

rnd_orig<- avg_sales_orig%>% mutate_if(is.numeric, ~round(., 1))


#average sales by size (Ordered)
by_orig<- rnd_orig %>% 
  group_by(mean_sales)
by_orig_desc<- by_orig[order(by_orig$mean_sales, decreasing = TRUE),]

#plot of avg sales by country
ggplot(by_orig_desc, 
       aes(x = origin_country,
           y=mean_sales))+
  geom_bar(stat='identity', fill='Darkgreen')+
  
  geom_text(aes(label = mean_sales), 
            vjust = -0.25) +
  
  labs(title = "Average Sales By Country", 
       subtitle = "as of the year 2020",
       x = "Country",
       y = "Average Units Sold")


#UNITS SOLD VS ADS

ggplot(final_data1, aes(x =units_sold )) +
  geom_histogram(fill = "cornflowerblue",
                 color = "white") +
  facet_wrap(~uses_ad_boosts, ncol = 1) +
  labs(title = "Units Sold By Ads Boost")


#UNITS SOLD VS RATING

ggplot(final_data, 
       aes(x = rating_count, 
           y = units_sold)) +
  geom_line(size = 1.5, 
            color = "lightgrey") +
  geom_point(size = 3, 
             color = "yellow") +
  labs(y = "NUmber of Units Sold", 
       x = "Rating Count",
       title = "Units Solds Versus Rating Count",
       subtitle = "as of the year  2020"
       )


#UNITS SOLD VS gap_price

ggplot(final_data, 
       aes(x = gap_price, 
           y = units_sold)) +
  geom_line(size = 1.5, 
            color = "lightgrey") +
  geom_point(size = 3, 
             color = "darkgreen") +
  labs(y = "NUmber of Units Sold", 
       x = "Gap Price",
       title = "Units Sold Varitaion With By Gap Price",
       subtitle = "as of the year  2020"
  )


#Rating Vs Rating_Count
ggplot(final_data, 
       aes(x = rating_count, 
           y = rating)) +
  geom_line(size = 1.5, 
            color = "lightgrey") +
  geom_point(size = 3, 
             color = "red") +
  labs(y = "Rating", 
       x = "Rating Count",
       title = "Product Rating Versus Rating Count",
       subtitle = "as of the year  2020"
  )




# PEARSON CORRELATION TEST
cor(sales_data2_num) #rating_count has the highest postive correlation
cor(sales_data2_num[,c('units_sold','price','retail_price','uses_ad_boosts',
                       'rating','rating_count','gap_price')])


# Correlation Visualization
pairs(sales_data2_num[,c('units_sold','price','retail_price','uses_ad_boosts',
                         'rating','rating_count')])
corrplot(cor(sales_data2_num),
         method='number',
         type= 'upper')


#using Pearson correlation:
#to test whether the association between the numeric variables 
#is significantly significant

# Ho: p=0 (there is no linear relationship between the variables)
# H1: p!=0 (there is a linear relationship between the variables)

corr_price<- cor.test(sales_data2_num$units_sold, 
                      sales_data2_num$price)
corr_price

#INTERPRETATION:

# Based on the correlation test, there is a negative correlation between the units sold and the price. However,
#the result of the Pearson correlation test shows that there is a no statistically significant association
#between the two variables.Therefore,the null hypothesis is not rejected.(p_value = 0.3253, r pearson=-0.02).

corr_retail<- cor.test(sales_data2_num$units_sold, 
                       sales_data2_num$retail_price)
corr_retail
#INTERPRETATION:

# Based on the correlation test, there is a positive correlation between the units sold and the price. However,
#the result of the Pearson correlation test shows that there this relationship is not statistically significant 
#Therefore,the null hypothesis is not rejected.(p_value = 0.6165, r pearson=0.013).

corr_rating<- cor.test(sales_data2_num$units_sold,
                       sales_data2_num$rating)
corr_rating
#INTERPRETATION:

# Based on the correlation test, there is a positive correlation between the units sold and the price. However,
#the result of the Pearson correlation test shows that this relationship is not statistically significant 
#Therefore,the null hypothesis is not rejected.(p_value = 0.1176, r pearson=0.039).

corr_rat_cnt<- cor.test(sales_data2_num$units_sold,
                        sales_data2_num$rating_count)
corr_rat_cnt


#INTERPRETATION:

# Based on the correlation test, there is a positive correlation between the units sold and the price and the result of the Pearson correlation test shows that this relationship is statistically significant 
#Therefore,the alternate hypothesis is not rejected.(p_value = p-value < 0.5246, r pearson= 0.8994637).

corr_ads<- cor.test(sales_data2_num$units_sold, 
                    sales_data2_num$uses_ad_boosts)
corr_ads


#INTERPRETATION:

# Based on the correlation test, there is a negative correlation between the units sold and whether or not the vendors use ads. However,
#the result of the Pearson correlation test shows that this relationship not statistically significant.
#between the two variables.Therefore,the null hypothesis is not rejected.(p_value = 0.3253, r pearson=-0.016).

corr_gap<- cor.test(sales_data2_num$units_sold, sales_data2_num$gap_price)
corr_gap


#INTERPRETATION:

# Based on the correlation test, there is a negative correlation between the units sold and whether or not the vendors use ads. However,
#the result of the Pearson correlation test shows that this relationship not statistically significant.
#between the two variables.Therefore,the null hypothesis is not rejected.(p_value = 0.3253, r pearson=-0.016).



#Visualization for correlation test

ggscatterstats(
  data = sales_data1_num, 
  x = rating, 
  y = units_sold,
  title = "Correlation Test For Unit Sold and Rating",
  messages = FALSE
)

#Combined Correlation

# this plot combines the correlation coefficients and 
#correlation tests and shows all possible scatter plots
ggpairs(sales_data2_num[,c('units_sold','price','retail_price',
                           'uses_ad_boosts',
                           'rating','rating_count','gap_price')])

# Plot for Correlations

ggcorrmat(
  data=sales_data2_num[,c('units_sold','price','retail_price','uses_ad_boosts',
                          'rating','rating_count','gap_price')],
  type='parametric', 
  colors=c('darkred','white','steelblue'))

  
#MACHINE LEARNING

#include categorical variable
sales_data2_num2 <- select(sales_data2,units_sold, price, retail_price,gap_price,
                           uses_ad_boosts, rating,rating_count,product_color)
sales_data2_num3 <- select(sales_data2,units_sold, price, retail_price,gap_price, 
                           uses_ad_boosts, rating,rating_count,product_color,
                           product_variation_size_id)

#build the three different Predictive Models

sales_model<-lm(log(units_sold) ~ price + retail_price +rating +rating_count +
                  uses_ad_boosts ,data= sales_data2_num)

sales_model2<-lm(log(units_sold) ~ price + retail_price +rating +rating_count +
                   uses_ad_boosts + product_color,data=sales_data2_num2)

sales_model3<-lm(log(units_sold) ~ price + retail_price +rating +rating_count + 
                   uses_ad_boosts + product_color+ product_variation_size_id,
                 data=sales_data2_num3)

#view results
summary(sales_model)
summary(sales_model2)
summary(sales_model3)

# Prediction Using MLR

sales_data2_num$pred <- predict(sales_model, sales_data2_num)
sales_data2_num2$pred <- predict(sales_model2, sales_data2_num2)
sales_data2_num3$pred <- predict(sales_model3, sales_data2_num3)

# model plot
install.packages('coefplot')
library(coefplot)

coefplot(sales_model)
#multiplot(sales_model,sales_model2,sales_model3)

# compute the correlation between the predicted units sold 
#and actual units sold

cor(sales_data2_num$pred, sales_data2_num$units_sold)
cor(sales_data2_num2$pred, sales_data2_num2$units_sold)
cor(sales_data2_num3$pred, sales_data2_num3$units_sold)


#DATA ASSUMPTIONS FOR MLR
install.packages('olsrr')
library(olsrr)

par(mfrow=c(2,2))
plot(sales_model)

#DATA ASSUMPTIONS FOR MLR
install.packages('olsrr')
library(olsrr)
#residual vs fitted values
ols_plot_resid_fit(sales_model)
ols_plot_resid_fit(sales_model2)
ols_plot_resid_fit(sales_model3)

#residual histogram to check normality
ols_plot_resid_hist(sales_model)
ols_plot_resid_qq(sales_model2)
ols_plot_resid_qq(sales_model3)

#test normality assumption
ols_test_normality(sales_model)    #model 1
shapiro.test(sales_model$residuals)
ols_test_normality(sales_model2)   #model 2
shapiro.test(sales_model2$residuals)
ols_test_normality(sales_model3)   #model 3
shapiro.test(sales_model3$residuals)



#OTHERS

#EXPLORING NUMERIC VARIABLES

#HISTOGRAM

ggplot(sales_data2_num) + aes(x=units_sold)+
  geom_histogram(bins=12,color='red', fill='yellow')+
  labs (
    title='Histogram Plot For Number Of Units Sold',
    x='Number Of Units Sold',
    y= 'Count') +
  theme_classic()


ggplot(sales_data2_num) + aes(x=log(units_sold))+
  geom_histogram(bins=12,color='red', fill='red')+
  labs (
    title='Log Transformation of Units Sold',
    x='Number Of Units Sold',
    y= 'Count') +
  theme_classic()
  

ggplot(sales_data1_num) + aes(x=price)+
  geom_histogram(bins=12,color='darkred', fill='red')+
  labs (
    title='Histogram Plot For Price Distribution',
    x='Price',
    y= 'Count') +
  theme_classic()

ggplot(sales_data1_num) + aes(x=retail_price)+
  geom_histogram(bins=12,color='darkgreen', fill='lightgreen')+
  labs (
    title='Histogram Plot For Retail Price',
    x='Retail Price',
    y= 'Count') +
  theme_classic()

ggplot(sales_data1_num) + aes(x=rating)+
  geom_histogram(bins=12,color='darkblue', fill='lightblue')+
  labs (
    title='Histogram Plot For Rating',
    x='Rating',
    y= 'Count') +
  theme_classic()

ggplot(sales_data1_num) + aes(x=rating_count)+
  geom_histogram(bins=12,color='darkred', fill='lightpink')+
  labs (
    title='Histogram Plot For Rating Count',
    x='Rating Count',
    y= 'Count') +
  theme_classic()


ggplot(sales_data1_num) + aes(x=uses_ad_boosts)+
  geom_histogram(bins=12,color='darkblue', fill='cyan')+
  labs (
    title='Histogram Plot For Uses Ad Boosts',
    x=' Uses Ad Boosts',
    y= 'Count') +
  theme_classic()


#Boxplot
boxplot(sales_data1_num$units_sold, main="Boxplot of Unit of Products Sold",
        ylab="Units Sold")
boxplot(sales_data1_num$price, main="Boxplot of Price of Products",
        ylab="Price")
boxplot(sales_data1_num$retail_price, main="Boxplot of Retail Price of Products",
        ylab="Retail Price")
boxplot(sales_data1_num$rating_count, main="Boxplot of Rating Count of Products",
        ylab="Rating Count")
boxplot(sales_data1_num$rating, main="Boxplot of Rating of Products",
        ylab="Rating")


#SCATTER PLOTS
basic 
plot(x = sales_data1$price, y = sales_data1$units_sold,
     main = "Scatterplot of Units Sold Vs Price",
     xlab = "Price(mi.)",
     ylab = "Units Sold")

# Units sold vs price (uses ads)
ggplot(data = sales_data1) +
  geom_point(mapping = aes(x = price, y = units_sold, color = uses_ad_boosts))+
  labs(
    title = 'Distribution Of Units Sold VS Price',
    x = "Price(L)",
    y = "Units Sold",
    colour = "Uses Ad Boost or Not"
    )

# Units sold vs price (country)
ggplot(sz, aes(x = rating, y = units_sold, group = product_variation_size_id))+
  geom_point(aes(color= product_variation_size_id))
  labs(
    title = 'Distribution Of Units Sold VS Price',
    x = "Price(L)",
    y = "Units Sold",
    colour = "Origin Country"
  )

# Units sold vs price (uses ads)
ggplot(data = sales_data1) +
  geom_point(mapping = aes(x = rating, y = units_sold, color = uses_ad_boosts))+
  labs(
    title = 'Distribution Of Units Sold VS Rating',
    x = "Rating",
    y = "Units Sold",
    colour = "Uses Ad Boost or Not"
  )

#units sold and rating (country)
ggplot(sales_data1, aes(x = rating, y = units_sold, group = origin_country))+
  geom_point(aes(color= origin_country))
  labs(
    title = 'Distribution Of Units Sold VS Rating',
    x = "Rating",
    y = "Units Sold",
    colour = "Origin Country"
  )
#rating count
  
ggplot(sales_data1, aes(x = rating, y = units_sold, group = origin_country))+
 geom_point(aes(color= origin_country))
 labs(
  title = 'Distribution Of Units Sold VS Rating Count',
  x = "Rating Count",
  y = "Units Sold",
  colour = "Origin Country"
)


