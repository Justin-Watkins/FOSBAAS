<<<<<<< HEAD
#-----------------------------------------------------------------
# This file contains all code from the FOSBAAS book
# Justin Watkins
# Copyright 2022
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Chapter 1
#-----------------------------------------------------------------


library(ggplot2)
library(dplyr)
#-----------------------------------------------------------------
# creating a color palette
#-----------------------------------------------------------------
palette <- c('dodgerblue','grey25','mediumseagreen', 'coral',
             'orchid','firebrick','goldenrod','cyan',
             'brown','steelblue','magenta')
#-----------------------------------------------------------------
# Creating a custom theme
#-----------------------------------------------------------------
require(ggplot2)
graphics_theme_1 <- ggplot2::theme() + 
  theme(axis.text.x  = element_text(angle = 0, size = 14, 
                                    vjust = 0, color = "grey10"),  
        axis.text.y  = element_text(angle = 0, size = 14, 
                                    vjust = 0, color = "grey10"),  
        axis.title.x = element_text(size = 16, face = "plain", 
                                    colour = "grey10"), 
        axis.title.y = element_text(size = 16, face = "plain", 
                                    color = "grey10"), 
        legend.title = element_text(size = 14, face = "plain", 
                                    color = "grey10"), 
        legend.text  = element_text(size = 11, 
                                    color = "grey10"), 
        plot.title   = element_text(colour = "grey10", 
                                    size = 14, angle = 0, 
                                    hjust = .5, vjust = .5, 
                                    face = "bold"), 
        legend.position   = "right", 
        legend.background = element_rect(fill = "grey99", 
                                         size = 3,  
                                         linetype= "solid", 
                                         colour = "grey99"), 
        legend.key        = element_rect(fill = "grey99", 
                                         color = "grey99"), 
        strip.background  = element_rect(fill =  "grey99", 
                                         colour = "grey99"), 
        strip.text        = element_text(size = 14, 
                                         face = "plain", 
                                         color = "grey10"), 
        panel.grid.major  = element_line(colour = "grey80"),  
        panel.grid.minor  = element_line(colour = "grey80"), 
        panel.background  = element_rect(fill = "grey99", 
                                         colour = "grey99"), 
        plot.background   = element_rect(fill = "grey99", 
                                         colour = "grey99"))

#-----------------------------------------------------------------
#-- SQL example
#-----------------------------------------------------------------
#SELECT 
#  A.customer_id
# ,A.email_addr
# ,B.plan_id
# ,B.price
#FROM Customer A LEFT JOIN Plans B ON A.customer_id = B.customer_id
#WHERE A.email_addr = "Ted.Williams@someserver.com"

#-----------------------------------------------------------------
#-- SQL example
#-----------------------------------------------------------------
#SELECT 
#  A.customer_id,
# ,A.ticketing_system_id
# ,B.deal_id
# ,C.opportunity_id
#FROM Customer A LEFT JOIN deal B ON A.customer_id = B.customer_id
#LEFT JOIN opportunity C ON B.deal_id = C.deal_id
#WHERE A.email_addr = "Ted.Williams@someserver.com"

#-----------------------------------------------------------------
# Demonstrate a linear equation Y = mx + b
#-----------------------------------------------------------------
x        <- c(-5,-4,-3,-2,-1,0,1,2,3,4,5) # x values
m        <- 2                             # Slope
b        <- 5                             # Y Intercept
y_values <- list()                        # Holds the output

i <- 1
while(i <= length(x)){
  y_values[i] <- x[i]*m + b
  i <- i + 1
}

line_values <- data.frame(x,unlist(y_values))
names(line_values) <- c('x', 'y')

#-----------------------------------------------------------------
# Linear equation output
#-----------------------------------------------------------------
lin_equ_out <- 
  ggplot(data = line_values, aes(x = x, y = y))  + 
  geom_line(color  = 'dodgerblue')               +
  geom_point(color = 'grey15', size = 2)         +
  graphics_theme_1

#-----------------------------------------------------------------
# Linear equation output
#-----------------------------------------------------------------
set.seed(101010)
lin_equ_out_b <- 
  ggplot(data = line_values, aes(x = x, y = y) )  + 
  geom_line(color   = 'dodgerblue')               +
  geom_jitter(color = 'grey15',height = 3,
              width = 0,size = 2)                 +
  graphics_theme_1

#-----------------------------------------------------------------
# Customer renewal data
#-----------------------------------------------------------------
renewal_data <- FOSBAAS::customer_renewals

#-----------------------------------------------------------------
# Customer renewal data
#-----------------------------------------------------------------
d_tree <- 
  rpart::rpart(formula = renewed ~ distance, 
               method  = "class",
               data    = renewal_data)
rpart.plot::rpart.plot(d_tree,
                       type  = 4,
                       extra = 101)

#-----------------------------------------------------------------
# percap scatter plot
#-----------------------------------------------------------------
set.seed(714)
percap_data <- tibble::tibble(
  percap = rnorm(81,40,10),
  revenue = rnorm(81,1000000,200000)
)
x_label  <- ('\n Percap')
y_label  <- ('Revenue \n')
title    <- ('Revenue vs. Percap')
scatter_percap <- 
  ggplot2::ggplot(data  = percap_data, 
                  aes(x = percap, 
                      y = revenue))                   +
  geom_point(alpha = .9, color = 'dodgerblue')        +
  geom_rug(color = 'coral')                           +
  scale_y_continuous(label = scales::dollar)          +
  scale_x_continuous(label = scales::dollar)          +
  xlab(x_label)                                       + 
  ylab(y_label)                                       + 
  ggtitle(title)                                      +
  graphics_theme_1

#-----------------------------------------------------------------
# Chapter 2
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Linear equation function
#-----------------------------------------------------------------
f_linear_equation <- function(x,slope,yIntercept){
  y <- slope*x + yIntercept
  return(y)
}

#-----------------------------------------------------------------
# Linear equation function inputs
#-----------------------------------------------------------------
f_linear_equation( x          = 2,
                   slope      = 10,
                   yIntercept = 7) 

#-----------------------------------------------------------------
# Create lead scoring data
#-----------------------------------------------------------------
library(FOSBAAS)
f_create_lead_scoring_data(714, 
                           5000,
                           "2021",
                           f_calculate_tenure,
                           f_calculate_spend,
                           f_calculate_ticket_use,
                           f_renewal_assignment,
                           f_assign_renewal,
                           renew = T)



#-----------------------------------------------------------------
# Chapter 3
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# High level schedule data
#-----------------------------------------------------------------
library(FOSBAAS)
season_data <- FOSBAAS::season_data

#-----------------------------------------------------------------
# Histograms
#-----------------------------------------------------------------
x_label  <- ('\n Ticket Sales')
y_label  <- ('Count \n')
title    <- ('Distribution of Seasonal Ticket Sales')
legend   <- ('Season')
hist_sales <- 
  ggplot2::ggplot(data  = season_data,
                  aes(x = ticketSales,
                      fill  = factor(season)))         +
  geom_histogram(binwidth = 1000)                      +
  scale_fill_manual(legend, values = palette)          +
  geom_rug(color = 'coral')                            +
  scale_x_continuous(label = scales::comma)            +
  scale_y_continuous(label = scales::comma)            +
  xlab(x_label)                                        + 
  ylab(y_label)                                        + 
  ggtitle(title)                                       +
  graphics_theme_1

#-----------------------------------------------------------------
# Kernel density plot
#-----------------------------------------------------------------
x_label  <- ('\n Ticket Sales')
y_label  <- ('Density \n')
title    <- ('Distribution of Seasonal Ticket Sales')
legend   <- ('Season')
density_sales <- 
  ggplot2::ggplot(data = season_data, 
                  aes(x    = ticketSales, 
                      fill = factor(season)))                +
  geom_density(alpha = .5)                                   +
  scale_fill_manual(legend,values = palette)                 +
  geom_rug(color = 'coral')                                  +
  scale_x_continuous(label = scales::comma)                  +
  scale_y_continuous(label = scales::percent)                +
  xlab(x_label)                                              + 
  ylab(y_label)                                              + 
  ggtitle(title)                                             +
  graphics_theme_1

#-----------------------------------------------------------------
# Demonstrate AUC
#-----------------------------------------------------------------
den      <- density(season_data$ticketSales)
bin_size <- (den$x[2] - den$x[1])

round(sum(den$y) * bin_size,2) # Approximates to 1

#-----------------------------------------------------------------
# Demonstrate AUC at 40,000 Tickets
#-----------------------------------------------------------------
sum(den$y[den$x >= 40000]) * bin_size

#-----------------------------------------------------------------
# Faceting a plot
#-----------------------------------------------------------------
x_label  <- ('\n Ticket Sales')
y_label  <- ('Count \n')
title    <- ('Distribution of Seasonal Ticket Sales')
histogram_sales_facet <- 
  ggplot2::ggplot(data = season_data, 
                  aes(x = ticketSales))                        +
  facet_grid(season ~ .)                                       +
  geom_histogram(binwidth = 1000, fill = palette[1])           +
  geom_rug(color = 'coral')                                    +
  scale_x_continuous(label = scales::comma)                    +
  scale_y_continuous(label = scales::comma)                    +
  xlab(x_label)                                                + 
  ylab(y_label)                                                + 
  ggtitle(title)                                               +
  graphics_theme_1 

#-----------------------------------------------------------------
# Box plots
#-----------------------------------------------------------------
x_label  <- ('\n Season')
y_label  <- ('Ticket Sales \n')
title    <- ('Distribution of Seasonal Ticket Sales')
box_sales <- 
  ggplot2::ggplot(data  = season_data, 
                  aes(x = factor(season), 
                      y = ticketSales))               +
  geom_boxplot(fill = 'dodgerblue')                   +
  geom_jitter(alpha = .2,  height = 0, 
              width = .25, color  = 'coral')          +
  geom_rug(color = 'coral')                           +
  scale_y_continuous(label = scales::comma)           +
  xlab(x_label)                                       + 
  ylab(y_label)                                       + 
  ggtitle(title)                                      +
  graphics_theme_1

#-----------------------------------------------------------------
# violin plot
#-----------------------------------------------------------------
x_label  <- ('\n Season')
y_label  <- ('Ticket Sales \n')
title    <- ('Distribution of Seasonal Ticket Sales')
violin_sales <-
  ggplot2::ggplot(data = season_data, 
                  aes(x = factor(season), 
                      y = ticketSales))             +
  geom_violin(fill = 'dodgerblue')                  +
  geom_jitter(alpha = .35, height = 0, 
              width = .25, color = 'coral')         +
  geom_rug(color = 'coral')                         +
  scale_y_continuous(label = scales::comma)         +
  xlab(x_label)                                     + 
  ylab(y_label)                                     + 
  ggtitle(title)                                    +
  graphics_theme_1

#-----------------------------------------------------------------
# Line plot
#-----------------------------------------------------------------
x_label <- ('\n Game Number')
y_label <- ('Ticket Sales \n')
title   <- ('Ticket Sales by Game Number')
legend  <- 'Season'
line_sales <- 
  ggplot2::ggplot(data      = season_data, 
                  aes(x     = gameNumber,
                      y     = ticketSales,
                      color = factor(season)))             +
  geom_line(size = .9)                                     +
  scale_color_manual(legend, values = palette)             +
  scale_y_continuous(label = scales::comma)                +
  xlab(x_label)                                            + 
  ylab(y_label)                                            + 
  ggtitle(title)                                           +
  graphics_theme_1 + theme(legend.position   = "bottom")

#-----------------------------------------------------------------
# Tile plot or heat map
#-----------------------------------------------------------------
x_label <- ('\n Day of Week')
y_label <- ('Month \n')
title   <- ('Ticket Sales by Day and Month')
# compress data into an easier format
sd_comp <- season_data                    %>% 
  select(dayOfWeek,month,ticketSales)     %>%
  group_by(dayOfWeek,month)               %>%
  summarise(avgSales = mean(ticketSales))

tile_sales <- 
  ggplot2::ggplot(data     = sd_comp, 
                  aes(x    = dayOfWeek,
                      y    = month,
                      fill = avgSales))                       +
  geom_tile()                                                 +
  scale_fill_gradient(low = "white", high = "dodgerblue",
                      name = 'Tickets',label = scales::comma) +
  xlab(x_label)                                               + 
  ylab(y_label)                                               + 
  ggtitle(title)                                              +
  graphics_theme_1

#-----------------------------------------------------------------
# Hexplots
#-----------------------------------------------------------------
x_label  <- ('\n Game Number')
y_label  <- ('Ticket Sales \n')
title   <- ('Ticket Sales by game')

hex_sales <- 
  ggplot2::ggplot(data     = season_data, 
                  aes(x    = gameNumber,
                      y    = ticketSales))                    +
  geom_hex()                                                  +
  scale_fill_gradient(low = "dodgerblue", high = "coral",
                      name = 'Count',label = scales::comma)   +
  scale_y_continuous(label = scales::comma)                   +
  xlab(x_label)                                               + 
  ylab(y_label)                                               + 
  ggtitle(title) 

#-----------------------------------------------------------------
# Bar plot version one
#-----------------------------------------------------------------
x_label  <- ('\n Season')
y_label  <- ('Proportion of Ticket Sales \n')
title    <- ('Proportion of Ticket Sales by DOW')
bar_sales_pro <- 
  ggplot2::ggplot(data     = season_data, 
                  aes(y    = ticketSales,
                      x    = season,
                      fill = weekEnd))                   +
  geom_bar(stat = 'identity',position = 'fill')          +
  scale_fill_manual(values = palette, name = 'Weekend')  +
  scale_y_continuous(label = scales::percent)            +
  xlab(x_label)                                          + 
  ylab(y_label)                                          + 
  ggtitle(title)                                         +
  graphics_theme_1 + theme(legend.position   = "bottom")

#-----------------------------------------------------------------
# Bar plot version two
#-----------------------------------------------------------------
x_label  <- ('\n Season')
y_label  <- ('Ticket Sales \n')
title    <- ('Ticket Sales by DOW')
bar_sales <- 
  ggplot2::ggplot(data     = season_data, 
                  aes(y    = ticketSales,
                      x    = season,
                      fill = weekEnd))                  +
  geom_bar(stat = 'identity', position = 'stack')       +
  scale_fill_manual(values = palette, name = 'Weekend') +
  scale_y_continuous(label = scales::comma)             +
  xlab(x_label)                                         + 
  ylab(y_label)                                         + 
  ggtitle(title)                                        +
  graphics_theme_1 + theme(legend.position   = "bottom")

#-----------------------------------------------------------------
# Creating a summary statistics table
#-----------------------------------------------------------------
library(dplyr)
average_by_dow <- 
  FOSBAAS::season_data                          %>% 
  group_by(dayOfWeek)                           %>% 
  summarise(AverageSales = mean(ticketSales))

#-----------------------------------------------------------------
# Creating a summary statistics table using 'by'
#-----------------------------------------------------------------
by(FOSBAAS::season_data$ticketSales,
   FOSBAAS::season_data$dayOfWeek,function(x) mean(x))

#-----------------------------------------------------------------
# Getting quantiles
#-----------------------------------------------------------------
quants <- quantile(FOSBAAS::season_data$ticketSales, 
                   probs = c(0,.10,.25,.5,.75,.9,1))
quants

#-----------------------------------------------------------------
# Converting to wide format
#-----------------------------------------------------------------
library(dplyr)
library(tidyr)
team_dow <- 
  FOSBAAS::season_data                                   %>%
  select(team,dayOfWeek,ticketSales)                     %>%
  filter(team %in% c('SF','BAL'))                        %>%
  group_by(team,dayOfWeek)                               %>%
  summarise(medianSales = median(ticketSales),
            games       = n())                           %>%
  tidyr::pivot_wider(names_from  = team,
                     values_from = c(medianSales,games)) %>%
  mutate(difference = medianSales_BAL - medianSales_SF)  %>%
  arrange(difference) 

#-----------------------------------------------------------------
# Converting to long format
#-----------------------------------------------------------------
library(dplyr)
library(tidyr)
team_dow_long <- 
  team_dow                                          %>%
  select(dayOfWeek, medianSales_BAL,medianSales_SF) %>%
  tidyr::pivot_longer(!dayOfWeek, 
                      names_to  = "club", 
                      values_to = "medianSales")

#-----------------------------------------------------------------
# Summary stats psych
#-----------------------------------------------------------------
library(psych)
psy_desc <- 
  t(data.frame(psych::describe(FOSBAAS::season_data$ticketSales)))

#-----------------------------------------------------------------
# Summary stats Hmisc
#-----------------------------------------------------------------
hmisc_desc <- (Hmisc::describe(FOSBAAS::season_data$ticketSales))
hmisc_desc <- unlist(hmisc_desc$counts)
hmisc_desc <- as.data.frame(hmisc_desc)

# Unload a package
# unloadNamespace("Hmisc")
# detach("package:Hmisc")

#-----------------------------------------------------------------
# Build a frequency table
#-----------------------------------------------------------------
table(FOSBAAS::season_data$promotion)

#-----------------------------------------------------------------
# One Way ANOVA
#-----------------------------------------------------------------
mod <- aov(ticketSales ~ promotion + dayOfWeek,
           data = FOSBAAS::season_data)
#summary(mod)

#-----------------------------------------------------------------
# Viewing group means
#-----------------------------------------------------------------
library(dplyr)
graph_table <- FOSBAAS::season_data %>%
  
  select(promotion,ticketSales,dayOfWeek,daysSinceLastGame)  %>%
  group_by(promotion,dayOfWeek)                              %>%
  summarise(sales = mean(ticketSales),
            daysSinceLastGame = mean(daysSinceLastGame),
            N = n(),
            sd = sd(ticketSales),
            se = sd/sqrt(N))         

x_label <- 'Day of Week'                                             
y_label <- 'Mean ticket sales'                                            
title   <- 'Group means and standard error: promos and sales'
se <- 
  ggplot(graph_table, aes(y=sales, 
                          x=reorder(dayOfWeek,sales,mean), 
                          color=promotion))                 + 
  geom_errorbar(aes(ymin = sales-se, ymax = sales+se), 
                width =.3,size = 1,
                position = position_dodge(0.25))            +
  geom_point()                                              +
  scale_color_manual(values = palette)                      +
  scale_y_continuous(label = scales::comma)                 +
  xlab(x_label)                                             + 
  ylab(y_label)                                             + 
  ggtitle(title)                                            +
  graphics_theme_1 

#-----------------------------------------------------------------
# Summary stats psych
#-----------------------------------------------------------------
tu_test <- TukeyHSD(mod)

#-----------------------------------------------------------------
# Viewing group means
#-----------------------------------------------------------------

x_label <- 'Value'                                             
y_label <- 'Promotion Comps'                                            
title   <- '95% CI comps by promotion '
tu_comp_graph <- 
  ggplot(tu_comp, aes(x=diff, 
                      y=promotion))                         + 
  geom_errorbar(aes(xmin = lwr, xmax = upr), 
                width =.3,size = 1)                         +
  geom_point()                                              +
  scale_x_continuous(label = scales::comma)                 +
  xlab(x_label)                                             + 
  ylab(y_label)                                             + 
  ggtitle(title)                                            +
  geom_vline(xintercept = 0,color = 'red',lty = 2)          +
  graphics_theme_1 

#-----------------------------------------------------------------
# Creating a simple linear model
#-----------------------------------------------------------------
ln_mod <- lm(ticketSales ~ team+dayOfWeek+month+
             daysSinceLastGame+openingDay+promotion,
             data = FOSBAAS::season_data)
stats_ticket_sales <- 
  tibble::tibble(st_error     = unlist(summary(ln_mod)[6]),
                 r_square     = unlist(summary(ln_mod)[8]),
                 adj_r_square = unlist(summary(ln_mod)[9]),
                 f_stat       = summary(ln_mod)$fstatistic[1])

#-----------------------------------------------------------------
# Using the regression output
#-----------------------------------------------------------------
seasons <- FOSBAAS::season_data
seasons$pred <- predict(ln_mod)

x_label <- 'Actual Sales'                                             
y_label <- 'Predicted Sales'                                            
title   <- 'Actual Sales vs. Predictions'
legend   <- ('Season')
sales_mod <- 
  ggplot(seasons, aes(x = ticketSales, 
                      y = pred,
                      color = factor(season)))              + 
  geom_point()                                              +
  stat_smooth(method = 'lm', se = T)                        +
  scale_color_manual(legend,values = palette)               +
  scale_x_continuous(label = scales::comma)                 +
  scale_y_continuous(label = scales::comma)                 +
  xlab(x_label)                                             + 
  ylab(y_label)                                             + 
  ggtitle(title)                                            +
  graphics_theme_1 

#-----------------------------------------------------------------
# Chapter 4
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Aggregated CRM data
#-----------------------------------------------------------------
ag_sales_data <- FOSBAAS::aggregated_crm_data

library(dplyr)
agg_calls <- 
  ag_sales_data                     %>% 
  group_by(repID)                   %>%
  summarise(calls   = sum(call),
            revenue = sum(revenue)) %>%
  mutate(revByCall = revenue/calls) %>%
  arrange(desc(revByCall))

#-----------------------------------------------------------------
# Correlation coefficient
#-----------------------------------------------------------------
cor(agg_calls$calls,agg_calls$revenue)

#-----------------------------------------------------------------
# Box plots of revenue by sales rep
#-----------------------------------------------------------------
x_label <- 'Rep ID'                                             
y_label <- 'Revenue by sale'                                            
title   <- 'Revenue by sale by rep'
sales_box <- 
  ggplot(ag_sales_data, aes(y=revenue, 
                            x=factor(repID)))               +
  geom_boxplot(fill = palette[1])                           +
  scale_color_manual(values = palette)                      +
  scale_y_continuous(label = scales::dollar)                +
  xlab(x_label)                                             + 
  ylab(y_label)                                             + 
  ggtitle(title)                                            +
  graphics_theme_1                                          +
  theme(axis.text.x  = element_text(angle = 90, size = 8, 
                                    vjust = 0,
                                    color = "grey10"))

#-----------------------------------------------------------------
# Cumulative revenue by rep by customer
#-----------------------------------------------------------------
ag_sales_line <- 
  ag_sales_data %>% group_by(repID) %>%
  mutate(cumSum = cumsum(revenue),
         observation = seq(1:500))

x_label <- 'Customer'                                             
y_label <- 'Revenue'                                            
title   <- 'Revenue generated per rep by customer'

sales_line <- 
  ggplot(ag_sales_line, aes(y     = cumSum, 
                            x     = factor(observation),
                            group = repID,
                            color = repID))                 +
  geom_line()                                               +
  scale_color_manual(values = palette,guide = FALSE)        +
  scale_y_continuous(label = scales::dollar)                +
  xlab(x_label)                                             + 
  ylab(y_label)                                             + 
  ggtitle(title)                                            +
  graphics_theme_1                                          +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major  = element_line(colour = "white"),  
        panel.grid.minor  = element_line(colour = "grey80"))

#-----------------------------------------------------------------
# Failed calls and customers by rep
#-----------------------------------------------------------------
failures <- 
  ag_sales_data                      %>% 
  group_by(repID)                    %>%
  filter(revenue == 0)               %>%
  summarise(failedCalls = sum(call),
            failedCusts = n())       %>%
  tidyr::pivot_longer(!repID, 
                      names_to  = "failure", 
                      values_to = "value")
x_label  <- ('\n Rep')
y_label  <- ('Count \n')
title    <- ('Failures by rep')
bar_sales <- 
  ggplot2::ggplot(data     = failures, 
                  aes(y    = value, 
                      x    = reorder(repID,value,sum),
                      fill = failure))                  +
  geom_bar(stat = 'identity', position = 'dodge')       +
  scale_fill_manual(values = palette, name = 'failure') +
  scale_y_continuous(label = scales::comma)             +
  xlab(x_label)                                         + 
  ylab(y_label)                                         + 
  ggtitle(title)                                        +
  coord_flip()                                          +
  graphics_theme_1                                      + 
  theme(legend.position   = "bottom")

#-----------------------------------------------------------------
# quantiles of sales
#-----------------------------------------------------------------
quants <- quantile(ag_sales_data$revenue,
                   probs = c(.5,.75,.9,.95,.975,.99,1))
quants

#-----------------------------------------------------------------
# quantiles of sales
#-----------------------------------------------------------------
descriptives <- 
psych::describe.by(ag_sales_data[which(ag_sales_data$repID %in% 
                    c("0LK62LATB8E3","AFA0Z9M2M4LQ")),]$revenue,
                   ag_sales_data[which(ag_sales_data$repID %in% 
                    c("0LK62LATB8E3","AFA0Z9M2M4LQ")),]$repID)

#-----------------------------------------------------------------
# description of sales
#-----------------------------------------------------------------
descriptives$`0LK62LATB8E3`[c(3,4,5,6,9)]

#-----------------------------------------------------------------
# description of sales
#-----------------------------------------------------------------
descriptives$AFA0Z9M2M4LQ[c(3,4,5,6,9)]

#-----------------------------------------------------------------
# quantiles of sales
#-----------------------------------------------------------------

ag_sales_data                                          %>% 
  filter(repID %in% c('0LK62LATB8E3','AFA0Z9M2M4LQ'),
         revenue >= quants[6])                         %>%
  group_by(repID)                                      %>%
  summarise(highRevenue  = sum(revenue),
            countRevenue = n())


#-----------------------------------------------------------------
# Clustering algorithm applied in python
#-----------------------------------------------------------------
# from sklearn.cluster import AgglomerativeClustering
# data = data.sample(n=1500)
# cluster = AgglomerativeClustering(n_clusters=6, 
#                                  affinity='euclidean', 
#                                  linkage='ward')  
# cl = py.DataFrame(cluster.fit_predict(data))

#-----------------------------------------------------------------
# kmeans algorithm applied in python
#-----------------------------------------------------------------
# from sklearn.cluster import KMeans
# data = data.sample(n=1500)
# cluster = KMeans(n_clusters=6, random_state=0)
# cl = py.DataFrame(cluster.fit_predict(data))

#-----------------------------------------------------------------
# Hierarchical clustering algorithm applied in R
#-----------------------------------------------------------------
library(stats)
library(cluster)
data         <- sample(data, 1500)
mod_data     <- cluster::daisy(data)
cl           <- stats::hclust(mod_data, method = "ward.D")
cuts         <- cutree(cl, k = 6)
data$cluster <- cuts

#-----------------------------------------------------------------
# k means algorithm applied in R
#-----------------------------------------------------------------
library(stats)
data         <- sample(data, 1500)
cl           <- stats::kmeans(data, centers = 6)
data$cluster <- cl$cluster

#-----------------------------------------------------------------
# Batch script for automation
#-----------------------------------------------------------------
# REM This command will call an R Script from a program or 
# scheduling tool

# "C:\Program Files\R\R-Version\bin\x64\R.exe" CMD BATCH 
#  --vanilla --slave ""C:\locationOfScript\YourRScript.R"

#-----------------------------------------------------------------
# Chapter 5
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Read data to use for segmentation
#-----------------------------------------------------------------
library(FOSBAAS)
demo_data  <- FOSBAAS::demographic_data

#-----------------------------------------------------------------
# Take a look at the structure of the data
#-----------------------------------------------------------------
str(demo_data)

#-----------------------------------------------------------------
# subset the data
#-----------------------------------------------------------------
library(dplyr)
demo_data <- demo_data %>% select(custID,ethnicity,age,
                                  maritalStatus,children,
                                  hhIncome,distance,gender)

#-----------------------------------------------------------------
# Histogram of the age of fans
#-----------------------------------------------------------------
x_label  <- ('\n Age')
y_label  <- ('Count \n')
title    <- ('Distribution of age (demographic)')
hist_age <- 
  ggplot2::ggplot(data=demo_data,aes(x=age,fill=gender))  +
  geom_histogram(binwidth = 2)                            +
  scale_fill_manual(values = palette)                     +
  geom_rug(color = 'coral')                               +
  scale_x_continuous(label = scales::comma)               +
  scale_y_continuous(label = scales::comma)               +
  xlab(x_label)                                           + 
  ylab(y_label)                                           + 
  ggtitle(title)                                          +
  graphics_theme_1

#-----------------------------------------------------------------
# Get summary statistics
#-----------------------------------------------------------------
library(psych)
descript   <- psych::describe(demo_data$age)
descriptMF <- psych::describeBy(demo_data$age,demo_data$gender)

#-----------------------------------------------------------------
# Distance from our facility
#-----------------------------------------------------------------
library(ggplot2)
x_label   <- ('\n Distance')
y_label   <- ('Count \n')
title     <- ('Distribution of distance (demographic)')
hist_dist <- 
  ggplot2::ggplot(data=demo_data,aes(x=distance))            +
  geom_histogram(binwidth = 2,fill='dodgerblue')             +
  geom_rug(color = 'coral')                                  +
  scale_x_continuous(label = scales::comma)                  +
  scale_y_continuous(label = scales::comma)                  +
  xlab(x_label)                                              + 
  ylab(y_label)                                              + 
  ggtitle(title)                                             +
  graphics_theme_1

#-----------------------------------------------------------------
# Use the psych package to generate summary statistics
#-----------------------------------------------------------------
descript <- psych::describe(demo_data$distance)

#-----------------------------------------------------------------
# Histogram of income
#-----------------------------------------------------------------
x_label  <- ('\n Household Income')
y_label  <- ('Count \n')
title    <- ('Distribution of Income (demographic)')
hist_income <- 
  ggplot2::ggplot(data=demo_data,aes(x=hhIncome)) +
  geom_histogram(binwidth = 2,fill='dodgerblue')  +
  geom_rug(color = 'coral')                       +
  scale_x_continuous(label = scales::comma)       +
  scale_y_continuous(label = scales::comma)       +
  xlab(x_label)                                   + 
  ylab(y_label)                                   + 
  ggtitle(title)                                  +
  graphics_theme_1

#-----------------------------------------------------------------
# summary statistics of hhincome
#-----------------------------------------------------------------
description <- psych::describe(demo_data$hhIncome)

#-----------------------------------------------------------------
# Generate summary stats of multiple values
#-----------------------------------------------------------------
numerical_cols <- names(dplyr::select_if(demo_data, is.numeric))
description    <- psych::describe(demo_data[numerical_cols])

#-----------------------------------------------------------------
# build proportion table for categorical data
#-----------------------------------------------------------------
description      <- table(demo_data$ethnicity)
description_prop <- prop.table(description)

#-----------------------------------------------------------------
# use describeBy to generate statistics tabulated by a factor
#-----------------------------------------------------------------
numerical_cols <- names(dplyr::select_if(demo_data, is.numeric))
description    <- psych::describeBy(demo_data[numerical_cols], 
                                    demo_data$ethnicity)
deth <- rbind(description$aa[,c(2,3,4,5,6)], 
              description$w[,c(2,3,4,5,6)])

#-----------------------------------------------------------------
# Write a function to generate stats on missing data
#-----------------------------------------------------------------
f_missing_data <- function(dF){
  
  na  <- sum(apply(dF,2,function(x) is.na(x)))
  nan <- sum(apply(dF,2,function(x) is.nan(x)))
  inf <- sum(apply(dF,2,function(x) is.infinite(x)))
  
  missing        <- as.data.frame(rbind(na,nan,inf))
  names(missing) <- 'Values'
  return(missing)
}

missing <- f_missing_data(demo_data)

#-----------------------------------------------------------------
# Randomly make some values in data frame NA
#-----------------------------------------------------------------
set.seed(755)
demo_data[] <- 
 apply(demo_data,1:2, function(x) ifelse(runif(1,0,1) > .98,NA,x))

#-----------------------------------------------------------------
# Randomly make some values in dataframe NA
#-----------------------------------------------------------------
missing <- f_missing_data(demo_data)

#-----------------------------------------------------------------
# Print the number of missing values for each column
#-----------------------------------------------------------------
for(i in names(demo_data)){
  print(paste(sum(is.na(demo_data[,i])),names(demo_data[i])))
}

#-----------------------------------------------------------------
# Only keep rows with no NAs in any column
#-----------------------------------------------------------------
complete_cases <- nrow(demo_data[complete.cases(demo_data),])

#-----------------------------------------------------------------
# Simple imputation. Make any NAs 'w'
#-----------------------------------------------------------------
demo_data$ethnicity[is.na(demo_data$ethnicity)] <- 'w'

#-----------------------------------------------------------------
# Make an y missing age values the mean of overall age
#-----------------------------------------------------------------
mA <- 
  round(mean(as.numeric(as.character(na.omit(demo_data$age)))),0)
demo_data$age[is.na(demo_data$age)] <- mA

#-----------------------------------------------------------------
# Impute values based on a proportion
#-----------------------------------------------------------------
l <- 
  length(demo_data$maritalStatus[is.na(demo_data$maritalStatus)])
demo_data$maritalStatus[is.na(demo_data$maritalStatus)] <- 
  sample(c('m','s'), size=l, prob=c(.6,.4), replace=TRUE)

#-----------------------------------------------------------------
# Impute remaining values based on a proportion
#-----------------------------------------------------------------
#prop.table(table(demo_data$children))
#prop.table(table(demo_data$gender))

l <- length(demo_data$children[is.na(demo_data$children)])
demo_data$children[is.na(demo_data$children)] <- 
  sample(c('n','y'), size=l, prob=c(.52,.48), replace=TRUE)

l <- length(demo_data$gender[is.na(demo_data$gender)])
demo_data$gender[is.na(demo_data$gender)] <- 
  sample(c('m','f'), size=l, prob=c(.5,.5), replace=TRUE)

#-----------------------------------------------------------------
# Impute numerical values using the mice package
#-----------------------------------------------------------------
library(mice)
demo_data$distance <- as.numeric(demo_data$distance)
imp_frame          <- as.data.frame(cbind(demo_data$hhIncome,
                                          demo_data$distance))
imp_frame[]        <- apply(imp_frame,2,function(x) as.numeric(x))

imputed_Data       <- mice(imp_frame, m=5, maxit = 10,
                           method = 'pmm', seed = 755,
                           printFlag = F)
new_Data <- complete(imputed_Data,2)
demo_data$hhIncome <- new_Data$V1
demo_data$distance <- new_Data$V2

#-----------------------------------------------------------------
# Remove any NA values
#-----------------------------------------------------------------
demo_data <- na.omit(demo_data)

#-----------------------------------------------------------------
# Look for NA values
#-----------------------------------------------------------------
missing <- f_missing_data(demo_data)

#-----------------------------------------------------------------
# Create a new data frame to work with
#-----------------------------------------------------------------
demo_data_discrete <- tibble(custID = demo_data$custID)

#-----------------------------------------------------------------
# Create a new dataframe to work with
#-----------------------------------------------------------------
demo_data_discrete <- as.data.frame(demo_data_discrete)

#-----------------------------------------------------------------
# Calculate birth year and assign to a generation
#-----------------------------------------------------------------
library(lubridate)
birthYear <- lubridate::year(Sys.Date()) - 
  as.numeric(demo_data$age)

f_seg_generation_def <- function(birth_year){
  
  if(birth_year >= 1910 & birth_year <= 1924){'gen_Greatest'}
  else if (birth_year >= 1925 & birth_year <= 1945){'gen_Silent'}
  else if (birth_year >= 1946 & birth_year <= 1964){'gen_Boomer'}    
  else if (birth_year >= 1965 & birth_year <= 1979){'gen_X'}     
  else if (birth_year >= 1980 & birth_year <= 1994){'gen_Mill'}       
  else if (birth_year >= 1995 & birth_year <= 2012){'gen_Y'}    
  else if (birth_year >= 2013 & birth_year <= 2020){'gen_Alpha'}      
  else{'Unknown'}
}
demo_data_discrete$generation <- 
  sapply(birthYear,f_seg_generation_def)

#-----------------------------------------------------------------
# Assign a discrete value to distance
#-----------------------------------------------------------------
quantile_list <- quantile(demo_data$distance,
                          probs = c(.1,.2,.3,.4,.5,.6,.7,.8,.9))

f_seg_dist_def <- function(dist,q){
  
  if(dist <= 30.67){'dist_Primary'}
  else if (dist > 30.67 & dist <= 51.23){'dist_Secondary'}
  else if (dist > 41.23 & dist <= 106.07){'dist_Tertiary'}    
  else{'dist_Quaternary'}        
}

demo_data_discrete$marketLoc <- 
  sapply(demo_data$distance,f_seg_dist_def)

#-----------------------------------------------------------------
# Assign a discrete value to household Income
#-----------------------------------------------------------------
hhIncomeQuantiles <- quantile(demo_data$hhIncome,
                              probs = c(.25,.75))

f_seg_hhInc_def <- function(hhInc, quant = hhIncomeQuantiles){
  
  if(hhInc <= hhIncomeQuantiles[[1]]){"income_low"}
  else if(hhInc >= hhIncomeQuantiles[[2]]){"income_high"}
  else{"income_med"}
}

demo_data_discrete$income <-
  sapply(demo_data$hhIncome,f_seg_hhInc_def)

#-----------------------------------------------------------------
# Add columns to demo_data
#-----------------------------------------------------------------
demo_data_discrete <- demo_data_discrete    %>% 
  mutate(ethnicity = demo_data$ethnicity,
         married   = demo_data$maritalStatus,
         gender    = demo_data$gender)
demo_data_discrete <- as.data.frame(demo_data_discrete)

#-----------------------------------------------------------------
# Dummy code (One Hot encoding) for model consumption
#-----------------------------------------------------------------
dummy_coded_vars <- 
  apply(demo_data_discrete[,2:7],2, 
        function(x) psych::dummy.code(factor(x)))

mod_data_discrete <- 
  cbind.data.frame(dummy_coded_vars[1:length(dummy_coded_vars)])

row.names(mod_data_discrete) <- demo_data_discrete$custID

#-----------------------------------------------------------------
# Combine data frames
#-----------------------------------------------------------------
mod_data_numeric <- demo_data[,c('age','distance','hhIncome')]
mod_data_numeric <- cbind.data.frame(mod_data_numeric,
                                     mod_data_discrete[,13:20]) 

#-----------------------------------------------------------------
# Prepared data for hierarchical clustering
#-----------------------------------------------------------------
library(dplyr)
library(cluster)
set.seed(755)
mod_data_samp     <- mod_data_numeric %>% dplyr::sample_n(25)
mod_data_samp$age <- as.numeric(mod_data_samp$age)
# Create dissimilarity matrix
mod_dist <- cluster::daisy(mod_data_samp)

#-----------------------------------------------------------------
# Apply heirarchical clustering method
#-----------------------------------------------------------------
mod_HC <- stats::hclust(mod_dist, method = "centroid")

#-----------------------------------------------------------------
# Apply hierarchical clustering method
#-----------------------------------------------------------------
plot(mod_HC)

#-----------------------------------------------------------------
# Latent Class regression
#-----------------------------------------------------------------
mod_data_LC   <- mod_data_discrete
mod_data_LC[] <- apply(mod_data_discrete, 2, 
                       function(x) ifelse(x == 1 ,1 ,2))

#-----------------------------------------------------------------
# Latent Class regression formula
#-----------------------------------------------------------------
format  <- paste(names(mod_data_LC), collapse = ",")
formula <- 
  with(mod_data_LC,
       cbind(generation.gen_X,generation.gen_Mill,
             generation.gen_Boomer,generation.gen_Y,
             generation.gen_Silent,marketLoc.dist_Quaternary,
             marketLoc.dist_Tertiary,marketLoc.dist_Secondary,
             marketLoc.dist_Primary,income.income_med,
             income.income_low,income.income_high,
             ethnicity.w,ethnicity.aa,ethnicity.h,
             ethnicity.a,married.m,married.s,gender.f,
             gender.m)~1)

#-----------------------------------------------------------------
# Latent Class regression
#-----------------------------------------------------------------
require(poLCA)
set.seed(363)
seg_LCR_5 <- poLCA::poLCA(formula, data = mod_data_LC, 
                          nclass = 5, verbose = F)

#save(seg_LCR_5, file="CH5_LCR_5_Model.Rdata")

#-----------------------------------------------------------------
# Evaluating the results
#-----------------------------------------------------------------
table(seg_LCR_5$predclass)

mod_results          <- 
  as.data.frame(matrix(nrow = nrow(mod_data_LC), ncol = 0))
mod_results$custID   <- row.names(mod_data_LC)
mod_results$lcrClass <- seg_LCR_5$predclass

#-----------------------------------------------------------------
# Attaching to the data frame
#-----------------------------------------------------------------
demo_data_segments <- 
  dplyr::left_join(demo_data,mod_results, by = "custID")

#-----------------------------------------------------------------
# Attaching to the data frame
#-----------------------------------------------------------------
demo_data_segments <- as.data.frame(demo_data_segments)

#-----------------------------------------------------------------
# Distance cross tab
#-----------------------------------------------------------------
library(dplyr)
demo_seg_distance <- demo_data_segments %>%
  group_by(lcrClass)                    %>% 
  summarise(avgDist = mean(distance),
            medDist = median(distance))

#-----------------------------------------------------------------
# Age cross tab
#-----------------------------------------------------------------
demo_data_segments$age <- as.numeric(demo_data_segments$age)

demo_seg_age <- demo_data_segments %>%
  group_by(lcrClass)               %>% 
  summarise(avgAge = mean(age),
            medAge = median(age))

#-----------------------------------------------------------------
# Household income cross tab
#-----------------------------------------------------------------
demo_seg_HHI <- demo_data_segments     %>%
  group_by(lcrClass)                   %>% 
  summarise(avgHHI = mean(hhIncome),
            medHHI = median(hhIncome))

#-----------------------------------------------------------------
# Gender cross tab
#-----------------------------------------------------------------
demo_seg_gender <- 
  demo_data_segments         %>%
  group_by(lcrClass)         %>% 
  count(gender)              %>%
  tidyr::pivot_wider(names_from = gender, 
                     values_from = n)

#-----------------------------------------------------------------
# Ethnicity tab
#-----------------------------------------------------------------
demo_seg_ethnicity <- 
  demo_data_segments    %>%
  group_by(lcrClass)    %>% 
  count(ethnicity)      %>%
  tidyr::pivot_wider(names_from  = ethnicity, 
                     values_from = n)

#-----------------------------------------------------------------
#  Visualize ethnicity
#-----------------------------------------------------------------
library(ggplot2)
demo_seg_ethnicity_l <- 
  demo_data_segments %>%
  group_by(lcrClass) %>% 
  count(ethnicity)      

demo_seg_ethnicity_l <- as.data.frame(demo_seg_ethnicity_l)

x_label  <- ('\n Ethnicity')
y_label  <- ('Class \n')
title   <- ('Ethnicity by class')
tile_segment <- ggplot2::ggplot(data   = demo_seg_ethnicity_l, 
                                aes(x    = ethnicity,
                                    y    = lcrClass,
                                    fill = log(n)))             +
  geom_tile()                                                   +
  scale_fill_gradient(low="white", high="dodgerblue",
                      name = 'log(Fans)',label = scales::comma) +
  xlab(x_label)                                                 + 
  ylab(y_label)                                                 + 
  ggtitle(title)                                                +
  graphics_theme_1

#-----------------------------------------------------------------
# access the survey data
#-----------------------------------------------------------------
survey <- FOSBAAS::fa_survey_data

#-----------------------------------------------------------------
# scale the numeric data
#-----------------------------------------------------------------
survey_sc <- scale(survey[,2:26])
survey_sc <- as.data.frame(survey_sc)

#-----------------------------------------------------------------
# access the survey data
#-----------------------------------------------------------------
nFacts <- nFactors::nScree(survey_sc)

#-----------------------------------------------------------------
# access the survey data
#-----------------------------------------------------------------
nFacts <- as.data.frame(unlist(nFacts$Components))
names(nFacts) <- 'Factors'
knitr::kable(nFacts,caption = "Number of Factors")

#-----------------------------------------------------------------
# run a factor analysis
#-----------------------------------------------------------------
library(GPArotation)
survey_sc_ob <- factanal(survey_sc,
                         factors  = 5,
                         rotation = "oblimin",
                         scores   = "Bartlett")

#-----------------------------------------------------------------
# Restructure the data
#-----------------------------------------------------------------
survey_sc_ob_scores <- as.data.frame(unlist(survey_sc_ob$scores))
survey_sc_ob_scores$Reason <- survey$ReasonForAttending

fa_data <- unclass(survey_sc_ob$loadings)
fa_data  <- as.data.frame(fa_data)
fa_data$Selection <- row.names(fa_data)

library(tidyr)
fa_data_p <-  
  fa_data %>% tidyr::pivot_longer(!Selection, 
                                  names_to  = "Factor", 
                                  values_to = "Loading")

fa_data_p$Order <- 
  reorder(fa_data_p$Selection,fa_data_p$Loading,sum)

#-----------------------------------------------------------------
# Visualize the factors
#-----------------------------------------------------------------
require(ggplot2)
factor_table <- 
  ggplot(fa_data_p, aes(Factor, Order))                     +
  geom_tile(aes(fill = Loading))                            + 
  geom_text(aes(label = round(Loading, 1)),color='grey40')  +
  scale_fill_gradient(low      = "white", 
                      high     = "dodgerblue", 
                      space    = "Lab",
                      na.value = "grey50", 
                      guide    = "colourbar")               +
  xlab('\n Factor')                                         + 
  ylab('Activity\n')                                        + 
  ggtitle('What do fans do at games? \n')                   +
  graphics_theme_1                                          +
  theme(legend.position="none")                             +
  theme(axis.text.y = element_text(angle = 0, 
                                   size = 8, vjust = 0, 
                                   color = "grey10"))

#-----------------------------------------------------------------
# predict classes
#-----------------------------------------------------------------
library(nnet)
mod_glm <-   nnet::multinom(Reason ~ .,
                            data = survey_sc_ob_scores,
                            linout = FALSE)

pred_survey_sc_ob_scores <- predict(mod_glm , 
                                    newdata=survey_sc_ob_scores,
                                    type='class')

tile_data <- 
  as.data.frame(table(survey_sc_ob_scores$Reason,
                      pred_survey_sc_ob_scores))

#-----------------------------------------------------------------
# Visualize the factors
#-----------------------------------------------------------------
require(ggplot2)

x_label  <- ('\n Actual response')
y_label  <- ('Attend predict \n')
title   <- ('Prediction of attendance reason')

tile_class <- ggplot2::ggplot(data = hex_data, 
                              aes(x    = Var1,
                                  y    = pred_survey_sc_ob_scores,
                                  fill = Freq))        +
  geom_tile()                               +
  scale_fill_gradient(low = "dodgerblue", high = "coral",
                      name = 'Count',label = scales::comma)   +
  xlab(x_label)                                               + 
  ylab(y_label)                                               + 
  ggtitle(title)                                              +
  graphics_theme_1                                            +
  theme(axis.text.x = element_text(angle = 90, 
                                   size = 8, vjust = 0, 
                                   color = "grey10"),
        axis.text.y = element_text(angle = 0, 
                                   size = 8, vjust = 0, 
                                   color = "grey10"))

#-----------------------------------------------------------------
# Apply classes to individuals
#-----------------------------------------------------------------
ids <- sample(demo_data_segments$custID,10000)
survey_sc_ob_scores$Class <- 
  colnames(survey_sc_ob_scores)[max.col(survey_sc_ob_scores[,1:5],
                                        ties.method="first")]
survey_sc_ob_scores$custID <- ids
combined_classes <- dplyr::left_join(demo_data_segments,
                                     survey_sc_ob_scores,
                                     by="custID")         %>%
                                     na.omit()
combined_classes <- as.data.frame(combined_classes)

#-----------------------------------------------------------------
# recode the factors and classes
#-----------------------------------------------------------------
combined_classes$f_seg_name <- 
  sapply(combined_classes$Class, 
         function(x) switch(x,"Factor1" = "Avid fan",
                              "Factor2" = "Socializers",
                              "Factor3" = "Foodies",
                              "Factor4" = "Parkies",
                              "Factor5" = "Strangers"))

combined_classes$d_seg_name <- 
  sapply(combined_classes$lcrClass, 
         function(x) switch(x,'1' = "Young and Broke",
                              '2' = "Marty Male",
                              '3' = "Fionna Female",
                              '4' = "Diffuse Diane",
                              '5' = "Nearby Ned"))

#-----------------------------------------------------------------
# Visualize the results
#-----------------------------------------------------------------
cc <- combined_classes                %>%
      group_by(f_seg_name,d_seg_name) %>% 
      count()      

x_label  <- ('\n Factor Segments')
y_label  <- ('Demographic Segment \n')
title   <- ('Segment Comparrison')
tile_segment <- 
  ggplot2::ggplot(data = cc, 
                  aes(x = f_seg_name,
                      y = d_seg_name,
                      fill = n))                              +
  geom_tile()                                                 +
  scale_fill_gradient(low="white", high="dodgerblue",
                      name = 'Count',label = scales::comma)   +
  xlab(x_label)                                               + 
  ylab(y_label)                                               + 
  ggtitle(title)                                              +
  graphics_theme_1                                            +
  theme(axis.text.x = element_text(angle = 0, 
                                   size = 8, vjust = 0, 
                                   color = "grey10"),
        axis.text.y = element_text(angle = 0, 
                                   size = 8, vjust = 0, 
                                   color = "grey10"))

#-----------------------------------------------------------------
# Scale the results and average
#-----------------------------------------------------------------
combined_classes$age_s <- 
  scale(as.numeric(combined_classes$age))
combined_classes$distance_s <- 
  scale(as.numeric(combined_classes$distance))

cc <- 
  combined_classes                %>%
  group_by(f_seg_name,d_seg_name) %>% 
  summarise(age = mean(age_s), 
            distance = mean(distance_s)) 

#-----------------------------------------------------------------
# Observe differences between the segments
#-----------------------------------------------------------------
x_label  <- ('\n Scaled Avg Age')
y_label  <- ('Scaled Avg Distance \n')
title    <- ('Segments by distance and age')

point_segment <- 
  ggplot2::ggplot(data      = cc, 
                  aes(x     = age,
                      y     = distance,
                      color = f_seg_name,
                      shape = d_seg_name))                +
  geom_point(size = 3)                                    +
  scale_color_manual(values = palette)                    +
  geom_hline(yintercept = median(cc$distance), lty = 2)   +
  geom_vline(xintercept = median(cc$age), lty = 2)        +
  xlab(x_label)                                           + 
  ylab(y_label)                                           + 
  ggtitle(title)                                          +
  graphics_theme_1

#-----------------------------------------------------------------
# Chapter 6
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# The Price response function
#-----------------------------------------------------------------
library(tidyverse)
# Build a simple data set
sales <- tibble::tibble(
  sales = c(20,30,35,43,35,8,2,0),
  price = c(25,28,29,30,31,34,35,36)
)

x_label  <- ('\n Price')
y_label  <- ('Ticket Sales \n')
title    <- ('Ticket Sales by Price')
line_sales <- 
  ggplot2::ggplot(data  = sales, 
                  aes(x = price,
                      y = sales))                 +
  geom_point(size = 2.5,color = 'mediumseagreen') +
  scale_x_continuous(label = scales::dollar)      +
  xlab(x_label)                                   + 
  ylab(y_label)                                   + 
  ggtitle(title)                                  +
  graphics_theme_1                                +
  geom_line(color = "mediumseagreen")             +
  geom_smooth(method = 'lm') 

#-----------------------------------------------------------------
# The Price response function
#-----------------------------------------------------------------
x_label  <- ('\n Price')
y_label  <- ('Ticket Sales \n')
title    <- ('Ticket Sales by Price')
line_sales_poly <- 
  ggplot2::ggplot(data = sales, 
                  aes(x = price,
                      y = sales))                 +
  geom_point(size = 2.5,color = 'mediumseagreen') +
  scale_x_continuous(label = scales::dollar)      +
  xlab(x_label)                                   + 
  ylab(y_label)                                   + 
  ggtitle(title)                                  +
  graphics_theme_1                                +
  geom_line(color = "mediumseagreen")             +          
  stat_smooth(method = "lm",color = 'coral', 
              formula = y ~ x + poly(x, 2)-1) 

#-----------------------------------------------------------------
# Function to return sales based on price
#-----------------------------------------------------------------
fit <- lm(sales$sales~poly(sales$price,2,raw=TRUE))

f_get_sales <- function(new_price){
  sales <- coef(fit)[1] + 
    (coef(fit)[2]*new_price + (coef(fit)[3] * new_price^2))
  return(sales)
}

#-----------------------------------------------------------------
# Use f_get_sales to get modeled demand at each price level
#-----------------------------------------------------------------
old_prices      <- c(25,28,29,30,31,34,35,36)
estimated_sales <- sapply(old_prices, function(x) f_get_sales(x))

#-----------------------------------------------------------------
# Use f_get_sales to get modeled demand at each price level
#-----------------------------------------------------------------
estimated_sales_new <- f_get_sales(26)

#-----------------------------------------------------------------
# Demonstrate the highest point on the curve
#-----------------------------------------------------------------
x_label  <- ('\n Price')
y_label  <- ('Ticket Sales \n')
title   <- ('Ticket Sales by Price')
line_sales_polyb <- 
  ggplot2::ggplot(data  = sales, 
                  aes(x = price,
                      y = sales))                     +
  geom_point(size = 2.5,color = 'mediumseagreen')     +
  scale_x_continuous(label = scales::dollar)          +
  xlab(x_label)                                       + 
  ylab(y_label)                                       + 
  ggtitle(title)                                      +
  graphics_theme_1                                    +
  geom_line(color = "mediumseagreen")                 +                                         
  stat_smooth(method = "lm", color = 'coral',
              formula = y ~ x + poly(x, 2)-1, se = F) +
  geom_hline(yintercept = 35.8151054, lty = 2)        +
  geom_vline(xintercept = 29.21, lty = 2)

#-----------------------------------------------------------------
# Look for optimum price levels
#-----------------------------------------------------------------
estimated_prices <- seq(from = 25, to = 35, by = 1)
estimated_sales <- sapply(estimated_prices,
                          function(x) f_get_sales(x))

estimated_revenue <- tibble::tibble(
  
  sales      = estimated_sales,
  price      = estimated_prices,
  revenue    = estimated_sales * estimated_prices,
  totalSales = rev(cumsum(rev(sales)))
)

#-----------------------------------------------------------------
# Look for optimum price levels
#-----------------------------------------------------------------
x <- 1
revenue <- list()
while(x <= nrow(estimated_revenue)){
  
  revenue[x] <- estimated_revenue[x,2] * estimated_revenue[x,4]
  x <- x + 1
}
estimated_revenue$totalRevenue <- unlist(revenue)

#-----------------------------------------------------------------
# Generate past seasons data
#-----------------------------------------------------------------
season_2022 <- 
  FOSBAAS::f_build_season(seed1 = 3000, season_year = 2022,
    seed2 = 714, num_games = 81, seed3 = 366, num_bbh = 5,
    num_con = 3, num_oth = 5, seed4 = 309, seed5  = 25,
    mean_sales = 29000, sd_sales = 3500
  )

season_2023 <- 
  FOSBAAS::f_build_season(seed1 = 755, season_year = 2023,
    seed2 = 4256, num_games = 81, seed3 = 54, num_bbh = 6,
    num_con = 4, num_oth = 7, seed4 = 309, seed5  = 25,
    mean_sales = 30500, sd_sales = 3000
  )

season_2024 <- 
  FOSBAAS::f_build_season(seed1 = 2892, season_year = 2024,
    seed2 = 714, num_games = 81, seed3 = 366, num_bbh = 9,
    num_con = 2, num_oth = 6, seed4 = 6856, seed5  = 2892,
    mean_sales = 32300, sd_sales = 2900
  )

past_season <- rbind(season_2022,season_2023,season_2024)

#-----------------------------------------------------------------
# Build test and training set
#-----------------------------------------------------------------
samp <- round(0.05 * nrow(past_season),0)

set.seed(715)
rows  <- sample(seq_len(nrow(past_season)), 
                size = samp)
train <- past_season[-rows, ]
test  <- past_season[rows, ]

#-----------------------------------------------------------------
# Linear model for ticket sales
#-----------------------------------------------------------------
ln_mod_bu <- lm(ticketSales ~ promotion + daysSinceLastGame + 
                  schoolInOut + weekEnd + team , data = train)
ln_mod_bu_sum <- 
  tibble::tibble(
    st_error     = unlist(summary(ln_mod_bu)[6]),
    r_square     = unlist(summary(ln_mod_bu)[8]),
    adj_r_square = unlist(summary(ln_mod_bu)[9]),
    f_stat       = unlist(summary(ln_mod_bu)$fstatistic[1]))

#-----------------------------------------------------------------
# Regression summary
#-----------------------------------------------------------------
summary(ln_mod_bu)

#-----------------------------------------------------------------
# Outliers test
#-----------------------------------------------------------------
library(car)
outliers <- car::outlierTest(ln_mod_bu)

#-----------------------------------------------------------------
# QQPlot
#-----------------------------------------------------------------
qq <- car::qqPlot(ln_mod_bu, main="QQ Plot")

#-----------------------------------------------------------------
# Influence plot
#-----------------------------------------------------------------
ip <- 
  car::influencePlot(ln_mod_bu, id.method="identify", 
                     main="Influence plot for our linear model ")

#-----------------------------------------------------------------
#  Remove outliers and view summary
#-----------------------------------------------------------------
past_season_clean <- past_season[-c(37,48,53,129,142,143),]

ln_mod_clean <- lm(ticketSales ~ promotion + daysSinceLastGame + 
                     schoolInOut + weekEnd + team , 
                   data = past_season_clean)
#-----------------------------------------------------------------
# Save our model
# save(ln_mod_clean, file="ch6_ln_mod_clean.rda")
#-----------------------------------------------------------------
ln_mod_clean_sum <- 
  tibble::tibble(
    st_error     = unlist(summary(ln_mod_clean)[6]),
    r_square     = unlist(summary(ln_mod_clean)[8]),
    adj_r_square = unlist(summary(ln_mod_clean)[9]),
    f_stat       = unlist(summary(ln_mod_clean)$fstatistic[1]))

#-----------------------------------------------------------------
# Transforming the data
#-----------------------------------------------------------------
summary(pw_mod <- car::powerTransform(ln_mod_clean))

#-----------------------------------------------------------------
# Directly transforming the data
#-----------------------------------------------------------------
ln_mod_log <- 
  lm(log(ticketSales) ~ promotion + daysSinceLastGame + 
       schoolInOut + weekEnd + team , 
     data = past_season_clean)
ln_mod_log_sum <- 
  tibble::tibble(
    st_error     = unlist(summary(ln_mod_log)[6]),
    r_square     = unlist(summary(ln_mod_log)[8]),
    adj_r_square = unlist(summary(ln_mod_log)[9]),
    f_stat       = unlist(summary(ln_mod_log)$fstatistic[1])
  )

#-----------------------------------------------------------------
# Apply predictions to our test data set. 
#-----------------------------------------------------------------
test$pred_tickets <- predict(ln_mod_clean,
                             newdata = test)
test$percDiff <- 
  (test$ticketSales - test$pred_tickets)/test$ticketSales

mean_test <- mean(test$percDiff)

#-----------------------------------------------------------------
# Apply predictions to our test data set. 
#-----------------------------------------------------------------
test_line <- test
test_line$order <- seq(1:nrow(test))
test_line <-   tidyr::pivot_longer(test_line,
                                   cols = c('ticketSales','pred_tickets'),
                                   values_transform = list(val = as.character))
x_label  <- ('\n Selected Game')
y_label  <- ('Ticket Sales \n')
title   <- ('Ticket forecasts vs. Actuals')
line_est <- 
  ggplot2::ggplot(data      = test_line, 
                  aes(x     = order,
                      y     = value,
                      color = name))             +
  geom_point(size = 2.5)                         +
  geom_line()                                    +
  scale_color_manual(values = palette)           +
  scale_y_continuous(label = scales::comma)      +
  xlab(x_label)                                  + 
  ylab(y_label)                                  + 
  ggtitle(title)                                 +
  graphics_theme_1

#-----------------------------------------------------------------
# Secondary market data, manifest, and sales data
#-----------------------------------------------------------------
sm  <- FOSBAAS::secondary_data
man <- FOSBAAS::manifest_data
sea <- FOSBAAS::season_data
sea$gameID <- seq(1:nrow(sea))

#-----------------------------------------------------------------
# Secondary sales by section
#-----------------------------------------------------------------
sm_man <- left_join(sm,man, by = 'seatID')

avg_price_comps <- 
  sm_man %>% dplyr::select(gameID,price,singlePrice,tickets) %>%
  na.omit()                                                  %>%
  dplyr::group_by(gameID)                                    %>%
  dplyr::summarise(meanSec   = mean(price),
                   meanPri   = mean(singlePrice),
                   tickets   = sum(tickets))

#-----------------------------------------------------------------
# Adjust secondary to reflect primary
#-----------------------------------------------------------------
sea_adj  <- left_join(sea,avg_price_comps, by = "gameID") 
adj_coef <- scale(sea_adj$ticketSales)
sea_adj$meanSecAdj <- sea_adj$meanSec + (adj_coef * 10)

#-----------------------------------------------------------------
# Adjust secondary to reflect primary
#-----------------------------------------------------------------
ln_mod_sec <- 
  lm(meanSecAdj ~ promotion + daysSinceLastGame + 
     schoolInOut + weekEnd + team, 
     data = sea_adj)
ln_mod_sec_sum <- 
  tibble::tibble(
    st_error     = unlist(summary(ln_mod_sec)[6]),
    r_square     = unlist(summary(ln_mod_sec)[8]),
    adj_r_square = unlist(summary(ln_mod_sec)[9]),
    f_stat       = unlist(summary(ln_mod_sec)$fstatistic[1])
  )

#-----------------------------------------------------------------
# Create data for a new season
#-----------------------------------------------------------------
season_2025 <- 
  FOSBAAS::f_build_season(seed1 = 755, season_year = 2025,
    seed2 = 714, num_games = 81, seed3 = 366, num_bbh = 5,
    num_con = 3, num_oth = 7, seed4 = 366, seed5  = 1,
    mean_sales = 0, sd_sales = 0
  )

#-----------------------------------------------------------------
# Apply model output to new data set
#-----------------------------------------------------------------
season_2025$predTickets <- predict(ln_mod_clean,
                                   newdata = season_2025)
season_2025$predPrices  <- predict(ln_mod_sec,
                                   newdata = season_2025)

#-----------------------------------------------------------------
# Build event scores
#-----------------------------------------------------------------
season_2025$eventScoreA <- 
  as.vector(scale(season_2025$predTickets) * 100)
season_2025$eventScoreB <- 
  as.vector(scale(season_2025$predPrices) * 100)
season_2025$eventScore  <- 
  season_2025$eventScoreA + season_2025$eventScoreB

season_2025 <- season_2025[order(-season_2025$eventScore),]

#-----------------------------------------------------------------
# Observe differences in event scores
#-----------------------------------------------------------------
library(ggplot2)
season_2025$order <- seq(1:nrow(season_2025))

x_label  <- ('\n Game')
y_label  <- ('Event Scores \n')
title   <- ('Ordered event scores')
line_est_es <- 
  ggplot2::ggplot(data  = season_2025, 
                  aes(x = order,
                      y = eventScore))             +
  geom_point(size = 1.3,color = 'dodgerblue')      +
  geom_line()                                      +
  scale_color_manual(values = palette)             +
  scale_y_continuous(label = scales::comma)        +
  xlab(x_label)                                    + 
  ylab(y_label)                                    + 
  ggtitle(title)                                   +
  graphics_theme_1

#-----------------------------------------------------------------
# K means clustering on event scores
#-----------------------------------------------------------------
set.seed(715)
clusters <- kmeans(season_2025$eventScore,6)
season_2025$cluster <- clusters$cluster
#write.csv(season_2025,'season_2025.csv',row.names = F)

#-----------------------------------------------------------------
# Summary statistics
#-----------------------------------------------------------------
library(dplyr)
season_summary <- season_2025            %>% 
  group_by(cluster)                      %>%
  summarise(mean   = mean(eventScore),
            median = median(eventScore),
            sd     = sd(eventScore),
            n      = n())                %>%
  arrange(desc(mean))

#-----------------------------------------------------------------
# Create Van Westendorp survey data
#-----------------------------------------------------------------
vw_data <- data.frame(matrix(nrow = 1000, ncol = 6))
names(vw_data) <- c('DugoutSeats', 'PriceExpectation', 
                    'TooExpensive', 'TooCheap', 
                    'WayTooCheap', 'WayTooExpensive')
set.seed(715)
vw_data[,1] <- 'DugoutSeats'
vw_data[,2] <- round(rnorm(1000,100,10),0)
vw_data[,3] <- round(rnorm(1000,130,20),0)
vw_data[,4] <- round(rnorm(1000,60,15),0)
vw_data[,5] <- round(rnorm(1000,50,10),0)
vw_data[,6] <- round(rnorm(1000,160,20),0)

#-----------------------------------------------------------------
# Empirical cumulative distribution function
#-----------------------------------------------------------------
library(Hmisc)
dat <- data.frame(
  "toocheap"     = vw_data$WayTooCheap,
  "notbargain"   = vw_data$TooExpensive,
  "notexpensive" = vw_data$TooCheap,
  "tooexpensive" = vw_data$WayTooExpensive
)
a <- Ecdf(dat$toocheap,what="1-F",pl=F)$y[-1]
b <- Ecdf(dat$notbargain, pl=F)$y[-1]
c <- Ecdf(dat$notexpensive,what = "1-F", pl=F)$y[-1]
d <- Ecdf(dat$tooexpensive,pl=F)$y[-1]

#-----------------------------------------------------------------
# Build data set for creating graphic
#-----------------------------------------------------------------
library(reshape2)
ecdf1 <- data.frame(
  "variable"  = rep("toocheap",length(a)),
  "ecdf"      = a,
  "value"    = sort(unique(dat$toocheap)))
ecdf2 <- data.frame(
  "variable"  = rep("notbargain",length(b)),
  "ecdf"      = b,
  "value"     = sort(unique(dat$notbargain),decreasing = T))
ecdf3 <- data.frame(
  "variable"  = rep("notexpensive",length(c)),
  "ecdf"     = c,
  "value"    = sort(unique(dat$notexpensive),decreasing = T))
ecdf4 <- data.frame(
  "variable"  = rep("tooexpensive",length(d)),
  "ecdf"      = d,
  "value"     = sort(unique(dat$tooexpensive)))
dat2 <- rbind(ecdf1,ecdf2,ecdf3,ecdf4)
dat  <- melt(dat)
dat  <- merge(dat,dat2,by=c("variable","value"))

#-----------------------------------------------------------------
# Graph the results
#-----------------------------------------------------------------
require(ggplot2)
require(scales)
require(RColorBrewer)

Paired     <- RColorBrewer::brewer.pal(4,"Paired")

g_xlab     <- '\n prices'
g_ylab     <- 'Responses  \n'
g_title    <- 'Dugout Price Value Perception\n'

vw_gaphic <- 
  ggplot(dat, aes(value, ecdf, color=variable)) + 
  annotate("rect", xmin = 55, xmax = 146, 
           ymin = 0,  ymax = 1,
           alpha = .4, fill = 'coral')          +
  geom_line(size = 1.2) +
  scale_color_manual(values = Paired,
                     name = 'Value Perception') + 
  xlab(g_xlab)                                  + 
  ylab(g_ylab)                                  + 
  ggtitle(g_title)                              + 
  scale_y_continuous(labels = percent)          +
  scale_x_continuous(labels = dollar)           +
  coord_cartesian(xlim = c(0,220),ylim= c(0,1)) +
  geom_hline(yintercept = .5,
             lty=4,
             alpha = .5)                        +
  graphics_theme_1

#-----------------------------------------------------------------
# Duplicate work with a library
#-----------------------------------------------------------------
library(pricesensitivitymeter)
price_sensitivity <- psm_analysis(
  toocheap        = "WayTooCheap",
  cheap           = "TooCheap",
  expensive       = "TooExpensive",
  tooexpensive    = "WayTooExpensive",
  data            = vw_data,
  validate        = TRUE)

#-----------------------------------------------------------------
# Explore price sensitivity data
#-----------------------------------------------------------------
ps <- 
  tibble::tibble(lower_price = price_sensitivity$pricerange_lower,
                 upper_price = price_sensitivity$pricerange_upper)

#-----------------------------------------------------------------
# Chapter 7
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# RFM data
#-----------------------------------------------------------------
library(dplyr)
demo_data <- FOSBAAS::demographic_data[,c(1,4)]
set.seed(44)
demo_data <- demo_data %>%
mutate(
 lastInteraction = abs(round(rnorm(nrow(demo_data),50,30),0)),
 interactionsYTD = abs(round(rnorm(nrow(demo_data),10,5),0)),
 lifetimeSpend   = abs(round(rnorm(nrow(demo_data),10000,7000),0))
)

#-----------------------------------------------------------------
# Function to calculate FRM scores
#-----------------------------------------------------------------
demo_data$Recency       <- -scale(demo_data$lastInteraction)
demo_data$Frequency     <- scale(demo_data$interactionsYTD)
demo_data$MonetaryValue <- scale(demo_data$lifetimeSpend)
# Produce quantiles for each scaled value
r_quant <- unname(quantile(demo_data$Recency,
                           probs = c(.2,.4,.6,.8)))
f_quant <- unname(quantile(demo_data$Recency,
                           probs = c(.2,.4,.6,.8)))
m_quant <- unname(quantile(demo_data$Recency,
                           probs = c(.2,.4,.6,.8)))
# Function to evaluate RFM score
f_create_rfm <- function(quantList,number){
  
if(number <= quantList[[1]]){'1'}
  else if(number <= quantList[[2]]){'2'}
    else if(number <= quantList[[3]]){'3'}
      else if(number <= quantList[[4]]){'4'}
        else{'5'}
}

#-----------------------------------------------------------------
# Create final RFM values
#-----------------------------------------------------------------
value <- list()
j     <- 1
for(i in demo_data$Recency){
  value[j] <- f_create_rfm(r_quant,i)
  j <- j + 1
}
demo_data$r_val <- unlist(value)
#-----------------------------------------------------------------
value <- list()
j     <- 1
for(i in demo_data$Frequency){
  value[j] <- f_create_rfm(f_quant,i)
  j <- j + 1
}
demo_data$f_val <- unlist(value)
#-----------------------------------------------------------------
value <- list()
j     <- 1
for(i in demo_data$MonetaryValue){
  value[j] <- f_create_rfm(m_quant,i)
  j <- j + 1
}
demo_data$m_val <- unlist(value)
#-----------------------------------------------------------------

demo_data$RFM <- paste(demo_data$r_val,
                       demo_data$f_val,
                       demo_data$m_val, sep = '')

#-----------------------------------------------------------------
# Create final RFM values
#-----------------------------------------------------------------
top_prospects <- subset(demo_data,demo_data$RFM == '555')

#-----------------------------------------------------------------
# access renewal data
#-----------------------------------------------------------------
library(FOSBAAS)
mod_data <- FOSBAAS::customer_renewals

#-----------------------------------------------------------------
# Dummy code and alter data frame for all numeric input
#-----------------------------------------------------------------
d1 <- as.data.frame(psych::dummy.code(mod_data$corporate))
d2 <- as.data.frame(psych::dummy.code(mod_data$planType))

mod_data_numeric <- dplyr::select(mod_data,ticketUsage,
                                  tenure,spend,tickets,
                                  distance,renewed) %>%
                    dplyr::bind_cols(d1,d2)

#-----------------------------------------------------------------
# Prepare the data for analysis
#-----------------------------------------------------------------
mod_data$renewed   <- factor(mod_data$renewed)
mod_data$accountID <- NULL
mod_data$season    <- NULL

#-----------------------------------------------------------------
# Using R to visualize geographic data
#-----------------------------------------------------------------
library(ggmap)
library(ggplot2)

demos <- FOSBAAS::demographic_data
demos <- demos[sample(nrow(demos), 5000), ]

map_data <- subset(demos,demos$longitude >= -125 & 
                     demos$longitude <= -67 &
                     demos$latitude >= 25.75 & 
                     demos$latitude <= 49)
us <- c(left = -91, bottom = 32, right = -80, top = 38)
map <- get_stamenmap(us, zoom = 6, maptype = "toner-lite") %>% 
  ggmap() 

geographic_vis <- 
  map +
  geom_point(data = map_data, 
             mapping = aes(x = longitude, y = latitude),
             size = .2,alpha = .5, color= 'dodgerblue')

#-----------------------------------------------------------------
# Download and install the mlr3 libraries
#-----------------------------------------------------------------
library("mlr3")         #  install.packages("mlr3viz")
library("mlr3learners") #  install.packages("mlr3learners")
library("mlr3viz")      #  install.packages("mlr3viz")
library("mlr3tuning")   #  install.packages("mlr3tuning")
library("paradox")      #  install.packages("paradox")

#-----------------------------------------------------------------
# Recode response to a factor
#-----------------------------------------------------------------
mod_data_numeric$renewed <- factor(mod_data_numeric$renewed)

#-----------------------------------------------------------------
# Build task
#-----------------------------------------------------------------
task_mod_data <- TaskClassif$new(id       = "task_renew", 
                                 backend  = mod_data_numeric, 
                                 target   = "renewed", 
                                 positive = "r")
# Add task to the task dictionary
mlr_tasks$add("task_renew", task_mod_data)

#-----------------------------------------------------------------
# Define learner
#-----------------------------------------------------------------
learner_ranger_rf <- lrn("classif.ranger",
                         predict_type = "prob",
                         mtry         = 3,
                         num.trees    = 500)
# Check parameters with this: learner_ranger_rf$param_set$ids()
# look at a list of learners: mlr3::mlr_learners

#-----------------------------------------------------------------
# Build test and training data set
#-----------------------------------------------------------------
set.seed(44)
train_mod_data <- sample(task_mod_data$nrow, 
                         0.75 * task_mod_data$nrow)
test_mod_data  <- setdiff(seq_len(task_mod_data$nrow), 
                          train_mod_data)

#-----------------------------------------------------------------
# Train the model
#-----------------------------------------------------------------
learner_ranger_rf$train(task    = task_mod_data, 
                        row_ids = train_mod_data)

#-----------------------------------------------------------------
# Inspect the results
#-----------------------------------------------------------------
# print(learner_ranger_rf$model)
learner_output <- 
  tibble::tibble(
    numTrees  = unname(learner_ranger_rf$model$num.trees),
    trys      = unname(learner_ranger_rf$model$mtry),
    samples   = unname(learner_ranger_rf$model$num.samples),
    error     = unname(learner_ranger_rf$model$prediction.error)
  )

#-----------------------------------------------------------------
# Evaluate holdout sample
#-----------------------------------------------------------------
prediction <- learner_ranger_rf$predict(task_mod_data, 
                                        row_ids = test_mod_data)

#-----------------------------------------------------------------
# Confusion matrix
#-----------------------------------------------------------------
prediction$confusion

#-----------------------------------------------------------------
# Evaluate holdout sample
#-----------------------------------------------------------------
measure = msr("classif.acc")
prediction$score(measure)

#-----------------------------------------------------------------
# Access model predictions
#-----------------------------------------------------------------
probs <- as.data.frame(learner_ranger_rf$model$predictions)
head(probs)

#-----------------------------------------------------------------
# Evaluate holdout sample
#-----------------------------------------------------------------

tasks        <- tsks(c("task_renew"))
learner      <- lrns(c("classif.featureless","classif.rpart"),
                     predict_type = "prob")
resampling   <- rsmps("cv")
object       <- benchmark(benchmark_grid(tasks, 
                                         learner, 
                                         resampling))
# Use head(fortify(object)) to see the ce for the resamples

#-----------------------------------------------------------------
# Boxplot of classification error
#-----------------------------------------------------------------
bplot <- 
  autoplot(object)   +
  graphics_theme_1 +
  geom_boxplot(fill = 'dodgerblue') 

#-----------------------------------------------------------------
# ROC curve for random forest model
#-----------------------------------------------------------------
roc_model <- 
  autoplot(object$filter(task_ids = "task_renew"), 
           type = "roc") +
  graphics_theme_1 +
  scale_color_manual(values = palette)

#-----------------------------------------------------------------
# Rebuild model
#-----------------------------------------------------------------
task_mod_data <- TaskClassif$new(id       = "task_renew", 
                                 backend  = mod_data_numeric, 
                                 target   = "renewed", 
                                 positive = "r")

learner_ranger_rf$train(task_mod_data, row_ids = train_mod_data)

#-----------------------------------------------------------------
# Add a resampliing parameter
#-----------------------------------------------------------------
resampling_mod_data  <- rsmp("cv")

#-----------------------------------------------------------------
# Rebuild model
#-----------------------------------------------------------------
resampling_mod_data$instantiate(task_mod_data)
resampling_mod_data$iters

#-----------------------------------------------------------------
# We can now call the resample object
#-----------------------------------------------------------------
resamp <- resample(task_mod_data, 
                   learner_ranger_rf, 
                   resampling_mod_data, 
                   store_models = TRUE)

#-----------------------------------------------------------------
# Rebuild model
#-----------------------------------------------------------------
resamp$aggregate(msr("classif.ce"))
# Look at scores from the models: resamp$score(msr("classif.ce"))

#-----------------------------------------------------------------
# Parameter set
#-----------------------------------------------------------------
param_set <- as.data.frame(learner_ranger_rf$param_set$ids())
head(param_set)

#-----------------------------------------------------------------
# Tuning parameters
#-----------------------------------------------------------------
tune_rf_params <- ParamSet$new(list(
  ParamInt$new("min.node.size", lower = 10, upper = 200),
  ParamInt$new("max.depth",     lower = 2,  upper = 20),
  ParamInt$new("num.trees",     lower = 500,  upper = 600)
))

#-----------------------------------------------------------------
# set resampling and eval parameters
#-----------------------------------------------------------------
resamp_strat     <- rsmp("holdout")
measure_mod_data <- msr("classif.ce")
evals_10         <- trm("evals", n_evals = 10)

#-----------------------------------------------------------------
# Build a tuning instance
#-----------------------------------------------------------------
tune_instance <- TuningInstanceSingleCrit$new(
  task         = task_mod_data,
  learner      = learner_ranger_rf,
  resampling   = resamp_strat,
  measure      = measure_mod_data,
  search_space = tune_rf_params,
  terminator   = evals_10
)

#-----------------------------------------------------------------
# Randomly select options within tuning min and max
#-----------------------------------------------------------------
tuner_rf = tnr("random_search")

#-----------------------------------------------------------------
# Run the models
#-----------------------------------------------------------------
tuner <- tuner_rf$optimize(tune_instance)

#-----------------------------------------------------------------
# Get the best parameters
#-----------------------------------------------------------------
best_params <- tune_instance$result_learner_param_vals

#-----------------------------------------------------------------
# Observe new classification error
#-----------------------------------------------------------------
tune_instance$result_y

#-----------------------------------------------------------------
# Rerun model
#-----------------------------------------------------------------
learner_ranger_rf$param_set$values = 
  tune_instance$result_learner_param_vals
learner_ranger_rf$train(task_mod_data)

#-----------------------------------------------------------------
# Build a tuning instance
#-----------------------------------------------------------------
prediction_tuned <- 
  learner_ranger_rf$predict(task_mod_data, 
                            row_ids = test_mod_data)

#-----------------------------------------------------------------
# Observe confusion matrix
#-----------------------------------------------------------------
prediction_tuned$confusion

#-----------------------------------------------------------------
# Observe optimized classification
#-----------------------------------------------------------------
measure = msr("classif.acc")
prediction_tuned$score(measure)

#-----------------------------------------------------------------
# Observe all learners
#-----------------------------------------------------------------
mlr3::mlr_learners

#-----------------------------------------------------------------
# Observe all learners
#-----------------------------------------------------------------

task_mod_data_num <- TaskClassif$new(id   = "task_bench", 
                                     backend  = mod_data_numeric, 
                                     target   = "renewed", 
                                     positive = "r")

learner_num <- 
  list(lrn("classif.xgboost", predict_type = "prob"), 
       lrn("classif.ranger", predict_type = "prob"))

set.seed(44)
train_mod_data_num <- 
  sample(task_mod_data_num$nrow, 0.75 * task_mod_data_num$nrow)
test_mod_data_num  <- 
  setdiff(seq_len(task_mod_data_num$nrow), train_mod_data_num)

#-----------------------------------------------------------------
# Build new task and learner
#-----------------------------------------------------------------
design_bnch <- benchmark_grid(
  task_bnch        <- TaskClassif$new(id  = "task_class2", 
                                      backend  = mod_data_numeric, 
                                      target   = "renewed", 
                                      positive = "r"),
  learners_bnch    <- 
    list(lrn("classif.xgboost", predict_type = "prob"), 
         lrn("classif.ranger",  predict_type = "prob"),
         lrn("classif.naive_bayes", predict_type = "prob" )),
  resamplings_bnch <- rsmp("holdout")
)

#-----------------------------------------------------------------
# benchmark our designs
#-----------------------------------------------------------------
bmr = benchmark(design_bnch)

#-----------------------------------------------------------------
# Observe available measures
#-----------------------------------------------------------------
mlr3::mlr_measures

#-----------------------------------------------------------------
# Compare the models
#-----------------------------------------------------------------
measures = list(
  msr("classif.auc", id = "auc"),
  msr("classif.ce", id = "ce_train")
)
measure_list <- as.data.frame(bmr$score(measures))
measure_list[,c(6,11,12)]

#-----------------------------------------------------------------
# Compare the models
#-----------------------------------------------------------------
mod <- glm(renewed ~ ticketUsage + tenure + 
             spend + distance,
           data = mod_data_numeric,
           family = binomial(link = "logit")
)
mod_sum <- 
  tibble::tibble(
    deviance      = unlist(summary(mod)$deviance),
    null.deviance = unlist(summary(mod)$null.deviance),
    aic           = unlist(summary(mod)$aic),
    df.residual   = unlist(summary(mod)$df.residual),
    pseudoR2      = 1 - mod$deviance / mod$null.deviance
  )

#-----------------------------------------------------------------
# Compare the models
#-----------------------------------------------------------------
calc_rates <- learner_ranger_rf$predict_newdata(mod_data_numeric)
mod_data_numeric$pred <- predict(mod,newdata = mod_data_numeric,
                                 type = 'response')

#-----------------------------------------------------------------
# Density plot of error
#-----------------------------------------------------------------
title   <- 'Density plot of renewal'
x_label <- 'Prediction'
y_label <- 'Density'
density_pred <- 
  ggplot(data = mod_data_numeric, 
         aes(x=pred,color=renewed,lty=renewed))  +
  geom_density(size = 1.2)                       + 
  scale_color_manual(values = palette)           +
  scale_x_continuous(label = scales::percent)    +
  xlab(x_label)                                  + 
  ylab(y_label)                                  + 
  ggtitle(title)                                 +
  graphics_theme_1

#-----------------------------------------------------------------
# ROC curve
#-----------------------------------------------------------------
library(pROC)
#define object to plot
roc_object <- roc(mod_data_numeric$renewed, mod_data_numeric$pred)

title   <- 'ROC curve for renewals'
x_label <- 'Specificity'
y_label <- 'Sensitivity'

roc_graph <-
  ggroc(roc_object,colour = 'dodgerblue',size = 1.2) +
  xlab(x_label)                                    + 
  ylab(y_label)                                    + 
  ggtitle(title)                                   +
  graphics_theme_1

#-----------------------------------------------------------------
# Build data fro cumulative gains chart
#-----------------------------------------------------------------
mod_data_sample <- 
  mod_data_numeric                                      %>% 
  dplyr::select(pred,renewed)                           %>%
  dplyr::mutate(custId = seq(1:nrow(mod_data_numeric))) %>%
  dplyr::sample_n(5000)                                 %>%
  dplyr::arrange(desc(pred))

qt <- quantile(mod_data_sample$pred,
               probs = c(.1,.2,.3,.4,.5,.6,.7,.8,.9))

f_apply_quant <- function(x){
  ifelse(x >= qt[9],1,
         ifelse(x >= qt[8],2,
                ifelse(x >= qt[7],3,
                       ifelse(x >= qt[6],4,
                              ifelse(x >= qt[5],5,
                                     ifelse(x >= qt[4],6,
                                            ifelse(x >= qt[3],7,
                                                   ifelse(x >= qt[2],8,
                                                          ifelse(x >= qt[1],9,10)))))))))
}

mod_data_sample$group <- sapply(mod_data_sample$pred,
                                function(x) f_apply_quant(x))

table(mod_data_sample$group,mod_data_sample$renewed)

#-----------------------------------------------------------------
# Build data for cumulative gains chart
#-----------------------------------------------------------------
mod_data_sample$renewedNum <- 
  ifelse(mod_data_sample$renewed == 'r',1,0)
mod_data_sample$perpop <- 
  (seq(nrow(mod_data_sample))/nrow(mod_data_sample))*100

mod_data_sample$percRenew <- 
cumsum(mod_data_sample$renewedNum)/sum(mod_data_sample$renewedNum)

title   <- 'Cumulative gains chart'
x_label <- 'Population'
y_label <- 'Cumulative Renewal Percentage'
cgc <- 
  ggplot(mod_data_sample,aes(y=percRenew,x=perpop))         +
  geom_line(color = mod_data_sample$group,size = 1.2 )      +
  geom_rug(color = mod_data_sample$group,sides = 'b' )      +
  geom_abline(intercept = 0, slope = .01, size = 0.5,lty=3) +
  xlab(x_label)                                             + 
  ylab(y_label)                                             + 
  ggtitle(title)                                            +
  graphics_theme_1

#-----------------------------------------------------------------
# Build data for improved cumulative gains chart 
#-----------------------------------------------------------------
mod_data_gain <- mod_data_sample %>%
  group_by(group) %>%
  summarise(cumRenewed = sum(renewedNum))

new_renewals <- c(500,490,455,400,300,200,120,90,70,20)
mod_data_gain$cumRenewed <- new_renewals

mod_data_gain$gain <- 
  cumsum(mod_data_gain$cumRenewed/sum(mod_data_gain$cumRenewed))


title   <- 'Cumulative gains chart'
x_label <- 'Group'
y_label <- 'Gain'

cgc_imp <- 
  ggplot(mod_data_gain,aes(y=gain,x=group))                 +
  geom_line(color = mod_data_gain$group,size = 1.2)         +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10))    +
  geom_abline(intercept = 0, slope = .01, size = 0.5,lty=3) +
  xlab(x_label)                                             + 
  ylab(y_label)                                             + 
  ggtitle(title)                                            +
  graphics_theme_1

#-----------------------------------------------------------------
# Chapter 8
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Season data structure
#-----------------------------------------------------------------
data <- FOSBAAS::season_data

data_struct <- 
  data.frame(
    variable = names(data),
    class    = sapply(data, typeof),
    values   = sapply(data, function(x) paste0(head(x)[1:2],  
                                               collapse = ", ")),
    row.names = NULL
  )

#-----------------------------------------------------------------
# Promotions by season
#-----------------------------------------------------------------
data$count <- seq(1:nrow(data))

x_label  <- ('\n 2022-2024 Home Games')
y_label  <- ('Tickets Sold \n')
title   <- ("M-Promos tend to have higher than average sales")
ticket_sales <- 
  ggplot(data, 
         aes(x = count,y = ticketSales, 
             color = factor(promotion)),
         group = promotion)                                  +
  ggtitle(title)                                                 +
  xlab(x_label)                                                  +                         
  ylab(y_label)                                                  +                           scale_x_continuous(breaks = 1:243  )                           +
  scale_y_continuous(labels = scales::comma)                     +
  geom_point(aes(y=ticketSales,x=count), size=2)                 +
  geom_smooth(data=subset(
    data,promotion == 'bobblehead' | promotion == 'concert' | 
      promotion == 'other' | promotion == 'none'),
    method='lm',formula=y~x,se=FALSE,fullrange=TRUE,size=1.2)    +
  geom_vline(xintercept = 1, lty=4, color='grey30')              +
  geom_vline(xintercept = 81, lty=4, color='grey30')             +
  geom_vline(xintercept = 162, lty=4, color='grey30')            +
  geom_vline(xintercept = 243, lty=4, color='grey30')            +
  scale_color_manual(
    breaks = c('bobblehead','concert','other','none'),
    values=palette, name='Promotion: ',
    labels=c("BHead","Concert","Other",'None'))                  +    
  annotate("text", x = 40,  y = 1000, 
           label = "2022", color='black')                        +
  annotate("text", x = 120, y = 1000, 
           label = "2023", color='black')                        +
  annotate("text", x = 200, y = 1000, 
           label = "2024", color='black')                        +
  graphics_theme_1                                               + 
  theme(
    axis.text.x      = element_blank(),
    legend.position  = "bottom",
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(), 
    axis.ticks.x=element_blank()
  )

#-----------------------------------------------------------------
# Promotions by day of the week
#-----------------------------------------------------------------
x_label  <- ('\n Promotion')
y_label  <- ('Day of Week\n')
title    <- 
  ('Average Sales (10,000s) by promotion and day of week \n')

promos <- 
  data                                        %>% 
  group_by(dayOfWeek,promotion,season)        %>%
  summarise(avgTickets = median(ticketSales))

tile_sales <- 
  ggplot(promos, aes(y=dayOfWeek,x=promotion))                   +
  facet_grid(.~season)                                           +
  geom_tile(aes(fill = avgTickets))                              + 
  geom_text(aes(label = round((avgTickets/10000), 2)),
            color='grey10')                                      +
  scale_fill_gradient(low = "white", high = "dodgerblue", 
                      space = "Lab",
                      na.value = "grey10", guide = "colourbar")  +
  ggtitle(title)                                                 +
  xlab(x_label)                                                  +                       
  ylab(y_label)                                                  + 
  scale_y_discrete(limits=c('Mon','Tue','Wed','Thu',
                            'Fri','Sat','Sun'))                  + 
  scale_x_discrete(limits = c('bobblehead','concert',
                              'none','other'),
                   labels=c('bh','concert','none','other'))      +
  graphics_theme_1 + theme(
    legend.position  = "bottom",
    axis.text.x      = element_text(angle = 0, size = 10, 
                                    vjust = 0, color = "grey10"),
    legend.title     = element_text(size = 10, face = "plain", 
                                    color = "grey10"), 
    legend.text      = element_text(size = 7, color = "grey10")
  )

#-----------------------------------------------------------------
# Color interpolation
#-----------------------------------------------------------------
library(viridis)
scale_fill_viridis(direction = 1, option = "B",trans="log2") 
scale_fill_distiller(palette = 'Greens',direction = 1)

#-----------------------------------------------------------------
# Promotions by season and day of week
#-----------------------------------------------------------------
data$count <- seq(1:nrow(data))

x_label  <- ('\n 2022-2024 Home Games')
y_label  <- ('Tickets Sold \n ')
title    <- ("Impacts are less clear when DOW is considered")

dow_sales <- 
  ggplot(data, aes(x = count,y = ticketSales, 
                   color = factor(dayOfWeek),
                   shape = factor(promotion),
                   group =factor(dayOfWeek)))                +
  ggtitle(title)                                             +
  xlab(x_label)                                              +                            
  ylab(y_label)                                              +
  scale_x_continuous( breaks = 1:242)                        +
  scale_y_continuous(labels  = scales::comma)                +
  geom_point(aes(y=data$ticketSales,x=data$count), size=3.5) +
  geom_vline(xintercept = 81,  lty = 4,  color ='grey30')    +
  geom_vline(xintercept = 162, lty = 4, color ='grey30')     +
  geom_vline(xintercept = 242, lty = 4, color ='grey30')     +
  scale_color_manual(breaks = c("Mon", "Tue",'Wed','Thu',
                                'Fri','Sat','Sun'),
                     values = palette,
                     name   ='Day: ',
                     labels = c("Mon", "Tue",'Wed','Thu',
                                'Fri','Sat','Sun'))          +   
  scale_shape_manual(
    breaks = c('bobblehead','none','concert','other'),
    name='Promotion: ',
    values = c(17,19,3,7),
    labels=c("bh",'none','con','other'))                     +   
  annotate("text", x = 40,  y = 1000, 
           label = "2022", color='grey10')                   +
  annotate("text", x = 120, y = 1000, 
           label = "2023", color='grey10')                   +
  annotate("text", x = 200, y = 1000, 
           label = "2024", color='grey10')                   +
  graphics_theme_1                                           +   
  theme(
    axis.text.x      = element_blank(),
    legend.position  = "right",
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(), 
    axis.ticks.x     = element_blank())

#-----------------------------------------------------------------
# Box plot, sales by day of week
#-----------------------------------------------------------------
x_label  <- ('\n 2022-2024 games by DOW')
y_label  <- ('Tickets Sold \n ')
title    <- ("Sales by DOW and promotion")

dow_sales_box <- 
  ggplot(data, aes(x     = dayOfWeek, 
                   fill  = factor(promotion), 
                   color = factor(promotion)))               +
  ggtitle(title)                                             +
  xlab(x_label)                                              +                               
  ylab(y_label)                                              +
  scale_y_continuous(labels = scales::comma)                 +
  scale_x_discrete(limits=c("Mon", "Tue",'Wed','Thu',
                            'Fri','Sat','Sun'))              + 
  geom_boxplot(aes(y=ticketSales))                           +
  scale_color_manual(
    breaks = c('bobblehead','concert','none','other'),
    values=c('grey40','grey40','grey40','grey40'), 
    name='Concert: ',
    labels=c('bobblehead','concert','none','other'),
    guide = 'none')                                          +    
  scale_fill_manual(
    breaks = c('bobblehead','concert','none','other'),
    values=c(palette), 
    name='Promotion: ',
    labels=c('bobblehead','concert','none','other'))         +
  graphics_theme_1                                           +   
  theme(legend.position  = "bottom")

#-----------------------------------------------------------------
# preprocessing our data
#-----------------------------------------------------------------
library(tidymodels)
library(readr)      
library(broom.mixed) 
library(dotwhisker)  
library(skimr)
library(dplyr)

data <- FOSBAAS::season_data
data <- data[,c("gameNumber","team","month","weekEnd",
                "daysSinceLastGame","promotion","ticketSales")]

#-----------------------------------------------------------------
# Alter promotions 
#-----------------------------------------------------------------
f_change_order <- function(x){
  if(x == "none"){"anone"}
  else{x}
}
data$promotion <- sapply(data$promotion,function(x) 
  f_change_order(x))

#-----------------------------------------------------------------
# Splitting our data set
#-----------------------------------------------------------------
set.seed(755)
data_split <- initial_split(data, prop = .75)
train_data <- rsample::training(data_split)
test_data  <- rsample::testing(data_split)

#-----------------------------------------------------------------
# Build a recipe
#-----------------------------------------------------------------
sales_rec <-  recipe(ticketSales ~ ., data = train_data)

#-----------------------------------------------------------------
# Add functions to the recipe
#-----------------------------------------------------------------
sales_rec <- recipe(ticketSales ~ ., data = train_data) %>% 
  update_role(gameNumber, new_role = "ID")              %>%
  step_dummy(all_nominal(), -all_outcomes())            %>%
  step_zv(all_predictors())

#-----------------------------------------------------------------
# Check the recipe 
#-----------------------------------------------------------------

data_test <- sales_rec      %>% 
  prep()                    %>% 
  bake(new_data = test_data)

head(data_test)[c(4:9)]

#-----------------------------------------------------------------
# Define a model
#-----------------------------------------------------------------
lm_model <- linear_reg() %>% 
  set_engine('lm')       %>% 
  set_mode('regression')

#-----------------------------------------------------------------
# build a workflow
#-----------------------------------------------------------------
sales_wflow <- workflow() %>% 
  add_model(lm_model)     %>% 
  add_recipe(sales_rec)

#-----------------------------------------------------------------
# build a workflow
#-----------------------------------------------------------------
set.seed(755)
folds        <- vfold_cv(train_data, v = 10)
sales_fit_rs <- sales_wflow          %>% 
  fit_resamples(folds)

cv_metrics <- collect_metrics(sales_fit_rs)

#-----------------------------------------------------------------
# Run the model and extract the results
#-----------------------------------------------------------------

sales_fit <- sales_wflow %>% 
  fit(data = train_data)

results   <- sales_fit   %>% 
  pull_workflow_fit()    %>% 
  tidy()

#-----------------------------------------------------------------
# build a workflow
#-----------------------------------------------------------------
model         <- extract_model(sales_fit)
model_metrics <- glance(model)

#-----------------------------------------------------------------
# Clustered events
#-----------------------------------------------------------------
season_2025 <- read.csv('season_2025.csv')

season_2025$cluster <- factor(season_2025$cluster)
x_label  <- ('\n Game')
y_label  <- ('Event Scores \n')
title   <- ('Event score clusters by event')
es_box <- 
  ggplot2::ggplot(data = season_2025, 
                  aes(x = order,
                      y = eventScore,
                      color = cluster))             +
  geom_point(size = 1,alpha = .5)                   +
  geom_boxplot()                                    +
  scale_color_manual(values = palette)              +
  scale_y_continuous(label = scales::comma)         +
  xlab(x_label)                                     + 
  ylab(y_label)                                     + 
  ggtitle(title)                                    +
  graphics_theme_1

#-----------------------------------------------------------------
# Identify game breaks
#-----------------------------------------------------------------
season_2025$border <- 
  ifelse(duplicated(season_2025$cluster) == TRUE,FALSE,TRUE)

#-----------------------------------------------------------------
# Identify candidate games
#-----------------------------------------------------------------
season_2025$cluster <- factor(season_2025$cluster)
x_label  <- ('\n Pred Sales')
y_label  <- ('Pred Price \n')
title   <- ('Predicted sales and price by cluster')
es_scatter <- 
  ggplot2::ggplot(data      = season_2025, 
                  aes(x     = predTickets,
                      y     = predPrices,
                      color = cluster,
                      shape = border))                      +
  annotate("rect", xmin = 45000 - confint(model)[36,2], 
           xmax = 45000, 
           ymin = 0,  ymax = 60,
           alpha = .4, fill = 'coral')                      +
  geom_point(size = 3,alpha = 1)                            +
  geom_segment(aes(x = 39000, y = 50, 
                   xend = 37500, yend = 41),
               arrow = arrow(length = unit(0.5, "cm")),
               color = 'black')                             +
  scale_color_manual(values = palette)                      +
  scale_x_continuous(label = scales::comma)                 +
  scale_y_continuous(label = scales::dollar)                +
  xlab(x_label)                                             + 
  ylab(y_label)                                             + 
  ggtitle(title)                                            +
  graphics_theme_1

#-----------------------------------------------------------------
# Get Confidence Intervals
#-----------------------------------------------------------------
candidate <- subset(season_2025, 
                    season_2025$border == T & cluster == 2)

#-----------------------------------------------------------------
# Chapter 9
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Create spend data
#-----------------------------------------------------------------
sth <- as.data.frame(matrix(nrow=1000,ncol = 2))
fan <- as.data.frame(matrix(nrow=1000,ncol = 2))

names(sth) <- c('spend','type')
names(fan) <- c('spend','type')

set.seed(715)
sth$spend <- rnorm(1000,25,7)
fan$spend <- rnorm(1000,18,7)
sth$type  <- 'sth'
fan$type  <- 'fan'

# Sample data
set.seed(354)
sth_samp <- sth[sample(nrow(sth), 400), ]
fan_samp <- fan[sample(nrow(fan), 400), ]

#-----------------------------------------------------------------
# T Test
#-----------------------------------------------------------------
t_test <- t.test(sth_samp$spend,fan_samp$spend)

#-----------------------------------------------------------------
# Power analysis
#-----------------------------------------------------------------
effect_size <- 3/sd(sth_samp$spend)
signif_lvl  <- .05
certainty   <- .9 

pwr_test <- pwr::pwr.t.test(d         = effect_size,
                            sig.level = signif_lvl,
                            power     = certainty,
                            type      = 'two.sample')

#-----------------------------------------------------------------
# Factorial Design
#-----------------------------------------------------------------
library(AlgDesign)

fd <- gen.factorial(levels   = 3,
                    nVars    = 2,
                    center   = TRUE,
                    factors  = FALSE,
                    varNames = c("Dugout Seats",
                                 "Homeplate Seats"))

#-----------------------------------------------------------------
# Survey Responses
#-----------------------------------------------------------------
survey_responses <- 
  tibble::tibble(dugout = factor(unlist(rep(fd[1],1000))),
                 homePlate = factor(unlist(rep(fd[2],1000))),
                 selection = NA)

survey_responses$dugout <- 
  sapply(survey_responses$dugout, 
         function(x) switch(x,'-1' = '75',
                              '0' = '85',
                              '1' = '95'))

survey_responses$homePlate <- 
  sapply(survey_responses$homePlate, 
         function(x) switch(x,'-1' = '75',
                              '0' = '85',
                              '1' = '95'))

survey_responses$respondent <- 
  c(rep('sth',4500), rep('sin',4500))

selection <- list()
x <- 1
set.seed(755)
while(x <= nrow(survey_responses)){
  
  s1 <- survey_responses[x,1] 
  s2 <- survey_responses[x,2]
  
  selection_list <- c(s1,s2)
  
  selection[x] <- sample(selection_list, 
                         size = 1, 
                         prob = c(.2,.8))
  x <- x + 1
  
}
survey_responses$selection <- unlist(selection)
survey_responses$section  <- 
  ifelse(survey_responses$selection == survey_responses$dugout,
         'dugout',
         'homePlate')

#-----------------------------------------------------------------
# Interaction Plot
#-----------------------------------------------------------------
ip_data <- survey_responses    %>%
  group_by(respondent,section) %>%
  summarise(meanSelection = mean(as.numeric(selection)))
g_xlab  <- 'Respondent Type'                     
g_ylab  <- 'Mean Price'                         
g_title <- 'Interaction Plot, respondents and location'

ip_plot <- 
  ggplot(ip_data, aes(x = respondent,
                      y = meanSelection,
                      color = section,
                      group = section))      +
  geom_point(shape=2, size=3)                +
  geom_line(size = 1.1)                      +
  scale_y_continuous(label = scales::dollar) +
  scale_color_manual(values = palette)       +
  xlab(g_xlab)                               + 
  ylab(g_ylab)                               + 
  ggtitle(g_title)                           +
  graphics_theme_1

#-----------------------------------------------------------------
# Calculating scores for perceptual data
#-----------------------------------------------------------------
library(anacor)
pd <- as.data.frame(FOSBAAS::perceptual_data)
row.names(pd) <- c('Chickens','Titans','Predators') 

anHolder <- anacor(pd,ndim=2)

anHolderGG <- 
  data.frame(dim1 = c(anHolder$col.scores[,1],
                      anHolder$row.scores[,1]), 
             dim2 = c(anHolder$col.scores[,2],
                      anHolder$row.scores[,2]),
             type = c(rep(1,length(anHolder$col.scores[,1])),
                      rep(2,length(anHolder$row.scores[,1]))))

#-----------------------------------------------------------------
# Create perceptual map
#-----------------------------------------------------------------
library(scales)
library(RColorBrewer)
library(grid)
library(gridExtra)

basePal     <- c('grey60','mediumseagreen','grey40','steelblue1')

g_xlab     <- '\n PC1 (55.7%)'
g_ylab     <- 'PC2 (44.3%) \n'
g_title    <- 'Nashville Area Sports Perceptual Map \n'

PM <- 
  ggplot(data = anHolderGG,aes(x=dim1,y=dim2,
                               colour=factor(type)))         +
  geom_point(size = .5, fill = 'white', colour = 'White')    +
  geom_segment(data = anHolderGG, aes(x = 0, y = 0, 
                                      xend = dim1, 
                                      yend = dim2), 
               arrow = arrow(length=unit(0.2,"cm")), 
               alpha = 0.75, color = "steelblue2")           +
  geom_text(aes(label       =  rownames(anHolderGG)),
            size         = 3.2,
            position     = "jitter")                         +
  scale_color_manual(values = basePal,
                     name   = 'Perception',
                     breaks = c(1,2),
                     guide  = FALSE)                         + 
  xlab(g_xlab)                                               + 
  ylab(g_ylab)                                               + 
  ggtitle(g_title)                                           + 
  graphics_theme_1                                           +
  geom_vline(xintercept = 0,lty=4,alpha = .4)                +
  geom_hline(yintercept = 0,lty=4,lpha = .4)                 +
  coord_cartesian(xlim = c(-.7,.9))       

#-----------------------------------------------------------------
# Chapter 10
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Access scan data
#-----------------------------------------------------------------
scan_data <- FOSBAAS::scan_data

#-----------------------------------------------------------------
# Scans
#-----------------------------------------------------------------
x_label <- 'observation'                                             
y_label <- 'scans'                                          
title   <- 'Ticket scans by minute at gates'
scan_point <- 
  ggplot(data = scan_data,aes(x     = observations,
                              y     = scans,
                              color = date))                +
  geom_point()                                              +
  scale_x_continuous(label = scales::comma)                 +
  scale_y_continuous(label = scales::comma)                 +
  scale_color_manual(values = palette)                      +
  xlab(x_label)                                             + 
  ylab(y_label)                                             + 
  ggtitle(title)                                            +
  stat_smooth(method = "lm", formula = y ~ x + poly(x,2)-1) +
  geom_vline(xintercept = 151, lty = 4)                     +
  graphics_theme_1                                          +
  guides(color = 
         guide_legend(override.aes = list(fill = "grey99")))

#-----------------------------------------------------------------
# Scans
#-----------------------------------------------------------------
max_scans <- scan_data                     %>% 
  group_by(date)                           %>%
  summarise(maxScans = max(scans),
            maxScansMean = (max(scans)/16),
            meanScans = mean(scans),
            medianScans = median(scans))

#-----------------------------------------------------------------
# Create scan data
#-----------------------------------------------------------------
scans_a <- f_get_scan_data(x_value = 230,
                           y_value = 90,
                           seed    = 714,
                           sd_mod  = 10)

#-----------------------------------------------------------------
# Create line length data
#-----------------------------------------------------------------
line_length_a <- f_get_line_length(seed = 755,
                                   n    = 300,
                                   u1   = 22,
                                   sd1  = 8,
                                   u2   = 8 ,
                                   sd2  = 5)

line_length_a$action_time <- f_get_time_observations(17,21)
line_length_a$date <- "4/1/2024"

#-----------------------------------------------------------------
# Line length and scans
#-----------------------------------------------------------------
scans_a$cumScans <- cumsum(scans_a$scans)
data  <- dplyr::left_join(scans_a,line_length_a, 
                          by = "action_time")
data$color <- as.numeric(as.character(gsub(":",
                                           "",
                                           data$action_time)))

x_label  <- ('\n Cumulative Scans')
y_label  <- ('Line Length \n')
title    <- ('Line Length and cumulative scans')
legend   <- ('Time')
line_length <- 
  ggplot(data, aes(y     = lineLength, 
                   x     = cumScans, 
                   color = color))                           +    
  geom_point()                                               +
  scale_color_gradient(breaks = c(2100,2000,1900,1800,1700),
                       labels = c("9:00","8:00", "7:00", 
                                  "6:00","5:00"),
                       high = 'dodgerblue',
                       low  = 'coral',
                       name = legend)                        +
  scale_x_continuous(label = scales::comma)                  +
  xlab(x_label)                                              + 
  ylab(y_label)                                              + 
  ggtitle(title)                                             +
  stat_smooth(method = "loess", formula = y ~ x)             +
  geom_vline(xintercept = 13068, lty = 4)                    +
  graphics_theme_1 

#-----------------------------------------------------------------
# Generalized additive model
#-----------------------------------------------------------------
library(mgcv)
scans_a$cumScans <- cumsum(scans_a$scans)
data             <- dplyr::left_join(scans_a,line_length_a, 
                                     by = "action_time")

data$color <- as.numeric(as.character(gsub(":",
                                           "",
                                           data$action_time)))
gam1 <- mgcv::gam(lineLength ~ s(cumScans, bs='ps', sp=.2), 
                  data = data)

newPredict <- cbind(data, predict(gam1, interval = 'confidence'))

gr <- 
  ggplot(newPredict, aes(y     = lineLength, 
                         x     = cumScans,
                         color = color))                      +    
  geom_point(alpha=.7)                                        +
  scale_color_gradient(breaks = c(2100,2000,1900,1800,1700),
                       labels = c("9:00","8:00", "7:00", 
                                  "6:00","5:00"),
                       high = 'dodgerblue',
                       low  = 'coral',
                       name = 'Time')                         +
  geom_line(aes(y = `predict(gam1, interval = "confidence")`,
                x = cumScans),
            color = 'dodgerblue',size = 1.2)                  +
  scale_x_continuous(label = scales::comma)                   +
  xlab('Cumulative Scans')                                    + 
  ylab('Line-Length')                                         + 
  ggtitle('Results of GAM on Line-Length Data')               +
  graphics_theme_1   

#-----------------------------------------------------------------
# Simulate wait times function
#-----------------------------------------------------------------
f_get_wait_times <- function(seed,n = 300,time,rate1,rate2,rate3){
  set.seed(seed)
  
  order_times       <- rexp(n, rate = rate1)
  payment_times     <- rexp(n, rate = rate2)
  fulfillment_times <- rexp(n, rate = rate3)
  total_time        <- order_times       + 
    payment_times     + 
    fulfillment_times
  
  wait_times <- data.frame(transaction  = seq(1,n, by = 1),
                           orderTimes   = order_times,
                           paymentTimes = payment_times,
                           fulfillTimes = fulfillment_times,
                           totalTime    = total_time)
  wait_times[] <- apply(wait_times,2,function(x) round(x,0))
  return(wait_times)
}

#-----------------------------------------------------------------
# Create wait times data set
#-----------------------------------------------------------------
wait_times_a <- f_get_wait_times(seed  = 755,
                                 n     = 300,
                                 rate1 = .03,
                                 rate2 = .06,
                                 rate3 = .15)

#-----------------------------------------------------------------
# Simulate wait times function
#-----------------------------------------------------------------
wait_dist <- 
  wait_times_a                                          %>% 
  select(orderTimes,paymentTimes,fulfillTimes)          %>%
  tidyr::pivot_longer(cols = c('orderTimes',
                               'paymentTimes',
                               'fulfillTimes'),
                      names_to = "measurment",
                      values_to = "seconds")            %>%
  mutate(scale_seconds = scale(seconds))

w_dist <-  
  ggplot(wait_dist, aes(x = seconds, fill= measurment)) +    
  geom_density(alpha=.75)                               +
  geom_rug(color='coral',sides = "b")                   +
  scale_fill_manual(values=palette)                     + 
  xlab('Seconds')                                       +
  ylab('Density')                                       + 
  ggtitle('Distribution of wait-time components')       +
  graphics_theme_1 

#-----------------------------------------------------------------
# Simulate wait times function
#-----------------------------------------------------------------
wait_times <- FOSBAAS::wait_times_data[1:300,]

#-----------------------------------------------------------------
# Build correlation table
#-----------------------------------------------------------------
wt_cor <- wait_times[,-1]
names(wt_cor) <- c('Order Time','Payment Time',
                   'Fulfillment Time','Total Time')

wt_cor_result      <- cor(wt_cor)
wt_cor_result      <- round(as.data.frame(wt_cor_result), 2)
wt_cor_result$type <- row.names(wt_cor_result)

cor_long <- 
  tidyr::pivot_longer(wt_cor_result,
                      cols = c('Order Time','Payment Time', 
                               'Fulfillment Time','Total Time'))

#-----------------------------------------------------------------
# Build correlation table
#-----------------------------------------------------------------
cor_long$order <- factor(cor_long$type, 
                         levels=c('Fulfillment Time',
                                  'Payment Time',
                                  'Order Time',
                                  'Total Time'))

#-----------------------------------------------------------------
# Correlation table 
#-----------------------------------------------------------------
library(forcats)
correlation_table <- 
  cor_long                                                 %>%
  mutate(name = fct_reorder(name, value, .fun='sum'))      %>%
  ggplot(aes(x = order, y = name,fill = value))          +
  geom_tile()                                            +
  geom_text(aes(label=value))                            +
  scale_fill_gradient2(low  = "mediumseagreen", 
                       mid  = "white", 
                       high = "dodgerblue")              +
  xlab('')                                               + 
  ylab('')                                               + 
  ggtitle('Correlation table')                           +
  graphics_theme_1                                       +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))

#-----------------------------------------------------------------
# Box plot of wait times
#-----------------------------------------------------------------
wait_long <- 
  tidyr::pivot_longer(wait_times,cols = c('orderTimes',
                                          'paymentTimes', 
                                          'fulfillTimes',
                                          'totalTime'))
wait_box <- 
  wait_long                                             %>%
  mutate(name = fct_reorder(name, value, .fun='sum'))   %>%
  ggplot(aes(x = name, y = value)) +
  geom_boxplot(fill = 'dodgerblue') +
  xlab('\n Transaction component')                       + 
  ylab('Wait time in seconds')                           + 
  ggtitle('Variance of transaction times')               +
  graphics_theme_1

#-----------------------------------------------------------------
# Quantiles
#-----------------------------------------------------------------
quantiles <-
  apply(wait_times[,-1],
        2,
        function(x) quantile(x, probs = c(.1,0.25, 0.5, 0.75,.9)))

#-----------------------------------------------------------------
# Simple linear model for total times
#-----------------------------------------------------------------
time_mod <- lm(totalTime ~ fulfillTimes + paymentTimes + 
                 orderTimes,data = wait_times)

stats_time_mod <- 
  tibble::tibble(
    st_error     = unlist(summary(time_mod)[6]),
    r_square     = unlist(summary(time_mod)[8]),
    adj_r_square = unlist(summary(time_mod)[9]),
    f_stat       = unlist(summary(time_mod)$fstatistic[1])
  )

#-----------------------------------------------------------------
# Acess the distribution data
#-----------------------------------------------------------------
library(FOSBAAS)

wait_times_distribution <- FOSBAAS::wait_times_distribution_data

#-----------------------------------------------------------------
# Get a sample of the data
#-----------------------------------------------------------------
set.seed(755)
wait_sample <- wait_times_distribution %>%
  sample_frac(size = .7)

#-----------------------------------------------------------------
# Cumulative distribution function
#-----------------------------------------------------------------
orders <- wait_sample$orderTimes 
cdf <- ecdf(orders) 

#-----------------------------------------------------------------
# Cumulative distribution function
#-----------------------------------------------------------------
cdf_out <- cdf(50)
qua_out <- quantile(wait_sample$orderTimes,probs = cdf_out)

ecdf_quant <- tibble::tibble(ecdf_50 = cdf_out,
                             quantile_ecdf_50 = qua_out)
ecdf_quant

#-----------------------------------------------------------------
# Cumulative distribution function plot
#-----------------------------------------------------------------
cdf_plot <- 
  ggplot(wait_sample, aes(orders))          + 
  stat_ecdf(geom = "step",
            color = 'dodgerblue',
            size = 1.1)                     +
  xlab('\n Orders')                         + 
  ylab('Percentage observations')           + 
  ggtitle('ECDF Order Time')                +
  geom_vline(xintercept = 50.22143,lty = 2) +
  geom_hline(yintercept = .7785714,lty = 2) +
  graphics_theme_1

#-----------------------------------------------------------------
# Simulate orders 
#-----------------------------------------------------------------
set.seed(715)
n <- 400 # observations
sim_orders <- rexp(n, rate=1/mean(wait_sample$orderTimes))

#-----------------------------------------------------------------
# Compare histograms
#-----------------------------------------------------------------
hist_tab <- 
  tibble::tibble(sim_data = sim_orders,
                 actual_data = sample(wait_sample$orderTimes,400)
 )

hist_tab_long <- hist_tab %>% 
  tidyr::pivot_longer(cols      = c('sim_data',
                                    'actual_data'),
                      names_to  = "measurment",
                      values_to = "seconds") 

hist_comp <- 
  ggplot(hist_tab_long , aes(seconds))    + 
  facet_grid(.~measurment)               +
  geom_histogram(fill = 'dodgerblue')    +
  xlab('\n seconds')                     + 
  ylab('count')                          + 
  ggtitle('Simulated vs. actual values') +
  graphics_theme_1

#-----------------------------------------------------------------
# Simulate, Iterate, and aggregate
#-----------------------------------------------------------------
n <- 500 # observations

sim_orders  <- list()
sim_pay     <- list()
sim_fulfill <- list()
# Iterate
for(i in 1:500){
  set.seed(i + 715)
  # Simulate
  sim_orders[[i]]  <-  
    rexp(n, rate=1/mean(wait_sample$orderTimes))
  sim_pay[[i]]     <-  
    rexp(n, rate=1/mean(wait_sample$paymentTimes))
  sim_fulfill[[i]] <-  
    rexp(n, rate=1/mean(wait_sample$fulfillTimes))
}
# Aggregate
mean_order   <- mean(sapply(sim_orders, mean))
mean_pay     <- mean(sapply(sim_pay, mean))
mean_fulfill <- mean(sapply(sim_fulfill, mean))
mean_total   <- mean_order + mean_pay + mean_fulfill

#-----------------------------------------------------------------
# Aggregated results
#-----------------------------------------------------------------
mean_chart <- tibble::tibble(order   = mean_order,
                             payment = mean_pay,
                             fulfill = mean_fulfill,
                             total   = mean_total)

#-----------------------------------------------------------------
# Results with variance
#-----------------------------------------------------------------
total_time_samp <- sim_orders[[1]]  +
  mean_pay[[1]]    + 
  mean_fulfill[[1]]
count_list <- list()
set.seed(715)
for(j in 1:30){
  time            <- 0
  count           <- 1
  seconds_in_hour <- 60*60
  while(time <= seconds_in_hour){
    i           <- sample(total_time_samp,1)
    time        <- time + i
    count       <- count + 1
  }
  count_list[j]   <- count - 1
}

#-----------------------------------------------------------------
# Observe variance in fans serviced per hour
#-----------------------------------------------------------------
counts <- tibble::tibble(fans_serviced = unlist(count_list),
                         simulation    = seq(1:30))

service_per_hour <- 
  ggplot(counts , aes(x = simulation,
                      y = fans_serviced)) + 
  geom_line(color = 'dodgerblue')         +
  xlab('\n simulation')                   + 
  ylab('Fans serviced')                   + 
  ggtitle('Simulated services per hour')  +
  geom_hline(yintercept = 64,lty = 4)     +
  graphics_theme_1

#-----------------------------------------------------------------
# Function to build a frequency table
#-----------------------------------------------------------------
f_build_freq_table <- function(variable){
  
  pr          <- as.data.frame(table(variable))
  pr$prob     <- pr$Freq/sum(pr$Freq)
  pr$variable <- as.numeric(as.character(pr$variable))  
  
  return(pr)
  
}

order_freq <- f_build_freq_table(wait_sample$orderTimes)
#sum(order_freq$prob)

#-----------------------------------------------------------------
# Access frequency table data
#-----------------------------------------------------------------
freq_table  <- FOSBAAS::freq_table_data

freq_table$cumprob <- cumsum(freq_table$prob)
freq_table_graph <- 
  ggplot(freq_table,aes(x = variable,y=cumprob)) +
  geom_line(size = 1.2,color = 'dodgerblue')     +
  xlab('\n Seconds')                             + 
  ylab('Percent of Values')                      + 
  ggtitle('Table of values')                     +
  graphics_theme_1

#-----------------------------------------------------------------
# Apply fits
#-----------------------------------------------------------------
library(mgcv)
freq_table$cumprob <- cumsum(freq_table$prob)
#-----------------------------------------------------------------
# Exponential fit
fit_ex <- 
  nls(variable ~ a*cumprob^m, data = freq_table, 
      start = list(a = 300,m=.15)) 
freq_table$pred_exp <- predict(fit_ex)
#-----------------------------------------------------------------
# Polynomial fit
fit_py <- 
  lm(freq_table$variable~poly(freq_table$cumprob,5,raw=TRUE))
freq_table$pred_poly <- predict(fit_py)
#-----------------------------------------------------------------
# GAM fit
fit_gm <- 
  mgcv::gam(variable ~ s(cumprob),data = freq_table)
freq_table$pred_gam <- predict(fit_gm)

#-----------------------------------------------------------------
# Apply logit fit
#-----------------------------------------------------------------
fit_lt <- nls(variable ~ SSlogis(cumprob, Asym, xmid, scal), 
              freq_table)
cof    <- coef(summary(fit_lt))

fit <- nls(variable ~ A/(1 + exp(((-I+cumprob)/S))), 
           data = freq_table,  
           start = list(A = cof[1],I= cof[2],S = -cof[3]), 
           control = list(maxiter  =  10000), trace=TRUE)

#-----------------------------------------------------------------
# Spline fit
#-----------------------------------------------------------------
fit_sp <- with(freq_table, smooth.spline(cumprob, variable))
freq_table$pred_sp <- predict(fit_sp)$y

#-----------------------------------------------------------------
# Observe fit data
#-----------------------------------------------------------------

dist_fits <- 
  ggplot(freq_table,aes(y = cumprob,x = variable))           +
  geom_point(alpha = .5,size = 1)                            +
  geom_line(aes(x = pred_exp), size = 1.1 , lty = 2, 
            color = 'dodgerblue')                            +
  geom_line(aes(x = pred_poly), size = 1.1, lty = 3, 
            color = 'mediumseagreen')                        + 
  geom_line(aes(x = pred_gam), size = 1.1, lty = 4, 
            color = 'coral')                                 +
  geom_line(aes(x = pred_sp), size = 1.1, lty =5, 
            color = 'orchid')                                +
  ylab('\n Cumulative Probability')                          + 
  xlab('Order time in seconds')                              + 
  ggtitle('Distribution of order times')                     + 
  graphics_theme_1

#-----------------------------------------------------------------
# Compare fits
#-----------------------------------------------------------------
models <- list(expon  = fit_ex,
               poly   = fit_py,
               gam    = fit_gm)

get_diagnostics <- function(mods){
  mods <- models
  aics   <- lapply(mods, function(x) AIC(x))
  bics   <- lapply(mods, function(x) BIC(x))  
  frame <- as.data.frame(matrix(nrow = length(mods), ncol = 3))
  frame[,1] <- names(mods)
  frame[,2] <- unlist(aics)
  frame[,3] <- unlist(bics)
  names(frame) <- c('model','AIC','BIC')
  return(frame)
  
}

models_table <- get_diagnostics(models)

#-----------------------------------------------------------------
# get fit
#-----------------------------------------------------------------
f_get_fifth_degree_fit <- function(new_var,dist_fit){
  var <-  coef(dist_fit)[1]              + 
    (coef(dist_fit)[2] * new_var    + 
       (coef(dist_fit)[3] * new_var^2) + 
       (coef(dist_fit)[4] * new_var^3) +
       (coef(dist_fit)[5] * new_var^4) + 
       (coef(dist_fit)[6] * new_var^5))
  return(var)
}

#-----------------------------------------------------------------
# Equation output
#-----------------------------------------------------------------
f_get_fifth_degree_fit(.7785714,fit_py)

#-----------------------------------------------------------------
# get fit
#-----------------------------------------------------------------
poly_fit <- 
  sapply(seq(0,1,by=.005),
         function(x) f_get_fifth_degree_fit(x,fit_py))

poly_values <- tibble::tibble(y = seq(0,1,by=.005),
                              x = poly_fit)
poly_graph <- 
  ggplot(poly_values,aes(x=x,y=y))           +
  geom_line(size = 1.5, lty = 3, 
            color = 'mediumseagreen')        +
  ylab('\n Cumulative Probability')          + 
  xlab('Order time in seconds')              + 
  ggtitle('Simulated order times')           + 
  graphics_theme_1

#-----------------------------------------------------------------
# Define terms for queuing equation
#-----------------------------------------------------------------
lambda                # Average arrivals per unit of time
mu                    # Average number of units processed 
k      <- 6           # Number of points of sale
N      <- 50          # Number that the queue can accommodate
tau_a  <- 1/lambda    # Average time between arrivals: 110 seconds
tau_s  <- 1/mu        # Average service time: 90 seconds
rho    <- tau_a/tau_s # Utilization ratio
n                     # Units in the system

#-----------------------------------------------------------------
# M/M/k/N Queue Inputs
#-----------------------------------------------------------------
k        = 2          # Number of points of sale at concept
N        = 5          # Number of people the queue can accommodate
tau_a    = 10         # time between arrivals: 40 seconds/60
tau_s    = 8          # Service time: 90 seconds/60 = 1.5
lambda   = 1/tau_a    # Customer Arrivals per minute
mu       = 1/tau_s    # Serviced customers per minute
#-----------------------------------------------------------------
# Translate to per hour figures
#-----------------------------------------------------------------
lambda_h = 60/tau_a   # Per hour
mu_h     = 60/tau_s   # Per hour
rho      = lambda/mu  # Utilization ratio

#-----------------------------------------------------------------
# M/M/k/N Calculating the Probability that n = 0
#-----------------------------------------------------------------
# Create the sequence for the P0 equation
n   = seq(0, N-1, by = 1 )
# Translate the equation into R:
P0 <- 
1/ sum(((rho^n)/factorial(n)) + ((rho^k)/
(factorial(k)*((k^(N-k+1)) - (rho^(N-k+1))/((k-rho)*(k^(N-k)))))))

#-----------------------------------------------------------------
# M/M/k/N  Probability of n units in the system
#-----------------------------------------------------------------

# For n = (0,k)
nk0 = seq(0, k, by = 1 )
Pnk0 <- rho^nk0/factorial(nk0)*P0

sum(Pnk0)

# For n = (k + 1,N)
nk1 = seq(k + 1, N, by = 1 )
Pnk1 <- rho^nk1/(factorial(k)*k^(nk1-k))*P0

sum(Pnk1)

round(sum(Pnk0,Pnk1),2)

#-----------------------------------------------------------------
# Calculate figures
#-----------------------------------------------------------------
lambda_e <- lambda*(1 - Pnk1) # Lambda Effective
rho_e    <- lambda_e/mu       # rho Effective
# Expected in queue
Lq = sum((n-k)*Pnk0)
Lq = sum((n-k)*Pnk1)
Ls = rho_e
# expected units in the system
L = Ls + Lq
# Expected service time
Ws = Ls/lambda_e # minutes in service
Wq = Lq/lambda_e # minutes in queue
W = L/lambda_e   # minutes in system


#-----------------------------------------------------------------
# Calculate figures
#-----------------------------------------------------------------
f_get_MMKN <- function(k,N,ta,ts){
  
  lambda = 1/ta #: per minute
  mu     = 1/ts #: per minute
  rho    = lambda/mu #: utilization ratio
  
  #-----------------------------------------------------------------
  # Probability of n units in the system
  # for
  n = seq(0, N-1, by = 1 )
  P0 <- 1/ sum(((rho^n)/factorial(n)) + 
                 ((rho^k)/(factorial(k)*((k^(N-k+1)) - 
                                           (rho^(N-k+1))/((k-rho)*(k^(N-k)))))))
  
  # Probability of n units in the system
  # for
  n = seq(0, k, by = 1 )
  Pn0 <- rho^n/factorial(n)*P0
  
  # for
  n = seq(k + 1, N, by = 1 )
  Pn1 <- rho^n/(factorial(k)*k^(n-k))*P0
  
  Pn      <- c(Pn0,Pn1)
  
  #-------------------------------------------------------------------
  # calculations
  len     <- max(length(Pn))
  
  lambda_e  <- lambda*(1 - Pn[len])
  rho_e    <- lambda_e/mu
  
  # Expected in queue
  Ls = rho_e #   Ls = 1*Pn[2] + 2*sum(Pn[-c(1,2)])
  
  # for
  n = seq(k+2, N + 1, by = 1 )
  Lq = sum((n-(k+1))*Pn[n]) # Lq = 1*Pn[4] + 2*Pn[5] + 3*Pn[6]
  
  # expected units in the system
  L = Ls + Lq
  
  # Expected service time
  Ws = Ls/lambda_e # minutes in service
  Wq = Lq/lambda_e # minutes in queue
  W  = Wq + Ws   # minutes in system
  
  #-------------------------------------------------------------------
  # Build output
  frame <- data.frame(matrix(nrow = 7,ncol =2))
  names(frame) <- c('Metric','Value')
  
  metric <- c('Servers:','System Capacity:','Time between arrivals:',
              'Average service time:','Minutes in service:',
              'Minutes in queue:','Minutes in system:')
  values <- c(k,N,ta,ts,Ws,Wq,W)
  
  frame[,1] <- metric
  frame[,2] <- values
  
  return(frame)
  
}

#-----------------------------------------------------------------
# Run our function
#-----------------------------------------------------------------
FOSBAAS::f_get_MMKN(2,5,10,8)









=======
#-----------------------------------------------------------------
# This file contains all code from the FOSBAAS book
# Justin Watkins
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Chapter 1
#-----------------------------------------------------------------


library(ggplot2)
library(dplyr)
#-----------------------------------------------------------------
# creating a color palette
#-----------------------------------------------------------------
palette <- c('dodgerblue','grey25','mediumseagreen', 'coral',
             'orchid','firebrick','goldenrod','cyan',
             'brown','steelblue','magenta')
#-----------------------------------------------------------------
# Creating a custom theme
#-----------------------------------------------------------------
require(ggplot2)
graphics_theme_1 <- ggplot2::theme() + 
  theme(axis.text.x  = element_text(angle = 0, size = 14, 
                                    vjust = 0, color = "grey10"),  
        axis.text.y  = element_text(angle = 0, size = 14, 
                                    vjust = 0, color = "grey10"),  
        axis.title.x = element_text(size = 16, face = "plain", 
                                    colour = "grey10"), 
        axis.title.y = element_text(size = 16, face = "plain", 
                                    color = "grey10"), 
        legend.title = element_text(size = 14, face = "plain", 
                                    color = "grey10"), 
        legend.text  = element_text(size = 11, 
                                    color = "grey10"), 
        plot.title   = element_text(colour = "grey10", 
                                    size = 14, angle = 0, 
                                    hjust = .5, vjust = .5, 
                                    face = "bold"), 
        legend.position   = "right", 
        legend.background = element_rect(fill = "grey99", 
                                         size = 3,  
                                         linetype= "solid", 
                                         colour = "grey99"), 
        legend.key        = element_rect(fill = "grey99", 
                                         color = "grey99"), 
        strip.background  = element_rect(fill =  "grey99", 
                                         colour = "grey99"), 
        strip.text        = element_text(size = 14, 
                                         face = "plain", 
                                         color = "grey10"), 
        panel.grid.major  = element_line(colour = "grey80"),  
        panel.grid.minor  = element_line(colour = "grey80"), 
        panel.background  = element_rect(fill = "grey99", 
                                         colour = "grey99"), 
        plot.background   = element_rect(fill = "grey99", 
                                         colour = "grey99"))

#-----------------------------------------------------------------
#-- SQL example
#-----------------------------------------------------------------
#SELECT 
#  A.customer_id
# ,A.email_addr
# ,B.plan_id
# ,B.price
#FROM Customer A LEFT JOIN Plans B ON A.customer_id = B.customer_id
#WHERE A.email_addr = "Ted.Williams@someserver.com"

#-----------------------------------------------------------------
#-- SQL example
#-----------------------------------------------------------------
#SELECT 
#  A.customer_id,
# ,A.ticketing_system_id
# ,B.deal_id
# ,C.opportunity_id
#FROM Customer A LEFT JOIN deal B ON A.customer_id = B.customer_id
#LEFT JOIN opportunity C ON B.deal_id = C.deal_id
#WHERE A.email_addr = "Ted.Williams@someserver.com"

#-----------------------------------------------------------------
# Demonstrate a linear equation Y = mx + b
#-----------------------------------------------------------------
x        <- c(-5,-4,-3,-2,-1,0,1,2,3,4,5) # x values
m        <- 2                             # Slope
b        <- 5                             # Y Intercept
y_values <- list()                        # Holds the output

i <- 1
while(i <= length(x)){
  y_values[i] <- x[i]*m + b
  i <- i + 1
}

line_values <- data.frame(x,unlist(y_values))
names(line_values) <- c('x', 'y')

#-----------------------------------------------------------------
# Linear equation output
#-----------------------------------------------------------------
lin_equ_out <- 
  ggplot(data = line_values, aes(x = x, y = y))  + 
  geom_line(color  = 'dodgerblue')               +
  geom_point(color = 'grey15', size = 2)         +
  graphics_theme_1

#-----------------------------------------------------------------
# Linear equation output
#-----------------------------------------------------------------
set.seed(101010)
lin_equ_out_b <- 
  ggplot(data = line_values, aes(x = x, y = y) )  + 
  geom_line(color   = 'dodgerblue')               +
  geom_jitter(color = 'grey15',height = 3,
              width = 0,size = 2)                 +
  graphics_theme_1

#-----------------------------------------------------------------
# Customer renewal data
#-----------------------------------------------------------------
renewal_data <- FOSBAAS::customer_renewals

#-----------------------------------------------------------------
# Customer renewal data
#-----------------------------------------------------------------
d_tree <- 
  rpart::rpart(formula = renewed ~ distance, 
               method  = "class",
               data    = renewal_data)
rpart.plot::rpart.plot(d_tree,
                       type  = 4,
                       extra = 101)

#-----------------------------------------------------------------
# percap scatter plot
#-----------------------------------------------------------------
set.seed(714)
percap_data <- tibble::tibble(
  percap = rnorm(81,40,10),
  revenue = rnorm(81,1000000,200000)
)
x_label  <- ('\n Percap')
y_label  <- ('Revenue \n')
title    <- ('Revenue vs. Percap')
scatter_percap <- 
  ggplot2::ggplot(data  = percap_data, 
                  aes(x = percap, 
                      y = revenue))                   +
  geom_point(alpha = .9, color = 'dodgerblue')        +
  geom_rug(color = 'coral')                           +
  scale_y_continuous(label = scales::dollar)          +
  scale_x_continuous(label = scales::dollar)          +
  xlab(x_label)                                       + 
  ylab(y_label)                                       + 
  ggtitle(title)                                      +
  graphics_theme_1

#-----------------------------------------------------------------
# Chapter 2
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Linear equation function
#-----------------------------------------------------------------
f_linear_equation <- function(x,slope,yIntercept){
  y <- slope*x + yIntercept
  return(y)
}

#-----------------------------------------------------------------
# Linear equation function inputs
#-----------------------------------------------------------------
f_linear_equation( x          = 2,
                   slope      = 10,
                   yIntercept = 7) 

#-----------------------------------------------------------------
# Create lead scoring data
#-----------------------------------------------------------------
library(FOSBAAS)
f_create_lead_scoring_data(714, 
                           5000,
                           "2021",
                           f_calculate_tenure,
                           f_calculate_spend,
                           f_calculate_ticket_use,
                           f_renewal_assignment,
                           f_assign_renewal,
                           renew = T)



#-----------------------------------------------------------------
# Chapter 3
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# High level schedule data
#-----------------------------------------------------------------
library(FOSBAAS)
season_data <- FOSBAAS::season_data

#-----------------------------------------------------------------
# Histograms
#-----------------------------------------------------------------
x_label  <- ('\n Ticket Sales')
y_label  <- ('Count \n')
title    <- ('Distribution of Seasonal Ticket Sales')
legend   <- ('Season')
hist_sales <- 
  ggplot2::ggplot(data  = season_data,
                  aes(x = ticketSales,
                      fill  = factor(season)))         +
  geom_histogram(binwidth = 1000)                      +
  scale_fill_manual(legend, values = palette)          +
  geom_rug(color = 'coral')                            +
  scale_x_continuous(label = scales::comma)            +
  scale_y_continuous(label = scales::comma)            +
  xlab(x_label)                                        + 
  ylab(y_label)                                        + 
  ggtitle(title)                                       +
  graphics_theme_1

#-----------------------------------------------------------------
# Kernel density plot
#-----------------------------------------------------------------
x_label  <- ('\n Ticket Sales')
y_label  <- ('Density \n')
title    <- ('Distribution of Seasonal Ticket Sales')
legend   <- ('Season')
density_sales <- 
  ggplot2::ggplot(data = season_data, 
                  aes(x    = ticketSales, 
                      fill = factor(season)))                +
  geom_density(alpha = .5)                                   +
  scale_fill_manual(legend,values = palette)                 +
  geom_rug(color = 'coral')                                  +
  scale_x_continuous(label = scales::comma)                  +
  scale_y_continuous(label = scales::percent)                +
  xlab(x_label)                                              + 
  ylab(y_label)                                              + 
  ggtitle(title)                                             +
  graphics_theme_1

#-----------------------------------------------------------------
# Demonstrate AUC
#-----------------------------------------------------------------
den      <- density(season_data$ticketSales)
bin_size <- (den$x[2] - den$x[1])

round(sum(den$y) * bin_size,2) # Approximates to 1

#-----------------------------------------------------------------
# Demonstrate AUC at 40,000 Tickets
#-----------------------------------------------------------------
sum(den$y[den$x >= 40000]) * bin_size

#-----------------------------------------------------------------
# Faceting a plot
#-----------------------------------------------------------------
x_label  <- ('\n Ticket Sales')
y_label  <- ('Count \n')
title    <- ('Distribution of Seasonal Ticket Sales')
histogram_sales_facet <- 
  ggplot2::ggplot(data = season_data, 
                  aes(x = ticketSales))                        +
  facet_grid(season ~ .)                                       +
  geom_histogram(binwidth = 1000, fill = palette[1])           +
  geom_rug(color = 'coral')                                    +
  scale_x_continuous(label = scales::comma)                    +
  scale_y_continuous(label = scales::comma)                    +
  xlab(x_label)                                                + 
  ylab(y_label)                                                + 
  ggtitle(title)                                               +
  graphics_theme_1 

#-----------------------------------------------------------------
# Box plots
#-----------------------------------------------------------------
x_label  <- ('\n Season')
y_label  <- ('Ticket Sales \n')
title    <- ('Distribution of Seasonal Ticket Sales')
box_sales <- 
  ggplot2::ggplot(data  = season_data, 
                  aes(x = factor(season), 
                      y = ticketSales))               +
  geom_boxplot(fill = 'dodgerblue')                   +
  geom_jitter(alpha = .2,  height = 0, 
              width = .25, color  = 'coral')          +
  geom_rug(color = 'coral')                           +
  scale_y_continuous(label = scales::comma)           +
  xlab(x_label)                                       + 
  ylab(y_label)                                       + 
  ggtitle(title)                                      +
  graphics_theme_1

#-----------------------------------------------------------------
# violin plot
#-----------------------------------------------------------------
x_label  <- ('\n Season')
y_label  <- ('Ticket Sales \n')
title    <- ('Distribution of Seasonal Ticket Sales')
violin_sales <-
  ggplot2::ggplot(data = season_data, 
                  aes(x = factor(season), 
                      y = ticketSales))             +
  geom_violin(fill = 'dodgerblue')                  +
  geom_jitter(alpha = .35, height = 0, 
              width = .25, color = 'coral')         +
  geom_rug(color = 'coral')                         +
  scale_y_continuous(label = scales::comma)         +
  xlab(x_label)                                     + 
  ylab(y_label)                                     + 
  ggtitle(title)                                    +
  graphics_theme_1

#-----------------------------------------------------------------
# Line plot
#-----------------------------------------------------------------
x_label <- ('\n Game Number')
y_label <- ('Ticket Sales \n')
title   <- ('Ticket Sales by Game Number')
legend  <- 'Season'
line_sales <- 
  ggplot2::ggplot(data      = season_data, 
                  aes(x     = gameNumber,
                      y     = ticketSales,
                      color = factor(season)))             +
  geom_line(size = .9)                                     +
  scale_color_manual(legend, values = palette)             +
  scale_y_continuous(label = scales::comma)                +
  xlab(x_label)                                            + 
  ylab(y_label)                                            + 
  ggtitle(title)                                           +
  graphics_theme_1 + theme(legend.position   = "bottom")

#-----------------------------------------------------------------
# Tile plot or heat map
#-----------------------------------------------------------------
x_label <- ('\n Day of Week')
y_label <- ('Month \n')
title   <- ('Ticket Sales by Day and Month')
# compress data into an easier format
sd_comp <- season_data                    %>% 
  select(dayOfWeek,month,ticketSales)     %>%
  group_by(dayOfWeek,month)               %>%
  summarise(avgSales = mean(ticketSales))

tile_sales <- 
  ggplot2::ggplot(data     = sd_comp, 
                  aes(x    = dayOfWeek,
                      y    = month,
                      fill = avgSales))                       +
  geom_tile()                                                 +
  scale_fill_gradient(low = "white", high = "dodgerblue",
                      name = 'Tickets',label = scales::comma) +
  xlab(x_label)                                               + 
  ylab(y_label)                                               + 
  ggtitle(title)                                              +
  graphics_theme_1

#-----------------------------------------------------------------
# Hexplots
#-----------------------------------------------------------------
x_label  <- ('\n Game Number')
y_label  <- ('Ticket Sales \n')
title   <- ('Ticket Sales by game')

hex_sales <- 
  ggplot2::ggplot(data     = season_data, 
                  aes(x    = gameNumber,
                      y    = ticketSales))                    +
  geom_hex()                                                  +
  scale_fill_gradient(low = "dodgerblue", high = "coral",
                      name = 'Count',label = scales::comma)   +
  scale_y_continuous(label = scales::comma)                   +
  xlab(x_label)                                               + 
  ylab(y_label)                                               + 
  ggtitle(title) 

#-----------------------------------------------------------------
# Bar plot version one
#-----------------------------------------------------------------
x_label  <- ('\n Season')
y_label  <- ('Proportion of Ticket Sales \n')
title    <- ('Proportion of Ticket Sales by DOW')
bar_sales_pro <- 
  ggplot2::ggplot(data     = season_data, 
                  aes(y    = ticketSales,
                      x    = season,
                      fill = weekEnd))                   +
  geom_bar(stat = 'identity',position = 'fill')          +
  scale_fill_manual(values = palette, name = 'Weekend')  +
  scale_y_continuous(label = scales::percent)            +
  xlab(x_label)                                          + 
  ylab(y_label)                                          + 
  ggtitle(title)                                         +
  graphics_theme_1 + theme(legend.position   = "bottom")

#-----------------------------------------------------------------
# Bar plot version two
#-----------------------------------------------------------------
x_label  <- ('\n Season')
y_label  <- ('Ticket Sales \n')
title    <- ('Ticket Sales by DOW')
bar_sales <- 
  ggplot2::ggplot(data     = season_data, 
                  aes(y    = ticketSales,
                      x    = season,
                      fill = weekEnd))                  +
  geom_bar(stat = 'identity', position = 'stack')       +
  scale_fill_manual(values = palette, name = 'Weekend') +
  scale_y_continuous(label = scales::comma)             +
  xlab(x_label)                                         + 
  ylab(y_label)                                         + 
  ggtitle(title)                                        +
  graphics_theme_1 + theme(legend.position   = "bottom")

#-----------------------------------------------------------------
# Creating a summary statistics table
#-----------------------------------------------------------------
library(dplyr)
average_by_dow <- 
  FOSBAAS::season_data                          %>% 
  group_by(dayOfWeek)                           %>% 
  summarise(AverageSales = mean(ticketSales))

#-----------------------------------------------------------------
# Creating a summary statistics table using 'by'
#-----------------------------------------------------------------
by(FOSBAAS::season_data$ticketSales,
   FOSBAAS::season_data$dayOfWeek,function(x) mean(x))

#-----------------------------------------------------------------
# Getting quantiles
#-----------------------------------------------------------------
quants <- quantile(FOSBAAS::season_data$ticketSales, 
                   probs = c(0,.10,.25,.5,.75,.9,1))
quants

#-----------------------------------------------------------------
# Converting to wide format
#-----------------------------------------------------------------
library(dplyr)
library(tidyr)
team_dow <- 
  FOSBAAS::season_data                                   %>%
  select(team,dayOfWeek,ticketSales)                     %>%
  filter(team %in% c('SF','BAL'))                        %>%
  group_by(team,dayOfWeek)                               %>%
  summarise(medianSales = median(ticketSales),
            games       = n())                           %>%
  tidyr::pivot_wider(names_from  = team,
                     values_from = c(medianSales,games)) %>%
  mutate(difference = medianSales_BAL - medianSales_SF)  %>%
  arrange(difference) 

#-----------------------------------------------------------------
# Converting to long format
#-----------------------------------------------------------------
library(dplyr)
library(tidyr)
team_dow_long <- 
  team_dow                                          %>%
  select(dayOfWeek, medianSales_BAL,medianSales_SF) %>%
  tidyr::pivot_longer(!dayOfWeek, 
                      names_to  = "club", 
                      values_to = "medianSales")

#-----------------------------------------------------------------
# Summary stats psych
#-----------------------------------------------------------------
library(psych)
psy_desc <- 
  t(data.frame(psych::describe(FOSBAAS::season_data$ticketSales)))

#-----------------------------------------------------------------
# Summary stats Hmisc
#-----------------------------------------------------------------
hmisc_desc <- (Hmisc::describe(FOSBAAS::season_data$ticketSales))
hmisc_desc <- unlist(hmisc_desc$counts)
hmisc_desc <- as.data.frame(hmisc_desc)

# Unload a package
# unloadNamespace("Hmisc")
# detach("package:Hmisc")

#-----------------------------------------------------------------
# Build a frequency table
#-----------------------------------------------------------------
table(FOSBAAS::season_data$promotion)

#-----------------------------------------------------------------
# One Way ANOVA
#-----------------------------------------------------------------
mod <- aov(ticketSales ~ promotion + dayOfWeek,
           data = FOSBAAS::season_data)
#summary(mod)

#-----------------------------------------------------------------
# Viewing group means
#-----------------------------------------------------------------
library(dplyr)
graph_table <- FOSBAAS::season_data %>%
  
  select(promotion,ticketSales,dayOfWeek,daysSinceLastGame)  %>%
  group_by(promotion,dayOfWeek)                              %>%
  summarise(sales = mean(ticketSales),
            daysSinceLastGame = mean(daysSinceLastGame),
            N = n(),
            sd = sd(ticketSales),
            se = sd/sqrt(N))         

x_label <- 'Day of Week'                                             
y_label <- 'Mean ticket sales'                                            
title   <- 'Group means and standard error: promos and sales'
se <- 
  ggplot(graph_table, aes(y=sales, 
                          x=reorder(dayOfWeek,sales,mean), 
                          color=promotion))                 + 
  geom_errorbar(aes(ymin = sales-se, ymax = sales+se), 
                width =.3,size = 1,
                position = position_dodge(0.25))            +
  geom_point()                                              +
  scale_color_manual(values = palette)                      +
  scale_y_continuous(label = scales::comma)                 +
  xlab(x_label)                                             + 
  ylab(y_label)                                             + 
  ggtitle(title)                                            +
  graphics_theme_1 

#-----------------------------------------------------------------
# Summary stats psych
#-----------------------------------------------------------------
tu_test <- TukeyHSD(mod)

#-----------------------------------------------------------------
# Viewing group means
#-----------------------------------------------------------------

x_label <- 'Value'                                             
y_label <- 'Promotion Comps'                                            
title   <- '95% CI comps by promotion '
tu_comp_graph <- 
  ggplot(tu_comp, aes(x=diff, 
                      y=promotion))                         + 
  geom_errorbar(aes(xmin = lwr, xmax = upr), 
                width =.3,size = 1)                         +
  geom_point()                                              +
  scale_x_continuous(label = scales::comma)                 +
  xlab(x_label)                                             + 
  ylab(y_label)                                             + 
  ggtitle(title)                                            +
  geom_vline(xintercept = 0,color = 'red',lty = 2)          +
  graphics_theme_1 

#-----------------------------------------------------------------
# Creating a simple linear model
#-----------------------------------------------------------------
ln_mod <- lm(ticketSales ~ team+dayOfWeek+month+
             daysSinceLastGame+openingDay+promotion,
             data = FOSBAAS::season_data)
stats_ticket_sales <- 
  tibble::tibble(st_error     = unlist(summary(ln_mod)[6]),
                 r_square     = unlist(summary(ln_mod)[8]),
                 adj_r_square = unlist(summary(ln_mod)[9]),
                 f_stat       = summary(ln_mod)$fstatistic[1])

#-----------------------------------------------------------------
# Using the regression output
#-----------------------------------------------------------------
seasons <- FOSBAAS::season_data
seasons$pred <- predict(ln_mod)

x_label <- 'Actual Sales'                                             
y_label <- 'Predicted Sales'                                            
title   <- 'Actual Sales vs. Predictions'
legend   <- ('Season')
sales_mod <- 
  ggplot(seasons, aes(x = ticketSales, 
                      y = pred,
                      color = factor(season)))              + 
  geom_point()                                              +
  stat_smooth(method = 'lm', se = T)                        +
  scale_color_manual(legend,values = palette)               +
  scale_x_continuous(label = scales::comma)                 +
  scale_y_continuous(label = scales::comma)                 +
  xlab(x_label)                                             + 
  ylab(y_label)                                             + 
  ggtitle(title)                                            +
  graphics_theme_1 

#-----------------------------------------------------------------
# Chapter 4
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Aggregated CRM data
#-----------------------------------------------------------------
ag_sales_data <- FOSBAAS::aggregated_crm_data

library(dplyr)
agg_calls <- 
  ag_sales_data                     %>% 
  group_by(repID)                   %>%
  summarise(calls   = sum(call),
            revenue = sum(revenue)) %>%
  mutate(revByCall = revenue/calls) %>%
  arrange(desc(revByCall))

#-----------------------------------------------------------------
# Correlation coefficient
#-----------------------------------------------------------------
cor(agg_calls$calls,agg_calls$revenue)

#-----------------------------------------------------------------
# Box plots of revenue by sales rep
#-----------------------------------------------------------------
x_label <- 'Rep ID'                                             
y_label <- 'Revenue by sale'                                            
title   <- 'Revenue by sale by rep'
sales_box <- 
  ggplot(ag_sales_data, aes(y=revenue, 
                            x=factor(repID)))               +
  geom_boxplot(fill = palette[1])                           +
  scale_color_manual(values = palette)                      +
  scale_y_continuous(label = scales::dollar)                +
  xlab(x_label)                                             + 
  ylab(y_label)                                             + 
  ggtitle(title)                                            +
  graphics_theme_1                                          +
  theme(axis.text.x  = element_text(angle = 90, size = 8, 
                                    vjust = 0,
                                    color = "grey10"))

#-----------------------------------------------------------------
# Cumulative revenue by rep by customer
#-----------------------------------------------------------------
ag_sales_line <- 
  ag_sales_data %>% group_by(repID) %>%
  mutate(cumSum = cumsum(revenue),
         observation = seq(1:500))

x_label <- 'Customer'                                             
y_label <- 'Revenue'                                            
title   <- 'Revenue generated per rep by customer'

sales_line <- 
  ggplot(ag_sales_line, aes(y     = cumSum, 
                            x     = factor(observation),
                            group = repID,
                            color = repID))                 +
  geom_line()                                               +
  scale_color_manual(values = palette,guide = FALSE)        +
  scale_y_continuous(label = scales::dollar)                +
  xlab(x_label)                                             + 
  ylab(y_label)                                             + 
  ggtitle(title)                                            +
  graphics_theme_1                                          +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major  = element_line(colour = "white"),  
        panel.grid.minor  = element_line(colour = "grey80"))

#-----------------------------------------------------------------
# Failed calls and customers by rep
#-----------------------------------------------------------------
failures <- 
  ag_sales_data                      %>% 
  group_by(repID)                    %>%
  filter(revenue == 0)               %>%
  summarise(failedCalls = sum(call),
            failedCusts = n())       %>%
  tidyr::pivot_longer(!repID, 
                      names_to  = "failure", 
                      values_to = "value")
x_label  <- ('\n Rep')
y_label  <- ('Count \n')
title    <- ('Failures by rep')
bar_sales <- 
  ggplot2::ggplot(data     = failures, 
                  aes(y    = value, 
                      x    = reorder(repID,value,sum),
                      fill = failure))                  +
  geom_bar(stat = 'identity', position = 'dodge')       +
  scale_fill_manual(values = palette, name = 'failure') +
  scale_y_continuous(label = scales::comma)             +
  xlab(x_label)                                         + 
  ylab(y_label)                                         + 
  ggtitle(title)                                        +
  coord_flip()                                          +
  graphics_theme_1                                      + 
  theme(legend.position   = "bottom")

#-----------------------------------------------------------------
# quantiles of sales
#-----------------------------------------------------------------
quants <- quantile(ag_sales_data$revenue,
                   probs = c(.5,.75,.9,.95,.975,.99,1))
quants

#-----------------------------------------------------------------
# quantiles of sales
#-----------------------------------------------------------------
descriptives <- 
psych::describe.by(ag_sales_data[which(ag_sales_data$repID %in% 
                    c("0LK62LATB8E3","AFA0Z9M2M4LQ")),]$revenue,
                   ag_sales_data[which(ag_sales_data$repID %in% 
                    c("0LK62LATB8E3","AFA0Z9M2M4LQ")),]$repID)

#-----------------------------------------------------------------
# description of sales
#-----------------------------------------------------------------
descriptives$`0LK62LATB8E3`[c(3,4,5,6,9)]

#-----------------------------------------------------------------
# description of sales
#-----------------------------------------------------------------
descriptives$AFA0Z9M2M4LQ[c(3,4,5,6,9)]

#-----------------------------------------------------------------
# quantiles of sales
#-----------------------------------------------------------------

ag_sales_data                                          %>% 
  filter(repID %in% c('0LK62LATB8E3','AFA0Z9M2M4LQ'),
         revenue >= quants[6])                         %>%
  group_by(repID)                                      %>%
  summarise(highRevenue  = sum(revenue),
            countRevenue = n())


#-----------------------------------------------------------------
# Clustering algorithm applied in python
#-----------------------------------------------------------------
# from sklearn.cluster import AgglomerativeClustering
# data = data.sample(n=1500)
# cluster = AgglomerativeClustering(n_clusters=6, 
#                                  affinity='euclidean', 
#                                  linkage='ward')  
# cl = py.DataFrame(cluster.fit_predict(data))

#-----------------------------------------------------------------
# kmeans algorithm applied in python
#-----------------------------------------------------------------
# from sklearn.cluster import KMeans
# data = data.sample(n=1500)
# cluster = KMeans(n_clusters=6, random_state=0)
# cl = py.DataFrame(cluster.fit_predict(data))

#-----------------------------------------------------------------
# Hierarchical clustering algorithm applied in R
#-----------------------------------------------------------------
library(stats)
library(cluster)
data         <- sample(data, 1500)
mod_data     <- cluster::daisy(data)
cl           <- stats::hclust(mod_data, method = "ward.D")
cuts         <- cutree(cl, k = 6)
data$cluster <- cuts

#-----------------------------------------------------------------
# k means algorithm applied in R
#-----------------------------------------------------------------
library(stats)
data         <- sample(data, 1500)
cl           <- stats::kmeans(data, centers = 6)
data$cluster <- cl$cluster

#-----------------------------------------------------------------
# Batch script for automation
#-----------------------------------------------------------------
# REM This command will call an R Script from a program or 
# scheduling tool

# "C:\Program Files\R\R-Version\bin\x64\R.exe" CMD BATCH 
#  --vanilla --slave ""C:\locationOfScript\YourRScript.R"

#-----------------------------------------------------------------
# Chapter 5
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Read data to use for segmentation
#-----------------------------------------------------------------
library(FOSBAAS)
demo_data  <- FOSBAAS::demographic_data

#-----------------------------------------------------------------
# Take a look at the structure of the data
#-----------------------------------------------------------------
str(demo_data)

#-----------------------------------------------------------------
# subset the data
#-----------------------------------------------------------------
library(dplyr)
demo_data <- demo_data %>% select(custID,ethnicity,age,
                                  maritalStatus,children,
                                  hhIncome,distance,gender)

#-----------------------------------------------------------------
# Histogram of the age of fans
#-----------------------------------------------------------------
x_label  <- ('\n Age')
y_label  <- ('Count \n')
title    <- ('Distribution of age (demographic)')
hist_age <- 
  ggplot2::ggplot(data=demo_data,aes(x=age,fill=gender))  +
  geom_histogram(binwidth = 2)                            +
  scale_fill_manual(values = palette)                     +
  geom_rug(color = 'coral')                               +
  scale_x_continuous(label = scales::comma)               +
  scale_y_continuous(label = scales::comma)               +
  xlab(x_label)                                           + 
  ylab(y_label)                                           + 
  ggtitle(title)                                          +
  graphics_theme_1

#-----------------------------------------------------------------
# Get summary statistics
#-----------------------------------------------------------------
library(psych)
descript   <- psych::describe(demo_data$age)
descriptMF <- psych::describeBy(demo_data$age,demo_data$gender)

#-----------------------------------------------------------------
# Distance from our facility
#-----------------------------------------------------------------
library(ggplot2)
x_label   <- ('\n Distance')
y_label   <- ('Count \n')
title     <- ('Distribution of distance (demographic)')
hist_dist <- 
  ggplot2::ggplot(data=demo_data,aes(x=distance))            +
  geom_histogram(binwidth = 2,fill='dodgerblue')             +
  geom_rug(color = 'coral')                                  +
  scale_x_continuous(label = scales::comma)                  +
  scale_y_continuous(label = scales::comma)                  +
  xlab(x_label)                                              + 
  ylab(y_label)                                              + 
  ggtitle(title)                                             +
  graphics_theme_1

#-----------------------------------------------------------------
# Use the psych package to generate summary statistics
#-----------------------------------------------------------------
descript <- psych::describe(demo_data$distance)

#-----------------------------------------------------------------
# Histogram of income
#-----------------------------------------------------------------
x_label  <- ('\n Household Income')
y_label  <- ('Count \n')
title    <- ('Distribution of Income (demographic)')
hist_income <- 
  ggplot2::ggplot(data=demo_data,aes(x=hhIncome)) +
  geom_histogram(binwidth = 2,fill='dodgerblue')  +
  geom_rug(color = 'coral')                       +
  scale_x_continuous(label = scales::comma)       +
  scale_y_continuous(label = scales::comma)       +
  xlab(x_label)                                   + 
  ylab(y_label)                                   + 
  ggtitle(title)                                  +
  graphics_theme_1

#-----------------------------------------------------------------
# summary statistics of hhincome
#-----------------------------------------------------------------
description <- psych::describe(demo_data$hhIncome)

#-----------------------------------------------------------------
# Generate summary stats of multiple values
#-----------------------------------------------------------------
numerical_cols <- names(dplyr::select_if(demo_data, is.numeric))
description    <- psych::describe(demo_data[numerical_cols])

#-----------------------------------------------------------------
# build proportion table for categorical data
#-----------------------------------------------------------------
description      <- table(demo_data$ethnicity)
description_prop <- prop.table(description)

#-----------------------------------------------------------------
# use describeBy to generate statistics tabulated by a factor
#-----------------------------------------------------------------
numerical_cols <- names(dplyr::select_if(demo_data, is.numeric))
description    <- psych::describeBy(demo_data[numerical_cols], 
                                    demo_data$ethnicity)
deth <- rbind(description$aa[,c(2,3,4,5,6)], 
              description$w[,c(2,3,4,5,6)])

#-----------------------------------------------------------------
# Write a function to generate stats on missing data
#-----------------------------------------------------------------
f_missing_data <- function(dF){
  
  na  <- sum(apply(dF,2,function(x) is.na(x)))
  nan <- sum(apply(dF,2,function(x) is.nan(x)))
  inf <- sum(apply(dF,2,function(x) is.infinite(x)))
  
  missing        <- as.data.frame(rbind(na,nan,inf))
  names(missing) <- 'Values'
  return(missing)
}

missing <- f_missing_data(demo_data)

#-----------------------------------------------------------------
# Randomly make some values in data frame NA
#-----------------------------------------------------------------
set.seed(755)
demo_data[] <- 
 apply(demo_data,1:2, function(x) ifelse(runif(1,0,1) > .98,NA,x))

#-----------------------------------------------------------------
# Randomly make some values in dataframe NA
#-----------------------------------------------------------------
missing <- f_missing_data(demo_data)

#-----------------------------------------------------------------
# Print the number of missing values for each column
#-----------------------------------------------------------------
for(i in names(demo_data)){
  print(paste(sum(is.na(demo_data[,i])),names(demo_data[i])))
}

#-----------------------------------------------------------------
# Only keep rows with no NAs in any column
#-----------------------------------------------------------------
complete_cases <- nrow(demo_data[complete.cases(demo_data),])

#-----------------------------------------------------------------
# Simple imputation. Make any NAs 'w'
#-----------------------------------------------------------------
demo_data$ethnicity[is.na(demo_data$ethnicity)] <- 'w'

#-----------------------------------------------------------------
# Make an y missing age values the mean of overall age
#-----------------------------------------------------------------
mA <- 
  round(mean(as.numeric(as.character(na.omit(demo_data$age)))),0)
demo_data$age[is.na(demo_data$age)] <- mA

#-----------------------------------------------------------------
# Impute values based on a proportion
#-----------------------------------------------------------------
l <- 
  length(demo_data$maritalStatus[is.na(demo_data$maritalStatus)])
demo_data$maritalStatus[is.na(demo_data$maritalStatus)] <- 
  sample(c('m','s'), size=l, prob=c(.6,.4), replace=TRUE)

#-----------------------------------------------------------------
# Impute remaining values based on a proportion
#-----------------------------------------------------------------
#prop.table(table(demo_data$children))
#prop.table(table(demo_data$gender))

l <- length(demo_data$children[is.na(demo_data$children)])
demo_data$children[is.na(demo_data$children)] <- 
  sample(c('n','y'), size=l, prob=c(.52,.48), replace=TRUE)

l <- length(demo_data$gender[is.na(demo_data$gender)])
demo_data$gender[is.na(demo_data$gender)] <- 
  sample(c('m','f'), size=l, prob=c(.5,.5), replace=TRUE)

#-----------------------------------------------------------------
# Impute numerical values using the mice package
#-----------------------------------------------------------------
library(mice)
demo_data$distance <- as.numeric(demo_data$distance)
imp_frame          <- as.data.frame(cbind(demo_data$hhIncome,
                                          demo_data$distance))
imp_frame[]        <- apply(imp_frame,2,function(x) as.numeric(x))

imputed_Data       <- mice(imp_frame, m=5, maxit = 10,
                           method = 'pmm', seed = 755,
                           printFlag = F)
new_Data <- complete(imputed_Data,2)
demo_data$hhIncome <- new_Data$V1
demo_data$distance <- new_Data$V2

#-----------------------------------------------------------------
# Remove any NA values
#-----------------------------------------------------------------
demo_data <- na.omit(demo_data)

#-----------------------------------------------------------------
# Look for NA values
#-----------------------------------------------------------------
missing <- f_missing_data(demo_data)

#-----------------------------------------------------------------
# Create a new data frame to work with
#-----------------------------------------------------------------
demo_data_discrete <- tibble(custID = demo_data$custID)

#-----------------------------------------------------------------
# Create a new dataframe to work with
#-----------------------------------------------------------------
demo_data_discrete <- as.data.frame(demo_data_discrete)

#-----------------------------------------------------------------
# Calculate birth year and assign to a generation
#-----------------------------------------------------------------
library(lubridate)
birthYear <- lubridate::year(Sys.Date()) - 
  as.numeric(demo_data$age)

f_seg_generation_def <- function(birth_year){
  
  if(birth_year >= 1910 & birth_year <= 1924){'gen_Greatest'}
  else if (birth_year >= 1925 & birth_year <= 1945){'gen_Silent'}
  else if (birth_year >= 1946 & birth_year <= 1964){'gen_Boomer'}    
  else if (birth_year >= 1965 & birth_year <= 1979){'gen_X'}     
  else if (birth_year >= 1980 & birth_year <= 1994){'gen_Mill'}       
  else if (birth_year >= 1995 & birth_year <= 2012){'gen_Y'}    
  else if (birth_year >= 2013 & birth_year <= 2020){'gen_Alpha'}      
  else{'Unknown'}
}
demo_data_discrete$generation <- 
  sapply(birthYear,f_seg_generation_def)

#-----------------------------------------------------------------
# Assign a discrete value to distance
#-----------------------------------------------------------------
quantile_list <- quantile(demo_data$distance,
                          probs = c(.1,.2,.3,.4,.5,.6,.7,.8,.9))

f_seg_dist_def <- function(dist,q){
  
  if(dist <= 30.67){'dist_Primary'}
  else if (dist > 30.67 & dist <= 51.23){'dist_Secondary'}
  else if (dist > 41.23 & dist <= 106.07){'dist_Tertiary'}    
  else{'dist_Quaternary'}        
}

demo_data_discrete$marketLoc <- 
  sapply(demo_data$distance,f_seg_dist_def)

#-----------------------------------------------------------------
# Assign a discrete value to household Income
#-----------------------------------------------------------------
hhIncomeQuantiles <- quantile(demo_data$hhIncome,
                              probs = c(.25,.75))

f_seg_hhInc_def <- function(hhInc, quant = hhIncomeQuantiles){
  
  if(hhInc <= hhIncomeQuantiles[[1]]){"income_low"}
  else if(hhInc >= hhIncomeQuantiles[[2]]){"income_high"}
  else{"income_med"}
}

demo_data_discrete$income <-
  sapply(demo_data$hhIncome,f_seg_hhInc_def)

#-----------------------------------------------------------------
# Add columns to demo_data
#-----------------------------------------------------------------
demo_data_discrete <- demo_data_discrete    %>% 
  mutate(ethnicity = demo_data$ethnicity,
         married   = demo_data$maritalStatus,
         gender    = demo_data$gender)
demo_data_discrete <- as.data.frame(demo_data_discrete)

#-----------------------------------------------------------------
# Dummy code (One Hot encoding) for model consumption
#-----------------------------------------------------------------
dummy_coded_vars <- 
  apply(demo_data_discrete[,2:7],2, 
        function(x) psych::dummy.code(factor(x)))

mod_data_discrete <- 
  cbind.data.frame(dummy_coded_vars[1:length(dummy_coded_vars)])

row.names(mod_data_discrete) <- demo_data_discrete$custID

#-----------------------------------------------------------------
# Combine data frames
#-----------------------------------------------------------------
mod_data_numeric <- demo_data[,c('age','distance','hhIncome')]
mod_data_numeric <- cbind.data.frame(mod_data_numeric,
                                     mod_data_discrete[,13:20]) 

#-----------------------------------------------------------------
# Prepared data for hierarchical clustering
#-----------------------------------------------------------------
library(dplyr)
library(cluster)
set.seed(755)
mod_data_samp     <- mod_data_numeric %>% dplyr::sample_n(25)
mod_data_samp$age <- as.numeric(mod_data_samp$age)
# Create dissimilarity matrix
mod_dist <- cluster::daisy(mod_data_samp)

#-----------------------------------------------------------------
# Apply heirarchical clustering method
#-----------------------------------------------------------------
mod_HC <- stats::hclust(mod_dist, method = "centroid")

#-----------------------------------------------------------------
# Apply hierarchical clustering method
#-----------------------------------------------------------------
plot(mod_HC)

#-----------------------------------------------------------------
# Latent Class regression
#-----------------------------------------------------------------
mod_data_LC   <- mod_data_discrete
mod_data_LC[] <- apply(mod_data_discrete, 2, 
                       function(x) ifelse(x == 1 ,1 ,2))

#-----------------------------------------------------------------
# Latent Class regression formula
#-----------------------------------------------------------------
format  <- paste(names(mod_data_LC), collapse = ",")
formula <- 
  with(mod_data_LC,
       cbind(generation.gen_X,generation.gen_Mill,
             generation.gen_Boomer,generation.gen_Y,
             generation.gen_Silent,marketLoc.dist_Quaternary,
             marketLoc.dist_Tertiary,marketLoc.dist_Secondary,
             marketLoc.dist_Primary,income.income_med,
             income.income_low,income.income_high,
             ethnicity.w,ethnicity.aa,ethnicity.h,
             ethnicity.a,married.m,married.s,gender.f,
             gender.m)~1)

#-----------------------------------------------------------------
# Latent Class regression
#-----------------------------------------------------------------
require(poLCA)
set.seed(363)
seg_LCR_5 <- poLCA::poLCA(formula, data = mod_data_LC, 
                          nclass = 5, verbose = F)

#save(seg_LCR_5, file="CH5_LCR_5_Model.Rdata")

#-----------------------------------------------------------------
# Evaluating the results
#-----------------------------------------------------------------
table(seg_LCR_5$predclass)

mod_results          <- 
  as.data.frame(matrix(nrow = nrow(mod_data_LC), ncol = 0))
mod_results$custID   <- row.names(mod_data_LC)
mod_results$lcrClass <- seg_LCR_5$predclass

#-----------------------------------------------------------------
# Attaching to the data frame
#-----------------------------------------------------------------
demo_data_segments <- 
  dplyr::left_join(demo_data,mod_results, by = "custID")

#-----------------------------------------------------------------
# Attaching to the data frame
#-----------------------------------------------------------------
demo_data_segments <- as.data.frame(demo_data_segments)

#-----------------------------------------------------------------
# Distance cross tab
#-----------------------------------------------------------------
library(dplyr)
demo_seg_distance <- demo_data_segments %>%
  group_by(lcrClass)                    %>% 
  summarise(avgDist = mean(distance),
            medDist = median(distance))

#-----------------------------------------------------------------
# Age cross tab
#-----------------------------------------------------------------
demo_data_segments$age <- as.numeric(demo_data_segments$age)

demo_seg_age <- demo_data_segments %>%
  group_by(lcrClass)               %>% 
  summarise(avgAge = mean(age),
            medAge = median(age))

#-----------------------------------------------------------------
# Household income cross tab
#-----------------------------------------------------------------
demo_seg_HHI <- demo_data_segments     %>%
  group_by(lcrClass)                   %>% 
  summarise(avgHHI = mean(hhIncome),
            medHHI = median(hhIncome))

#-----------------------------------------------------------------
# Gender cross tab
#-----------------------------------------------------------------
demo_seg_gender <- 
  demo_data_segments         %>%
  group_by(lcrClass)         %>% 
  count(gender)              %>%
  tidyr::pivot_wider(names_from = gender, 
                     values_from = n)

#-----------------------------------------------------------------
# Ethnicity tab
#-----------------------------------------------------------------
demo_seg_ethnicity <- 
  demo_data_segments    %>%
  group_by(lcrClass)    %>% 
  count(ethnicity)      %>%
  tidyr::pivot_wider(names_from  = ethnicity, 
                     values_from = n)

#-----------------------------------------------------------------
#  Visualize ethnicity
#-----------------------------------------------------------------
library(ggplot2)
demo_seg_ethnicity_l <- 
  demo_data_segments %>%
  group_by(lcrClass) %>% 
  count(ethnicity)      

demo_seg_ethnicity_l <- as.data.frame(demo_seg_ethnicity_l)

x_label  <- ('\n Ethnicity')
y_label  <- ('Class \n')
title   <- ('Ethnicity by class')
tile_segment <- ggplot2::ggplot(data   = demo_seg_ethnicity_l, 
                                aes(x    = ethnicity,
                                    y    = lcrClass,
                                    fill = log(n)))             +
  geom_tile()                                                   +
  scale_fill_gradient(low="white", high="dodgerblue",
                      name = 'log(Fans)',label = scales::comma) +
  xlab(x_label)                                                 + 
  ylab(y_label)                                                 + 
  ggtitle(title)                                                +
  graphics_theme_1

#-----------------------------------------------------------------
# access the survey data
#-----------------------------------------------------------------
survey <- FOSBAAS::fa_survey_data

#-----------------------------------------------------------------
# scale the numeric data
#-----------------------------------------------------------------
survey_sc <- scale(survey[,2:26])
survey_sc <- as.data.frame(survey_sc)

#-----------------------------------------------------------------
# access the survey data
#-----------------------------------------------------------------
nFacts <- nFactors::nScree(survey_sc)

#-----------------------------------------------------------------
# access the survey data
#-----------------------------------------------------------------
nFacts <- as.data.frame(unlist(nFacts$Components))
names(nFacts) <- 'Factors'
knitr::kable(nFacts,caption = "Number of Factors")

#-----------------------------------------------------------------
# run a factor analysis
#-----------------------------------------------------------------
library(GPArotation)
survey_sc_ob <- factanal(survey_sc,
                         factors  = 5,
                         rotation = "oblimin",
                         scores   = "Bartlett")

#-----------------------------------------------------------------
# Restructure the data
#-----------------------------------------------------------------
survey_sc_ob_scores <- as.data.frame(unlist(survey_sc_ob$scores))
survey_sc_ob_scores$Reason <- survey$ReasonForAttending

fa_data <- unclass(survey_sc_ob$loadings)
fa_data  <- as.data.frame(fa_data)
fa_data$Selection <- row.names(fa_data)

library(tidyr)
fa_data_p <-  
  fa_data %>% tidyr::pivot_longer(!Selection, 
                                  names_to  = "Factor", 
                                  values_to = "Loading")

fa_data_p$Order <- 
  reorder(fa_data_p$Selection,fa_data_p$Loading,sum)

#-----------------------------------------------------------------
# Visualize the factors
#-----------------------------------------------------------------
require(ggplot2)
factor_table <- 
  ggplot(fa_data_p, aes(Factor, Order))                     +
  geom_tile(aes(fill = Loading))                            + 
  geom_text(aes(label = round(Loading, 1)),color='grey40')  +
  scale_fill_gradient(low      = "white", 
                      high     = "dodgerblue", 
                      space    = "Lab",
                      na.value = "grey50", 
                      guide    = "colourbar")               +
  xlab('\n Factor')                                         + 
  ylab('Activity\n')                                        + 
  ggtitle('What do fans do at games? \n')                   +
  graphics_theme_1                                          +
  theme(legend.position="none")                             +
  theme(axis.text.y = element_text(angle = 0, 
                                   size = 8, vjust = 0, 
                                   color = "grey10"))

#-----------------------------------------------------------------
# predict classes
#-----------------------------------------------------------------
library(nnet)
mod_glm <-   nnet::multinom(Reason ~ .,
                            data = survey_sc_ob_scores,
                            linout = FALSE)

pred_survey_sc_ob_scores <- predict(mod_glm , 
                                    newdata=survey_sc_ob_scores,
                                    type='class')

tile_data <- 
  as.data.frame(table(survey_sc_ob_scores$Reason,
                      pred_survey_sc_ob_scores))

#-----------------------------------------------------------------
# Visualize the factors
#-----------------------------------------------------------------
require(ggplot2)

x_label  <- ('\n Actual response')
y_label  <- ('Attend predict \n')
title   <- ('Prediction of attendance reason')

tile_class <- ggplot2::ggplot(data = hex_data, 
                              aes(x    = Var1,
                                  y    = pred_survey_sc_ob_scores,
                                  fill = Freq))        +
  geom_tile()                               +
  scale_fill_gradient(low = "dodgerblue", high = "coral",
                      name = 'Count',label = scales::comma)   +
  xlab(x_label)                                               + 
  ylab(y_label)                                               + 
  ggtitle(title)                                              +
  graphics_theme_1                                            +
  theme(axis.text.x = element_text(angle = 90, 
                                   size = 8, vjust = 0, 
                                   color = "grey10"),
        axis.text.y = element_text(angle = 0, 
                                   size = 8, vjust = 0, 
                                   color = "grey10"))

#-----------------------------------------------------------------
# Apply classes to individuals
#-----------------------------------------------------------------
ids <- sample(demo_data_segments$custID,10000)
survey_sc_ob_scores$Class <- 
  colnames(survey_sc_ob_scores)[max.col(survey_sc_ob_scores[,1:5],
                                        ties.method="first")]
survey_sc_ob_scores$custID <- ids
combined_classes <- dplyr::left_join(demo_data_segments,
                                     survey_sc_ob_scores,
                                     by="custID")         %>%
                                     na.omit()
combined_classes <- as.data.frame(combined_classes)

#-----------------------------------------------------------------
# recode the factors and classes
#-----------------------------------------------------------------
combined_classes$f_seg_name <- 
  sapply(combined_classes$Class, 
         function(x) switch(x,"Factor1" = "Avid fan",
                              "Factor2" = "Socializers",
                              "Factor3" = "Foodies",
                              "Factor4" = "Parkies",
                              "Factor5" = "Strangers"))

combined_classes$d_seg_name <- 
  sapply(combined_classes$lcrClass, 
         function(x) switch(x,'1' = "Young and Broke",
                              '2' = "Marty Male",
                              '3' = "Fionna Female",
                              '4' = "Diffuse Diane",
                              '5' = "Nearby Ned"))

#-----------------------------------------------------------------
# Visualize the results
#-----------------------------------------------------------------
cc <- combined_classes                %>%
      group_by(f_seg_name,d_seg_name) %>% 
      count()      

x_label  <- ('\n Factor Segments')
y_label  <- ('Demographic Segment \n')
title   <- ('Segment Comparrison')
tile_segment <- 
  ggplot2::ggplot(data = cc, 
                  aes(x = f_seg_name,
                      y = d_seg_name,
                      fill = n))                              +
  geom_tile()                                                 +
  scale_fill_gradient(low="white", high="dodgerblue",
                      name = 'Count',label = scales::comma)   +
  xlab(x_label)                                               + 
  ylab(y_label)                                               + 
  ggtitle(title)                                              +
  graphics_theme_1                                            +
  theme(axis.text.x = element_text(angle = 0, 
                                   size = 8, vjust = 0, 
                                   color = "grey10"),
        axis.text.y = element_text(angle = 0, 
                                   size = 8, vjust = 0, 
                                   color = "grey10"))

#-----------------------------------------------------------------
# Scale the results and average
#-----------------------------------------------------------------
combined_classes$age_s <- 
  scale(as.numeric(combined_classes$age))
combined_classes$distance_s <- 
  scale(as.numeric(combined_classes$distance))

cc <- 
  combined_classes                %>%
  group_by(f_seg_name,d_seg_name) %>% 
  summarise(age = mean(age_s), 
            distance = mean(distance_s)) 

#-----------------------------------------------------------------
# Observe differences between the segments
#-----------------------------------------------------------------
x_label  <- ('\n Scaled Avg Age')
y_label  <- ('Scaled Avg Distance \n')
title    <- ('Segments by distance and age')

point_segment <- 
  ggplot2::ggplot(data      = cc, 
                  aes(x     = age,
                      y     = distance,
                      color = f_seg_name,
                      shape = d_seg_name))                +
  geom_point(size = 3)                                    +
  scale_color_manual(values = palette)                    +
  geom_hline(yintercept = median(cc$distance), lty = 2)   +
  geom_vline(xintercept = median(cc$age), lty = 2)        +
  xlab(x_label)                                           + 
  ylab(y_label)                                           + 
  ggtitle(title)                                          +
  graphics_theme_1

#-----------------------------------------------------------------
# Chapter 6
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# The Price response function
#-----------------------------------------------------------------
library(tidyverse)
# Build a simple data set
sales <- tibble::tibble(
  sales = c(20,30,35,43,35,8,2,0),
  price = c(25,28,29,30,31,34,35,36)
)

x_label  <- ('\n Price')
y_label  <- ('Ticket Sales \n')
title    <- ('Ticket Sales by Price')
line_sales <- 
  ggplot2::ggplot(data  = sales, 
                  aes(x = price,
                      y = sales))                 +
  geom_point(size = 2.5,color = 'mediumseagreen') +
  scale_x_continuous(label = scales::dollar)      +
  xlab(x_label)                                   + 
  ylab(y_label)                                   + 
  ggtitle(title)                                  +
  graphics_theme_1                                +
  geom_line(color = "mediumseagreen")             +
  geom_smooth(method = 'lm') 

#-----------------------------------------------------------------
# The Price response function
#-----------------------------------------------------------------
x_label  <- ('\n Price')
y_label  <- ('Ticket Sales \n')
title    <- ('Ticket Sales by Price')
line_sales_poly <- 
  ggplot2::ggplot(data = sales, 
                  aes(x = price,
                      y = sales))                 +
  geom_point(size = 2.5,color = 'mediumseagreen') +
  scale_x_continuous(label = scales::dollar)      +
  xlab(x_label)                                   + 
  ylab(y_label)                                   + 
  ggtitle(title)                                  +
  graphics_theme_1                                +
  geom_line(color = "mediumseagreen")             +          
  stat_smooth(method = "lm",color = 'coral', 
              formula = y ~ x + poly(x, 2)-1) 

#-----------------------------------------------------------------
# Function to return sales based on price
#-----------------------------------------------------------------
fit <- lm(sales$sales~poly(sales$price,2,raw=TRUE))

f_get_sales <- function(new_price){
  sales <- coef(fit)[1] + 
    (coef(fit)[2]*new_price + (coef(fit)[3] * new_price^2))
  return(sales)
}

#-----------------------------------------------------------------
# Use f_get_sales to get modeled demand at each price level
#-----------------------------------------------------------------
old_prices      <- c(25,28,29,30,31,34,35,36)
estimated_sales <- sapply(old_prices, function(x) f_get_sales(x))

#-----------------------------------------------------------------
# Use f_get_sales to get modeled demand at each price level
#-----------------------------------------------------------------
estimated_sales_new <- f_get_sales(26)

#-----------------------------------------------------------------
# Demonstrate the highest point on the curve
#-----------------------------------------------------------------
x_label  <- ('\n Price')
y_label  <- ('Ticket Sales \n')
title   <- ('Ticket Sales by Price')
line_sales_polyb <- 
  ggplot2::ggplot(data  = sales, 
                  aes(x = price,
                      y = sales))                     +
  geom_point(size = 2.5,color = 'mediumseagreen')     +
  scale_x_continuous(label = scales::dollar)          +
  xlab(x_label)                                       + 
  ylab(y_label)                                       + 
  ggtitle(title)                                      +
  graphics_theme_1                                    +
  geom_line(color = "mediumseagreen")                 +                                         
  stat_smooth(method = "lm", color = 'coral',
              formula = y ~ x + poly(x, 2)-1, se = F) +
  geom_hline(yintercept = 35.8151054, lty = 2)        +
  geom_vline(xintercept = 29.21, lty = 2)

#-----------------------------------------------------------------
# Look for optimum price levels
#-----------------------------------------------------------------
estimated_prices <- seq(from = 25, to = 35, by = 1)
estimated_sales <- sapply(estimated_prices,
                          function(x) f_get_sales(x))

estimated_revenue <- tibble::tibble(
  
  sales      = estimated_sales,
  price      = estimated_prices,
  revenue    = estimated_sales * estimated_prices,
  totalSales = rev(cumsum(rev(sales)))
)

#-----------------------------------------------------------------
# Look for optimum price levels
#-----------------------------------------------------------------
x <- 1
revenue <- list()
while(x <= nrow(estimated_revenue)){
  
  revenue[x] <- estimated_revenue[x,2] * estimated_revenue[x,4]
  x <- x + 1
}
estimated_revenue$totalRevenue <- unlist(revenue)

#-----------------------------------------------------------------
# Generate past seasons data
#-----------------------------------------------------------------
season_2022 <- 
  FOSBAAS::f_build_season(seed1 = 3000, season_year = 2022,
    seed2 = 714, num_games = 81, seed3 = 366, num_bbh = 5,
    num_con = 3, num_oth = 5, seed4 = 309, seed5  = 25,
    mean_sales = 29000, sd_sales = 3500
  )

season_2023 <- 
  FOSBAAS::f_build_season(seed1 = 755, season_year = 2023,
    seed2 = 4256, num_games = 81, seed3 = 54, num_bbh = 6,
    num_con = 4, num_oth = 7, seed4 = 309, seed5  = 25,
    mean_sales = 30500, sd_sales = 3000
  )

season_2024 <- 
  FOSBAAS::f_build_season(seed1 = 2892, season_year = 2024,
    seed2 = 714, num_games = 81, seed3 = 366, num_bbh = 9,
    num_con = 2, num_oth = 6, seed4 = 6856, seed5  = 2892,
    mean_sales = 32300, sd_sales = 2900
  )

past_season <- rbind(season_2022,season_2023,season_2024)

#-----------------------------------------------------------------
# Build test and training set
#-----------------------------------------------------------------
samp <- round(0.05 * nrow(past_season),0)

set.seed(715)
rows  <- sample(seq_len(nrow(past_season)), 
                size = samp)
train <- past_season[-rows, ]
test  <- past_season[rows, ]

#-----------------------------------------------------------------
# Linear model for ticket sales
#-----------------------------------------------------------------
ln_mod_bu <- lm(ticketSales ~ promotion + daysSinceLastGame + 
                  schoolInOut + weekEnd + team , data = train)
ln_mod_bu_sum <- 
  tibble::tibble(
    st_error     = unlist(summary(ln_mod_bu)[6]),
    r_square     = unlist(summary(ln_mod_bu)[8]),
    adj_r_square = unlist(summary(ln_mod_bu)[9]),
    f_stat       = unlist(summary(ln_mod_bu)$fstatistic[1]))

#-----------------------------------------------------------------
# Regression summary
#-----------------------------------------------------------------
summary(ln_mod_bu)

#-----------------------------------------------------------------
# Outliers test
#-----------------------------------------------------------------
library(car)
outliers <- car::outlierTest(ln_mod_bu)

#-----------------------------------------------------------------
# QQPlot
#-----------------------------------------------------------------
qq <- car::qqPlot(ln_mod_bu, main="QQ Plot")

#-----------------------------------------------------------------
# Influence plot
#-----------------------------------------------------------------
ip <- 
  car::influencePlot(ln_mod_bu, id.method="identify", 
                     main="Influence plot for our linear model ")

#-----------------------------------------------------------------
#  Remove outliers and view summary
#-----------------------------------------------------------------
past_season_clean <- past_season[-c(37,48,53,129,142,143),]

ln_mod_clean <- lm(ticketSales ~ promotion + daysSinceLastGame + 
                     schoolInOut + weekEnd + team , 
                   data = past_season_clean)
#-----------------------------------------------------------------
# Save our model
# save(ln_mod_clean, file="ch6_ln_mod_clean.rda")
#-----------------------------------------------------------------
ln_mod_clean_sum <- 
  tibble::tibble(
    st_error     = unlist(summary(ln_mod_clean)[6]),
    r_square     = unlist(summary(ln_mod_clean)[8]),
    adj_r_square = unlist(summary(ln_mod_clean)[9]),
    f_stat       = unlist(summary(ln_mod_clean)$fstatistic[1]))

#-----------------------------------------------------------------
# Transforming the data
#-----------------------------------------------------------------
summary(pw_mod <- car::powerTransform(ln_mod_clean))

#-----------------------------------------------------------------
# Directly transforming the data
#-----------------------------------------------------------------
ln_mod_log <- 
  lm(log(ticketSales) ~ promotion + daysSinceLastGame + 
       schoolInOut + weekEnd + team , 
     data = past_season_clean)
ln_mod_log_sum <- 
  tibble::tibble(
    st_error     = unlist(summary(ln_mod_log)[6]),
    r_square     = unlist(summary(ln_mod_log)[8]),
    adj_r_square = unlist(summary(ln_mod_log)[9]),
    f_stat       = unlist(summary(ln_mod_log)$fstatistic[1])
  )

#-----------------------------------------------------------------
# Apply predictions to our test data set. 
#-----------------------------------------------------------------
test$pred_tickets <- predict(ln_mod_clean,
                             newdata = test)
test$percDiff <- 
  (test$ticketSales - test$pred_tickets)/test$ticketSales

mean_test <- mean(test$percDiff)

#-----------------------------------------------------------------
# Apply predictions to our test data set. 
#-----------------------------------------------------------------
test_line <- test
test_line$order <- seq(1:nrow(test))
test_line <-   tidyr::pivot_longer(test_line,
                                   cols = c('ticketSales','pred_tickets'),
                                   values_transform = list(val = as.character))
x_label  <- ('\n Selected Game')
y_label  <- ('Ticket Sales \n')
title   <- ('Ticket forecasts vs. Actuals')
line_est <- 
  ggplot2::ggplot(data      = test_line, 
                  aes(x     = order,
                      y     = value,
                      color = name))             +
  geom_point(size = 2.5)                         +
  geom_line()                                    +
  scale_color_manual(values = palette)           +
  scale_y_continuous(label = scales::comma)      +
  xlab(x_label)                                  + 
  ylab(y_label)                                  + 
  ggtitle(title)                                 +
  graphics_theme_1

#-----------------------------------------------------------------
# Secondary market data, manifest, and sales data
#-----------------------------------------------------------------
sm  <- FOSBAAS::secondary_data
man <- FOSBAAS::manifest_data
sea <- FOSBAAS::season_data
sea$gameID <- seq(1:nrow(sea))

#-----------------------------------------------------------------
# Secondary sales by section
#-----------------------------------------------------------------
sm_man <- left_join(sm,man, by = 'seatID')

avg_price_comps <- 
  sm_man %>% dplyr::select(gameID,price,singlePrice,tickets) %>%
  na.omit()                                                  %>%
  dplyr::group_by(gameID)                                    %>%
  dplyr::summarise(meanSec   = mean(price),
                   meanPri   = mean(singlePrice),
                   tickets   = sum(tickets))

#-----------------------------------------------------------------
# Adjust secondary to reflect primary
#-----------------------------------------------------------------
sea_adj  <- left_join(sea,avg_price_comps, by = "gameID") 
adj_coef <- scale(sea_adj$ticketSales)
sea_adj$meanSecAdj <- sea_adj$meanSec + (adj_coef * 10)

#-----------------------------------------------------------------
# Adjust secondary to reflect primary
#-----------------------------------------------------------------
ln_mod_sec <- 
  lm(meanSecAdj ~ promotion + daysSinceLastGame + 
     schoolInOut + weekEnd + team, 
     data = sea_adj)
ln_mod_sec_sum <- 
  tibble::tibble(
    st_error     = unlist(summary(ln_mod_sec)[6]),
    r_square     = unlist(summary(ln_mod_sec)[8]),
    adj_r_square = unlist(summary(ln_mod_sec)[9]),
    f_stat       = unlist(summary(ln_mod_sec)$fstatistic[1])
  )

#-----------------------------------------------------------------
# Create data for a new season
#-----------------------------------------------------------------
season_2025 <- 
  FOSBAAS::f_build_season(seed1 = 755, season_year = 2025,
    seed2 = 714, num_games = 81, seed3 = 366, num_bbh = 5,
    num_con = 3, num_oth = 7, seed4 = 366, seed5  = 1,
    mean_sales = 0, sd_sales = 0
  )

#-----------------------------------------------------------------
# Apply model output to new data set
#-----------------------------------------------------------------
season_2025$predTickets <- predict(ln_mod_clean,
                                   newdata = season_2025)
season_2025$predPrices  <- predict(ln_mod_sec,
                                   newdata = season_2025)

#-----------------------------------------------------------------
# Build event scores
#-----------------------------------------------------------------
season_2025$eventScoreA <- 
  as.vector(scale(season_2025$predTickets) * 100)
season_2025$eventScoreB <- 
  as.vector(scale(season_2025$predPrices) * 100)
season_2025$eventScore  <- 
  season_2025$eventScoreA + season_2025$eventScoreB

season_2025 <- season_2025[order(-season_2025$eventScore),]

#-----------------------------------------------------------------
# Observe differences in event scores
#-----------------------------------------------------------------
library(ggplot2)
season_2025$order <- seq(1:nrow(season_2025))

x_label  <- ('\n Game')
y_label  <- ('Event Scores \n')
title   <- ('Ordered event scores')
line_est_es <- 
  ggplot2::ggplot(data  = season_2025, 
                  aes(x = order,
                      y = eventScore))             +
  geom_point(size = 1.3,color = 'dodgerblue')      +
  geom_line()                                      +
  scale_color_manual(values = palette)             +
  scale_y_continuous(label = scales::comma)        +
  xlab(x_label)                                    + 
  ylab(y_label)                                    + 
  ggtitle(title)                                   +
  graphics_theme_1

#-----------------------------------------------------------------
# K means clustering on event scores
#-----------------------------------------------------------------
set.seed(715)
clusters <- kmeans(season_2025$eventScore,6)
season_2025$cluster <- clusters$cluster
#write.csv(season_2025,'season_2025.csv',row.names = F)

#-----------------------------------------------------------------
# Summary statistics
#-----------------------------------------------------------------
library(dplyr)
season_summary <- season_2025            %>% 
  group_by(cluster)                      %>%
  summarise(mean   = mean(eventScore),
            median = median(eventScore),
            sd     = sd(eventScore),
            n      = n())                %>%
  arrange(desc(mean))

#-----------------------------------------------------------------
# Create Van Westendorp survey data
#-----------------------------------------------------------------
vw_data <- data.frame(matrix(nrow = 1000, ncol = 6))
names(vw_data) <- c('DugoutSeats', 'PriceExpectation', 
                    'TooExpensive', 'TooCheap', 
                    'WayTooCheap', 'WayTooExpensive')
set.seed(715)
vw_data[,1] <- 'DugoutSeats'
vw_data[,2] <- round(rnorm(1000,100,10),0)
vw_data[,3] <- round(rnorm(1000,130,20),0)
vw_data[,4] <- round(rnorm(1000,60,15),0)
vw_data[,5] <- round(rnorm(1000,50,10),0)
vw_data[,6] <- round(rnorm(1000,160,20),0)

#-----------------------------------------------------------------
# Empirical cumulative distribution function
#-----------------------------------------------------------------
library(Hmisc)
dat <- data.frame(
  "toocheap"     = vw_data$WayTooCheap,
  "notbargain"   = vw_data$TooExpensive,
  "notexpensive" = vw_data$TooCheap,
  "tooexpensive" = vw_data$WayTooExpensive
)
a <- Ecdf(dat$toocheap,what="1-F",pl=F)$y[-1]
b <- Ecdf(dat$notbargain, pl=F)$y[-1]
c <- Ecdf(dat$notexpensive,what = "1-F", pl=F)$y[-1]
d <- Ecdf(dat$tooexpensive,pl=F)$y[-1]

#-----------------------------------------------------------------
# Build data set for creating graphic
#-----------------------------------------------------------------
library(reshape2)
ecdf1 <- data.frame(
  "variable"  = rep("toocheap",length(a)),
  "ecdf"      = a,
  "value"    = sort(unique(dat$toocheap)))
ecdf2 <- data.frame(
  "variable"  = rep("notbargain",length(b)),
  "ecdf"      = b,
  "value"     = sort(unique(dat$notbargain),decreasing = T))
ecdf3 <- data.frame(
  "variable"  = rep("notexpensive",length(c)),
  "ecdf"     = c,
  "value"    = sort(unique(dat$notexpensive),decreasing = T))
ecdf4 <- data.frame(
  "variable"  = rep("tooexpensive",length(d)),
  "ecdf"      = d,
  "value"     = sort(unique(dat$tooexpensive)))
dat2 <- rbind(ecdf1,ecdf2,ecdf3,ecdf4)
dat  <- melt(dat)
dat  <- merge(dat,dat2,by=c("variable","value"))

#-----------------------------------------------------------------
# Graph the results
#-----------------------------------------------------------------
require(ggplot2)
require(scales)
require(RColorBrewer)

Paired     <- RColorBrewer::brewer.pal(4,"Paired")

g_xlab     <- '\n prices'
g_ylab     <- 'Responses  \n'
g_title    <- 'Dugout Price Value Perception\n'

vw_gaphic <- 
  ggplot(dat, aes(value, ecdf, color=variable)) + 
  annotate("rect", xmin = 55, xmax = 146, 
           ymin = 0,  ymax = 1,
           alpha = .4, fill = 'coral')          +
  geom_line(size = 1.2) +
  scale_color_manual(values = Paired,
                     name = 'Value Perception') + 
  xlab(g_xlab)                                  + 
  ylab(g_ylab)                                  + 
  ggtitle(g_title)                              + 
  scale_y_continuous(labels = percent)          +
  scale_x_continuous(labels = dollar)           +
  coord_cartesian(xlim = c(0,220),ylim= c(0,1)) +
  geom_hline(yintercept = .5,
             lty=4,
             alpha = .5)                        +
  graphics_theme_1

#-----------------------------------------------------------------
# Duplicate work with a library
#-----------------------------------------------------------------
library(pricesensitivitymeter)
price_sensitivity <- psm_analysis(
  toocheap        = "WayTooCheap",
  cheap           = "TooCheap",
  expensive       = "TooExpensive",
  tooexpensive    = "WayTooExpensive",
  data            = vw_data,
  validate        = TRUE)

#-----------------------------------------------------------------
# Explore price sensitivity data
#-----------------------------------------------------------------
ps <- 
  tibble::tibble(lower_price = price_sensitivity$pricerange_lower,
                 upper_price = price_sensitivity$pricerange_upper)

#-----------------------------------------------------------------
# Chapter 7
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# RFM data
#-----------------------------------------------------------------
library(dplyr)
demo_data <- FOSBAAS::demographic_data[,c(1,4)]
set.seed(44)
demo_data <- demo_data %>%
mutate(
 lastInteraction = abs(round(rnorm(nrow(demo_data),50,30),0)),
 interactionsYTD = abs(round(rnorm(nrow(demo_data),10,5),0)),
 lifetimeSpend   = abs(round(rnorm(nrow(demo_data),10000,7000),0))
)

#-----------------------------------------------------------------
# Function to calculate FRM scores
#-----------------------------------------------------------------
demo_data$Recency       <- -scale(demo_data$lastInteraction)
demo_data$Frequency     <- scale(demo_data$interactionsYTD)
demo_data$MonetaryValue <- scale(demo_data$lifetimeSpend)
# Produce quantiles for each scaled value
r_quant <- unname(quantile(demo_data$Recency,
                           probs = c(.2,.4,.6,.8)))
f_quant <- unname(quantile(demo_data$Recency,
                           probs = c(.2,.4,.6,.8)))
m_quant <- unname(quantile(demo_data$Recency,
                           probs = c(.2,.4,.6,.8)))
# Function to evaluate RFM score
f_create_rfm <- function(quantList,number){
  
if(number <= quantList[[1]]){'1'}
  else if(number <= quantList[[2]]){'2'}
    else if(number <= quantList[[3]]){'3'}
      else if(number <= quantList[[4]]){'4'}
        else{'5'}
}

#-----------------------------------------------------------------
# Create final RFM values
#-----------------------------------------------------------------
value <- list()
j     <- 1
for(i in demo_data$Recency){
  value[j] <- f_create_rfm(r_quant,i)
  j <- j + 1
}
demo_data$r_val <- unlist(value)
#-----------------------------------------------------------------
value <- list()
j     <- 1
for(i in demo_data$Frequency){
  value[j] <- f_create_rfm(f_quant,i)
  j <- j + 1
}
demo_data$f_val <- unlist(value)
#-----------------------------------------------------------------
value <- list()
j     <- 1
for(i in demo_data$MonetaryValue){
  value[j] <- f_create_rfm(m_quant,i)
  j <- j + 1
}
demo_data$m_val <- unlist(value)
#-----------------------------------------------------------------

demo_data$RFM <- paste(demo_data$r_val,
                       demo_data$f_val,
                       demo_data$m_val, sep = '')

#-----------------------------------------------------------------
# Create final RFM values
#-----------------------------------------------------------------
top_prospects <- subset(demo_data,demo_data$RFM == '555')

#-----------------------------------------------------------------
# access renewal data
#-----------------------------------------------------------------
library(FOSBAAS)
mod_data <- FOSBAAS::customer_renewals

#-----------------------------------------------------------------
# Dummy code and alter data frame for all numeric input
#-----------------------------------------------------------------
d1 <- as.data.frame(psych::dummy.code(mod_data$corporate))
d2 <- as.data.frame(psych::dummy.code(mod_data$planType))

mod_data_numeric <- dplyr::select(mod_data,ticketUsage,
                                  tenure,spend,tickets,
                                  distance,renewed) %>%
                    dplyr::bind_cols(d1,d2)

#-----------------------------------------------------------------
# Prepare the data for analysis
#-----------------------------------------------------------------
mod_data$renewed   <- factor(mod_data$renewed)
mod_data$accountID <- NULL
mod_data$season    <- NULL

#-----------------------------------------------------------------
# Using R to visualize geographic data
#-----------------------------------------------------------------
library(ggmap)
library(ggplot2)

demos <- FOSBAAS::demographic_data
demos <- demos[sample(nrow(demos), 5000), ]

map_data <- subset(demos,demos$longitude >= -125 & 
                     demos$longitude <= -67 &
                     demos$latitude >= 25.75 & 
                     demos$latitude <= 49)
us <- c(left = -91, bottom = 32, right = -80, top = 38)
map <- get_stamenmap(us, zoom = 6, maptype = "toner-lite") %>% 
  ggmap() 

geographic_vis <- 
  map +
  geom_point(data = map_data, 
             mapping = aes(x = longitude, y = latitude),
             size = .2,alpha = .5, color= 'dodgerblue')

#-----------------------------------------------------------------
# Download and install the mlr3 libraries
#-----------------------------------------------------------------
library("mlr3")         #  install.packages("mlr3viz")
library("mlr3learners") #  install.packages("mlr3learners")
library("mlr3viz")      #  install.packages("mlr3viz")
library("mlr3tuning")   #  install.packages("mlr3tuning")
library("paradox")      #  install.packages("paradox")

#-----------------------------------------------------------------
# Recode response to a factor
#-----------------------------------------------------------------
mod_data_numeric$renewed <- factor(mod_data_numeric$renewed)

#-----------------------------------------------------------------
# Build task
#-----------------------------------------------------------------
task_mod_data <- TaskClassif$new(id       = "task_renew", 
                                 backend  = mod_data_numeric, 
                                 target   = "renewed", 
                                 positive = "r")
# Add task to the task dictionary
mlr_tasks$add("task_renew", task_mod_data)

#-----------------------------------------------------------------
# Define learner
#-----------------------------------------------------------------
learner_ranger_rf <- lrn("classif.ranger",
                         predict_type = "prob",
                         mtry         = 3,
                         num.trees    = 500)
# Check parameters with this: learner_ranger_rf$param_set$ids()
# look at a list of learners: mlr3::mlr_learners

#-----------------------------------------------------------------
# Build test and training data set
#-----------------------------------------------------------------
set.seed(44)
train_mod_data <- sample(task_mod_data$nrow, 
                         0.75 * task_mod_data$nrow)
test_mod_data  <- setdiff(seq_len(task_mod_data$nrow), 
                          train_mod_data)

#-----------------------------------------------------------------
# Train the model
#-----------------------------------------------------------------
learner_ranger_rf$train(task    = task_mod_data, 
                        row_ids = train_mod_data)

#-----------------------------------------------------------------
# Inspect the results
#-----------------------------------------------------------------
# print(learner_ranger_rf$model)
learner_output <- 
  tibble::tibble(
    numTrees  = unname(learner_ranger_rf$model$num.trees),
    trys      = unname(learner_ranger_rf$model$mtry),
    samples   = unname(learner_ranger_rf$model$num.samples),
    error     = unname(learner_ranger_rf$model$prediction.error)
  )

#-----------------------------------------------------------------
# Evaluate holdout sample
#-----------------------------------------------------------------
prediction <- learner_ranger_rf$predict(task_mod_data, 
                                        row_ids = test_mod_data)

#-----------------------------------------------------------------
# Confusion matrix
#-----------------------------------------------------------------
prediction$confusion

#-----------------------------------------------------------------
# Evaluate holdout sample
#-----------------------------------------------------------------
measure = msr("classif.acc")
prediction$score(measure)

#-----------------------------------------------------------------
# Access model predictions
#-----------------------------------------------------------------
probs <- as.data.frame(learner_ranger_rf$model$predictions)
head(probs)

#-----------------------------------------------------------------
# Evaluate holdout sample
#-----------------------------------------------------------------

tasks        <- tsks(c("task_renew"))
learner      <- lrns(c("classif.featureless","classif.rpart"),
                     predict_type = "prob")
resampling   <- rsmps("cv")
object       <- benchmark(benchmark_grid(tasks, 
                                         learner, 
                                         resampling))
# Use head(fortify(object)) to see the ce for the resamples

#-----------------------------------------------------------------
# Boxplot of classification error
#-----------------------------------------------------------------
bplot <- 
  autoplot(object)   +
  graphics_theme_1 +
  geom_boxplot(fill = 'dodgerblue') 

#-----------------------------------------------------------------
# ROC curve for random forest model
#-----------------------------------------------------------------
roc_model <- 
  autoplot(object$filter(task_ids = "task_renew"), 
           type = "roc") +
  graphics_theme_1 +
  scale_color_manual(values = palette)

#-----------------------------------------------------------------
# Rebuild model
#-----------------------------------------------------------------
task_mod_data <- TaskClassif$new(id       = "task_renew", 
                                 backend  = mod_data_numeric, 
                                 target   = "renewed", 
                                 positive = "r")

learner_ranger_rf$train(task_mod_data, row_ids = train_mod_data)

#-----------------------------------------------------------------
# Add a resampliing parameter
#-----------------------------------------------------------------
resampling_mod_data  <- rsmp("cv")

#-----------------------------------------------------------------
# Rebuild model
#-----------------------------------------------------------------
resampling_mod_data$instantiate(task_mod_data)
resampling_mod_data$iters

#-----------------------------------------------------------------
# We can now call the resample object
#-----------------------------------------------------------------
resamp <- resample(task_mod_data, 
                   learner_ranger_rf, 
                   resampling_mod_data, 
                   store_models = TRUE)

#-----------------------------------------------------------------
# Rebuild model
#-----------------------------------------------------------------
resamp$aggregate(msr("classif.ce"))
# Look at scores from the models: resamp$score(msr("classif.ce"))

#-----------------------------------------------------------------
# Parameter set
#-----------------------------------------------------------------
param_set <- as.data.frame(learner_ranger_rf$param_set$ids())
head(param_set)

#-----------------------------------------------------------------
# Tuning parameters
#-----------------------------------------------------------------
tune_rf_params <- ParamSet$new(list(
  ParamInt$new("min.node.size", lower = 10, upper = 200),
  ParamInt$new("max.depth",     lower = 2,  upper = 20),
  ParamInt$new("num.trees",     lower = 500,  upper = 600)
))

#-----------------------------------------------------------------
# set resampling and eval parameters
#-----------------------------------------------------------------
resamp_strat     <- rsmp("holdout")
measure_mod_data <- msr("classif.ce")
evals_10         <- trm("evals", n_evals = 10)

#-----------------------------------------------------------------
# Build a tuning instance
#-----------------------------------------------------------------
tune_instance <- TuningInstanceSingleCrit$new(
  task         = task_mod_data,
  learner      = learner_ranger_rf,
  resampling   = resamp_strat,
  measure      = measure_mod_data,
  search_space = tune_rf_params,
  terminator   = evals_10
)

#-----------------------------------------------------------------
# Randomly select options within tuning min and max
#-----------------------------------------------------------------
tuner_rf = tnr("random_search")

#-----------------------------------------------------------------
# Run the models
#-----------------------------------------------------------------
tuner <- tuner_rf$optimize(tune_instance)

#-----------------------------------------------------------------
# Get the best parameters
#-----------------------------------------------------------------
best_params <- tune_instance$result_learner_param_vals

#-----------------------------------------------------------------
# Observe new classification error
#-----------------------------------------------------------------
tune_instance$result_y

#-----------------------------------------------------------------
# Rerun model
#-----------------------------------------------------------------
learner_ranger_rf$param_set$values = 
  tune_instance$result_learner_param_vals
learner_ranger_rf$train(task_mod_data)

#-----------------------------------------------------------------
# Build a tuning instance
#-----------------------------------------------------------------
prediction_tuned <- 
  learner_ranger_rf$predict(task_mod_data, 
                            row_ids = test_mod_data)

#-----------------------------------------------------------------
# Observe confusion matrix
#-----------------------------------------------------------------
prediction_tuned$confusion

#-----------------------------------------------------------------
# Observe optimized classification
#-----------------------------------------------------------------
measure = msr("classif.acc")
prediction_tuned$score(measure)

#-----------------------------------------------------------------
# Observe all learners
#-----------------------------------------------------------------
mlr3::mlr_learners

#-----------------------------------------------------------------
# Observe all learners
#-----------------------------------------------------------------

task_mod_data_num <- TaskClassif$new(id   = "task_bench", 
                                     backend  = mod_data_numeric, 
                                     target   = "renewed", 
                                     positive = "r")

learner_num <- 
  list(lrn("classif.xgboost", predict_type = "prob"), 
       lrn("classif.ranger", predict_type = "prob"))

set.seed(44)
train_mod_data_num <- 
  sample(task_mod_data_num$nrow, 0.75 * task_mod_data_num$nrow)
test_mod_data_num  <- 
  setdiff(seq_len(task_mod_data_num$nrow), train_mod_data_num)

#-----------------------------------------------------------------
# Build new task and learner
#-----------------------------------------------------------------
design_bnch <- benchmark_grid(
  task_bnch        <- TaskClassif$new(id  = "task_class2", 
                                      backend  = mod_data_numeric, 
                                      target   = "renewed", 
                                      positive = "r"),
  learners_bnch    <- 
    list(lrn("classif.xgboost", predict_type = "prob"), 
         lrn("classif.ranger",  predict_type = "prob"),
         lrn("classif.naive_bayes", predict_type = "prob" )),
  resamplings_bnch <- rsmp("holdout")
)

#-----------------------------------------------------------------
# benchmark our designs
#-----------------------------------------------------------------
bmr = benchmark(design_bnch)

#-----------------------------------------------------------------
# Observe available measures
#-----------------------------------------------------------------
mlr3::mlr_measures

#-----------------------------------------------------------------
# Compare the models
#-----------------------------------------------------------------
measures = list(
  msr("classif.auc", id = "auc"),
  msr("classif.ce", id = "ce_train")
)
measure_list <- as.data.frame(bmr$score(measures))
measure_list[,c(6,11,12)]

#-----------------------------------------------------------------
# Compare the models
#-----------------------------------------------------------------
mod <- glm(renewed ~ ticketUsage + tenure + 
             spend + distance,
           data = mod_data_numeric,
           family = binomial(link = "logit")
)
mod_sum <- 
  tibble::tibble(
    deviance      = unlist(summary(mod)$deviance),
    null.deviance = unlist(summary(mod)$null.deviance),
    aic           = unlist(summary(mod)$aic),
    df.residual   = unlist(summary(mod)$df.residual),
    pseudoR2      = 1 - mod$deviance / mod$null.deviance
  )

#-----------------------------------------------------------------
# Compare the models
#-----------------------------------------------------------------
calc_rates <- learner_ranger_rf$predict_newdata(mod_data_numeric)
mod_data_numeric$pred <- predict(mod,newdata = mod_data_numeric,
                                 type = 'response')

#-----------------------------------------------------------------
# Density plot of error
#-----------------------------------------------------------------
title   <- 'Density plot of renewal'
x_label <- 'Prediction'
y_label <- 'Density'
density_pred <- 
  ggplot(data = mod_data_numeric, 
         aes(x=pred,color=renewed,lty=renewed))  +
  geom_density(size = 1.2)                       + 
  scale_color_manual(values = palette)           +
  scale_x_continuous(label = scales::percent)    +
  xlab(x_label)                                  + 
  ylab(y_label)                                  + 
  ggtitle(title)                                 +
  graphics_theme_1

#-----------------------------------------------------------------
# ROC curve
#-----------------------------------------------------------------
library(pROC)
#define object to plot
roc_object <- roc(mod_data_numeric$renewed, mod_data_numeric$pred)

title   <- 'ROC curve for renewals'
x_label <- 'Specificity'
y_label <- 'Sensitivity'

roc_graph <-
  ggroc(roc_object,colour = 'dodgerblue',size = 1.2) +
  xlab(x_label)                                    + 
  ylab(y_label)                                    + 
  ggtitle(title)                                   +
  graphics_theme_1

#-----------------------------------------------------------------
# Build data fro cumulative gains chart
#-----------------------------------------------------------------
mod_data_sample <- 
  mod_data_numeric                                      %>% 
  dplyr::select(pred,renewed)                           %>%
  dplyr::mutate(custId = seq(1:nrow(mod_data_numeric))) %>%
  dplyr::sample_n(5000)                                 %>%
  dplyr::arrange(desc(pred))

qt <- quantile(mod_data_sample$pred,
               probs = c(.1,.2,.3,.4,.5,.6,.7,.8,.9))

f_apply_quant <- function(x){
  ifelse(x >= qt[9],1,
         ifelse(x >= qt[8],2,
                ifelse(x >= qt[7],3,
                       ifelse(x >= qt[6],4,
                              ifelse(x >= qt[5],5,
                                     ifelse(x >= qt[4],6,
                                            ifelse(x >= qt[3],7,
                                                   ifelse(x >= qt[2],8,
                                                          ifelse(x >= qt[1],9,10)))))))))
}

mod_data_sample$group <- sapply(mod_data_sample$pred,
                                function(x) f_apply_quant(x))

table(mod_data_sample$group,mod_data_sample$renewed)

#-----------------------------------------------------------------
# Build data for cumulative gains chart
#-----------------------------------------------------------------
mod_data_sample$renewedNum <- 
  ifelse(mod_data_sample$renewed == 'r',1,0)
mod_data_sample$perpop <- 
  (seq(nrow(mod_data_sample))/nrow(mod_data_sample))*100

mod_data_sample$percRenew <- 
cumsum(mod_data_sample$renewedNum)/sum(mod_data_sample$renewedNum)

title   <- 'Cumulative gains chart'
x_label <- 'Population'
y_label <- 'Cumulative Renewal Percentage'
cgc <- 
  ggplot(mod_data_sample,aes(y=percRenew,x=perpop))         +
  geom_line(color = mod_data_sample$group,size = 1.2 )      +
  geom_rug(color = mod_data_sample$group,sides = 'b' )      +
  geom_abline(intercept = 0, slope = .01, size = 0.5,lty=3) +
  xlab(x_label)                                             + 
  ylab(y_label)                                             + 
  ggtitle(title)                                            +
  graphics_theme_1

#-----------------------------------------------------------------
# Build data for improved cumulative gains chart 
#-----------------------------------------------------------------
mod_data_gain <- mod_data_sample %>%
  group_by(group) %>%
  summarise(cumRenewed = sum(renewedNum))

new_renewals <- c(500,490,455,400,300,200,120,90,70,20)
mod_data_gain$cumRenewed <- new_renewals

mod_data_gain$gain <- 
  cumsum(mod_data_gain$cumRenewed/sum(mod_data_gain$cumRenewed))


title   <- 'Cumulative gains chart'
x_label <- 'Group'
y_label <- 'Gain'

cgc_imp <- 
  ggplot(mod_data_gain,aes(y=gain,x=group))                 +
  geom_line(color = mod_data_gain$group,size = 1.2)         +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10))    +
  geom_abline(intercept = 0, slope = .01, size = 0.5,lty=3) +
  xlab(x_label)                                             + 
  ylab(y_label)                                             + 
  ggtitle(title)                                            +
  graphics_theme_1

#-----------------------------------------------------------------
# Chapter 8
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Season data structure
#-----------------------------------------------------------------
data <- FOSBAAS::season_data

data_struct <- 
  data.frame(
    variable = names(data),
    class    = sapply(data, typeof),
    values   = sapply(data, function(x) paste0(head(x)[1:2],  
                                               collapse = ", ")),
    row.names = NULL
  )

#-----------------------------------------------------------------
# Promotions by season
#-----------------------------------------------------------------
data$count <- seq(1:nrow(data))

x_label  <- ('\n 2022-2024 Home Games')
y_label  <- ('Tickets Sold \n')
title   <- ("M-Promos tend to have higher than average sales")
ticket_sales <- 
  ggplot(data, 
         aes(x = count,y = ticketSales, 
             color = factor(promotion)),
         group = promotion)                                  +
  ggtitle(title)                                                 +
  xlab(x_label)                                                  +                         
  ylab(y_label)                                                  +                           scale_x_continuous(breaks = 1:243  )                           +
  scale_y_continuous(labels = scales::comma)                     +
  geom_point(aes(y=ticketSales,x=count), size=2)                 +
  geom_smooth(data=subset(
    data,promotion == 'bobblehead' | promotion == 'concert' | 
      promotion == 'other' | promotion == 'none'),
    method='lm',formula=y~x,se=FALSE,fullrange=TRUE,size=1.2)    +
  geom_vline(xintercept = 1, lty=4, color='grey30')              +
  geom_vline(xintercept = 81, lty=4, color='grey30')             +
  geom_vline(xintercept = 162, lty=4, color='grey30')            +
  geom_vline(xintercept = 243, lty=4, color='grey30')            +
  scale_color_manual(
    breaks = c('bobblehead','concert','other','none'),
    values=palette, name='Promotion: ',
    labels=c("BHead","Concert","Other",'None'))                  +    
  annotate("text", x = 40,  y = 1000, 
           label = "2022", color='black')                        +
  annotate("text", x = 120, y = 1000, 
           label = "2023", color='black')                        +
  annotate("text", x = 200, y = 1000, 
           label = "2024", color='black')                        +
  graphics_theme_1                                               + 
  theme(
    axis.text.x      = element_blank(),
    legend.position  = "bottom",
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(), 
    axis.ticks.x=element_blank()
  )

#-----------------------------------------------------------------
# Promotions by day of the week
#-----------------------------------------------------------------
x_label  <- ('\n Promotion')
y_label  <- ('Day of Week\n')
title    <- 
  ('Average Sales (10,000s) by promotion and day of week \n')

promos <- 
  data                                        %>% 
  group_by(dayOfWeek,promotion,season)        %>%
  summarise(avgTickets = median(ticketSales))

tile_sales <- 
  ggplot(promos, aes(y=dayOfWeek,x=promotion))                   +
  facet_grid(.~season)                                           +
  geom_tile(aes(fill = avgTickets))                              + 
  geom_text(aes(label = round((avgTickets/10000), 2)),
            color='grey10')                                      +
  scale_fill_gradient(low = "white", high = "dodgerblue", 
                      space = "Lab",
                      na.value = "grey10", guide = "colourbar")  +
  ggtitle(title)                                                 +
  xlab(x_label)                                                  +                       
  ylab(y_label)                                                  + 
  scale_y_discrete(limits=c('Mon','Tue','Wed','Thu',
                            'Fri','Sat','Sun'))                  + 
  scale_x_discrete(limits = c('bobblehead','concert',
                              'none','other'),
                   labels=c('bh','concert','none','other'))      +
  graphics_theme_1 + theme(
    legend.position  = "bottom",
    axis.text.x      = element_text(angle = 0, size = 10, 
                                    vjust = 0, color = "grey10"),
    legend.title     = element_text(size = 10, face = "plain", 
                                    color = "grey10"), 
    legend.text      = element_text(size = 7, color = "grey10")
  )

#-----------------------------------------------------------------
# Color interpolation
#-----------------------------------------------------------------
library(viridis)
scale_fill_viridis(direction = 1, option = "B",trans="log2") 
scale_fill_distiller(palette = 'Greens',direction = 1)

#-----------------------------------------------------------------
# Promotions by season and day of week
#-----------------------------------------------------------------
data$count <- seq(1:nrow(data))

x_label  <- ('\n 2022-2024 Home Games')
y_label  <- ('Tickets Sold \n ')
title    <- ("Impacts are less clear when DOW is considered")

dow_sales <- 
  ggplot(data, aes(x = count,y = ticketSales, 
                   color = factor(dayOfWeek),
                   shape = factor(promotion),
                   group =factor(dayOfWeek)))                +
  ggtitle(title)                                             +
  xlab(x_label)                                              +                            
  ylab(y_label)                                              +
  scale_x_continuous( breaks = 1:242)                        +
  scale_y_continuous(labels  = scales::comma)                +
  geom_point(aes(y=data$ticketSales,x=data$count), size=3.5) +
  geom_vline(xintercept = 81,  lty = 4,  color ='grey30')    +
  geom_vline(xintercept = 162, lty = 4, color ='grey30')     +
  geom_vline(xintercept = 242, lty = 4, color ='grey30')     +
  scale_color_manual(breaks = c("Mon", "Tue",'Wed','Thu',
                                'Fri','Sat','Sun'),
                     values = palette,
                     name   ='Day: ',
                     labels = c("Mon", "Tue",'Wed','Thu',
                                'Fri','Sat','Sun'))          +   
  scale_shape_manual(
    breaks = c('bobblehead','none','concert','other'),
    name='Promotion: ',
    values = c(17,19,3,7),
    labels=c("bh",'none','con','other'))                     +   
  annotate("text", x = 40,  y = 1000, 
           label = "2022", color='grey10')                   +
  annotate("text", x = 120, y = 1000, 
           label = "2023", color='grey10')                   +
  annotate("text", x = 200, y = 1000, 
           label = "2024", color='grey10')                   +
  graphics_theme_1                                           +   
  theme(
    axis.text.x      = element_blank(),
    legend.position  = "right",
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(), 
    axis.ticks.x     = element_blank())

#-----------------------------------------------------------------
# Box plot, sales by day of week
#-----------------------------------------------------------------
x_label  <- ('\n 2022-2024 games by DOW')
y_label  <- ('Tickets Sold \n ')
title    <- ("Sales by DOW and promotion")

dow_sales_box <- 
  ggplot(data, aes(x     = dayOfWeek, 
                   fill  = factor(promotion), 
                   color = factor(promotion)))               +
  ggtitle(title)                                             +
  xlab(x_label)                                              +                               
  ylab(y_label)                                              +
  scale_y_continuous(labels = scales::comma)                 +
  scale_x_discrete(limits=c("Mon", "Tue",'Wed','Thu',
                            'Fri','Sat','Sun'))              + 
  geom_boxplot(aes(y=ticketSales))                           +
  scale_color_manual(
    breaks = c('bobblehead','concert','none','other'),
    values=c('grey40','grey40','grey40','grey40'), 
    name='Concert: ',
    labels=c('bobblehead','concert','none','other'),
    guide = 'none')                                          +    
  scale_fill_manual(
    breaks = c('bobblehead','concert','none','other'),
    values=c(palette), 
    name='Promotion: ',
    labels=c('bobblehead','concert','none','other'))         +
  graphics_theme_1                                           +   
  theme(legend.position  = "bottom")

#-----------------------------------------------------------------
# preprocessing our data
#-----------------------------------------------------------------
library(tidymodels)
library(readr)      
library(broom.mixed) 
library(dotwhisker)  
library(skimr)
library(dplyr)

data <- FOSBAAS::season_data
data <- data[,c("gameNumber","team","month","weekEnd",
                "daysSinceLastGame","promotion","ticketSales")]

#-----------------------------------------------------------------
# Alter promotions 
#-----------------------------------------------------------------
f_change_order <- function(x){
  if(x == "none"){"anone"}
  else{x}
}
data$promotion <- sapply(data$promotion,function(x) 
  f_change_order(x))

#-----------------------------------------------------------------
# Splitting our data set
#-----------------------------------------------------------------
set.seed(755)
data_split <- initial_split(data, prop = .75)
train_data <- rsample::training(data_split)
test_data  <- rsample::testing(data_split)

#-----------------------------------------------------------------
# Build a recipe
#-----------------------------------------------------------------
sales_rec <-  recipe(ticketSales ~ ., data = train_data)

#-----------------------------------------------------------------
# Add functions to the recipe
#-----------------------------------------------------------------
sales_rec <- recipe(ticketSales ~ ., data = train_data) %>% 
  update_role(gameNumber, new_role = "ID")              %>%
  step_dummy(all_nominal(), -all_outcomes())            %>%
  step_zv(all_predictors())

#-----------------------------------------------------------------
# Check the recipe 
#-----------------------------------------------------------------

data_test <- sales_rec      %>% 
  prep()                    %>% 
  bake(new_data = test_data)

head(data_test)[c(4:9)]

#-----------------------------------------------------------------
# Define a model
#-----------------------------------------------------------------
lm_model <- linear_reg() %>% 
  set_engine('lm')       %>% 
  set_mode('regression')

#-----------------------------------------------------------------
# build a workflow
#-----------------------------------------------------------------
sales_wflow <- workflow() %>% 
  add_model(lm_model)     %>% 
  add_recipe(sales_rec)

#-----------------------------------------------------------------
# build a workflow
#-----------------------------------------------------------------
set.seed(755)
folds        <- vfold_cv(train_data, v = 10)
sales_fit_rs <- sales_wflow          %>% 
  fit_resamples(folds)

cv_metrics <- collect_metrics(sales_fit_rs)

#-----------------------------------------------------------------
# Run the model and extract the results
#-----------------------------------------------------------------

sales_fit <- sales_wflow %>% 
  fit(data = train_data)

results   <- sales_fit   %>% 
  pull_workflow_fit()    %>% 
  tidy()

#-----------------------------------------------------------------
# build a workflow
#-----------------------------------------------------------------
model         <- extract_model(sales_fit)
model_metrics <- glance(model)

#-----------------------------------------------------------------
# Clustered events
#-----------------------------------------------------------------
season_2025 <- read.csv('season_2025.csv')

season_2025$cluster <- factor(season_2025$cluster)
x_label  <- ('\n Game')
y_label  <- ('Event Scores \n')
title   <- ('Event score clusters by event')
es_box <- 
  ggplot2::ggplot(data = season_2025, 
                  aes(x = order,
                      y = eventScore,
                      color = cluster))             +
  geom_point(size = 1,alpha = .5)                   +
  geom_boxplot()                                    +
  scale_color_manual(values = palette)              +
  scale_y_continuous(label = scales::comma)         +
  xlab(x_label)                                     + 
  ylab(y_label)                                     + 
  ggtitle(title)                                    +
  graphics_theme_1

#-----------------------------------------------------------------
# Identify game breaks
#-----------------------------------------------------------------
season_2025$border <- 
  ifelse(duplicated(season_2025$cluster) == TRUE,FALSE,TRUE)

#-----------------------------------------------------------------
# Identify candidate games
#-----------------------------------------------------------------
season_2025$cluster <- factor(season_2025$cluster)
x_label  <- ('\n Pred Sales')
y_label  <- ('Pred Price \n')
title   <- ('Predicted sales and price by cluster')
es_scatter <- 
  ggplot2::ggplot(data      = season_2025, 
                  aes(x     = predTickets,
                      y     = predPrices,
                      color = cluster,
                      shape = border))                      +
  annotate("rect", xmin = 45000 - confint(model)[36,2], 
           xmax = 45000, 
           ymin = 0,  ymax = 60,
           alpha = .4, fill = 'coral')                      +
  geom_point(size = 3,alpha = 1)                            +
  geom_segment(aes(x = 39000, y = 50, 
                   xend = 37500, yend = 41),
               arrow = arrow(length = unit(0.5, "cm")),
               color = 'black')                             +
  scale_color_manual(values = palette)                      +
  scale_x_continuous(label = scales::comma)                 +
  scale_y_continuous(label = scales::dollar)                +
  xlab(x_label)                                             + 
  ylab(y_label)                                             + 
  ggtitle(title)                                            +
  graphics_theme_1

#-----------------------------------------------------------------
# Get Confidence Intervals
#-----------------------------------------------------------------
candidate <- subset(season_2025, 
                    season_2025$border == T & cluster == 2)

#-----------------------------------------------------------------
# Chapter 9
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Create spend data
#-----------------------------------------------------------------
sth <- as.data.frame(matrix(nrow=1000,ncol = 2))
fan <- as.data.frame(matrix(nrow=1000,ncol = 2))

names(sth) <- c('spend','type')
names(fan) <- c('spend','type')

set.seed(715)
sth$spend <- rnorm(1000,25,7)
fan$spend <- rnorm(1000,18,7)
sth$type  <- 'sth'
fan$type  <- 'fan'

# Sample data
set.seed(354)
sth_samp <- sth[sample(nrow(sth), 400), ]
fan_samp <- fan[sample(nrow(fan), 400), ]

#-----------------------------------------------------------------
# T Test
#-----------------------------------------------------------------
t_test <- t.test(sth_samp$spend,fan_samp$spend)

#-----------------------------------------------------------------
# Power analysis
#-----------------------------------------------------------------
effect_size <- 3/sd(sth_samp$spend)
signif_lvl  <- .05
certainty   <- .9 

pwr_test <- pwr::pwr.t.test(d         = effect_size,
                            sig.level = signif_lvl,
                            power     = certainty,
                            type      = 'two.sample')

#-----------------------------------------------------------------
# Factorial Design
#-----------------------------------------------------------------
library(AlgDesign)

fd <- gen.factorial(levels   = 3,
                    nVars    = 2,
                    center   = TRUE,
                    factors  = FALSE,
                    varNames = c("Dugout Seats",
                                 "Homeplate Seats"))

#-----------------------------------------------------------------
# Survey Responses
#-----------------------------------------------------------------
survey_responses <- 
  tibble::tibble(dugout = factor(unlist(rep(fd[1],1000))),
                 homePlate = factor(unlist(rep(fd[2],1000))),
                 selection = NA)

survey_responses$dugout <- 
  sapply(survey_responses$dugout, 
         function(x) switch(x,'-1' = '75',
                              '0' = '85',
                              '1' = '95'))

survey_responses$homePlate <- 
  sapply(survey_responses$homePlate, 
         function(x) switch(x,'-1' = '75',
                              '0' = '85',
                              '1' = '95'))

survey_responses$respondent <- 
  c(rep('sth',4500), rep('sin',4500))

selection <- list()
x <- 1
set.seed(755)
while(x <= nrow(survey_responses)){
  
  s1 <- survey_responses[x,1] 
  s2 <- survey_responses[x,2]
  
  selection_list <- c(s1,s2)
  
  selection[x] <- sample(selection_list, 
                         size = 1, 
                         prob = c(.2,.8))
  x <- x + 1
  
}
survey_responses$selection <- unlist(selection)
survey_responses$section  <- 
  ifelse(survey_responses$selection == survey_responses$dugout,
         'dugout',
         'homePlate')

#-----------------------------------------------------------------
# Interaction Plot
#-----------------------------------------------------------------
ip_data <- survey_responses    %>%
  group_by(respondent,section) %>%
  summarise(meanSelection = mean(as.numeric(selection)))
g_xlab  <- 'Respondent Type'                     
g_ylab  <- 'Mean Price'                         
g_title <- 'Interaction Plot, respondents and location'

ip_plot <- 
  ggplot(ip_data, aes(x = respondent,
                      y = meanSelection,
                      color = section,
                      group = section))      +
  geom_point(shape=2, size=3)                +
  geom_line(size = 1.1)                      +
  scale_y_continuous(label = scales::dollar) +
  scale_color_manual(values = palette)       +
  xlab(g_xlab)                               + 
  ylab(g_ylab)                               + 
  ggtitle(g_title)                           +
  graphics_theme_1

#-----------------------------------------------------------------
# Calculating scores for perceptual data
#-----------------------------------------------------------------
library(anacor)
pd <- as.data.frame(FOSBAAS::perceptual_data)
row.names(pd) <- c('Chickens','Titans','Predators') 

anHolder <- anacor(pd,ndim=2)

anHolderGG <- 
  data.frame(dim1 = c(anHolder$col.scores[,1],
                      anHolder$row.scores[,1]), 
             dim2 = c(anHolder$col.scores[,2],
                      anHolder$row.scores[,2]),
             type = c(rep(1,length(anHolder$col.scores[,1])),
                      rep(2,length(anHolder$row.scores[,1]))))

#-----------------------------------------------------------------
# Create perceptual map
#-----------------------------------------------------------------
library(scales)
library(RColorBrewer)
library(grid)
library(gridExtra)

basePal     <- c('grey60','mediumseagreen','grey40','steelblue1')

g_xlab     <- '\n PC1 (55.7%)'
g_ylab     <- 'PC2 (44.3%) \n'
g_title    <- 'Nashville Area Sports Perceptual Map \n'

PM <- 
  ggplot(data = anHolderGG,aes(x=dim1,y=dim2,
                               colour=factor(type)))         +
  geom_point(size = .5, fill = 'white', colour = 'White')    +
  geom_segment(data = anHolderGG, aes(x = 0, y = 0, 
                                      xend = dim1, 
                                      yend = dim2), 
               arrow = arrow(length=unit(0.2,"cm")), 
               alpha = 0.75, color = "steelblue2")           +
  geom_text(aes(label       =  rownames(anHolderGG)),
            size         = 3.2,
            position     = "jitter")                         +
  scale_color_manual(values = basePal,
                     name   = 'Perception',
                     breaks = c(1,2),
                     guide  = FALSE)                         + 
  xlab(g_xlab)                                               + 
  ylab(g_ylab)                                               + 
  ggtitle(g_title)                                           + 
  graphics_theme_1                                           +
  geom_vline(xintercept = 0,lty=4,alpha = .4)                +
  geom_hline(yintercept = 0,lty=4,lpha = .4)                 +
  coord_cartesian(xlim = c(-.7,.9))       

#-----------------------------------------------------------------
# Chapter 10
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Access scan data
#-----------------------------------------------------------------
scan_data <- FOSBAAS::scan_data

#-----------------------------------------------------------------
# Scans
#-----------------------------------------------------------------
x_label <- 'observation'                                             
y_label <- 'scans'                                          
title   <- 'Ticket scans by minute at gates'
scan_point <- 
  ggplot(data = scan_data,aes(x     = observations,
                              y     = scans,
                              color = date))                +
  geom_point()                                              +
  scale_x_continuous(label = scales::comma)                 +
  scale_y_continuous(label = scales::comma)                 +
  scale_color_manual(values = palette)                      +
  xlab(x_label)                                             + 
  ylab(y_label)                                             + 
  ggtitle(title)                                            +
  stat_smooth(method = "lm", formula = y ~ x + poly(x,2)-1) +
  geom_vline(xintercept = 151, lty = 4)                     +
  graphics_theme_1                                          +
  guides(color = 
         guide_legend(override.aes = list(fill = "grey99")))

#-----------------------------------------------------------------
# Scans
#-----------------------------------------------------------------
max_scans <- scan_data                     %>% 
  group_by(date)                           %>%
  summarise(maxScans = max(scans),
            maxScansMean = (max(scans)/16),
            meanScans = mean(scans),
            medianScans = median(scans))

#-----------------------------------------------------------------
# Create scan data
#-----------------------------------------------------------------
scans_a <- f_get_scan_data(x_value = 230,
                           y_value = 90,
                           seed    = 714,
                           sd_mod  = 10)

#-----------------------------------------------------------------
# Create line length data
#-----------------------------------------------------------------
line_length_a <- f_get_line_length(seed = 755,
                                   n    = 300,
                                   u1   = 22,
                                   sd1  = 8,
                                   u2   = 8 ,
                                   sd2  = 5)

line_length_a$action_time <- f_get_time_observations(17,21)
line_length_a$date <- "4/1/2024"

#-----------------------------------------------------------------
# Line length and scans
#-----------------------------------------------------------------
scans_a$cumScans <- cumsum(scans_a$scans)
data  <- dplyr::left_join(scans_a,line_length_a, 
                          by = "action_time")
data$color <- as.numeric(as.character(gsub(":",
                                           "",
                                           data$action_time)))

x_label  <- ('\n Cumulative Scans')
y_label  <- ('Line Length \n')
title    <- ('Line Length and cumulative scans')
legend   <- ('Time')
line_length <- 
  ggplot(data, aes(y     = lineLength, 
                   x     = cumScans, 
                   color = color))                           +    
  geom_point()                                               +
  scale_color_gradient(breaks = c(2100,2000,1900,1800,1700),
                       labels = c("9:00","8:00", "7:00", 
                                  "6:00","5:00"),
                       high = 'dodgerblue',
                       low  = 'coral',
                       name = legend)                        +
  scale_x_continuous(label = scales::comma)                  +
  xlab(x_label)                                              + 
  ylab(y_label)                                              + 
  ggtitle(title)                                             +
  stat_smooth(method = "loess", formula = y ~ x)             +
  geom_vline(xintercept = 13068, lty = 4)                    +
  graphics_theme_1 

#-----------------------------------------------------------------
# Generalized additive model
#-----------------------------------------------------------------
library(mgcv)
scans_a$cumScans <- cumsum(scans_a$scans)
data             <- dplyr::left_join(scans_a,line_length_a, 
                                     by = "action_time")

data$color <- as.numeric(as.character(gsub(":",
                                           "",
                                           data$action_time)))
gam1 <- mgcv::gam(lineLength ~ s(cumScans, bs='ps', sp=.2), 
                  data = data)

newPredict <- cbind(data, predict(gam1, interval = 'confidence'))

gr <- 
  ggplot(newPredict, aes(y     = lineLength, 
                         x     = cumScans,
                         color = color))                      +    
  geom_point(alpha=.7)                                        +
  scale_color_gradient(breaks = c(2100,2000,1900,1800,1700),
                       labels = c("9:00","8:00", "7:00", 
                                  "6:00","5:00"),
                       high = 'dodgerblue',
                       low  = 'coral',
                       name = 'Time')                         +
  geom_line(aes(y = `predict(gam1, interval = "confidence")`,
                x = cumScans),
            color = 'dodgerblue',size = 1.2)                  +
  scale_x_continuous(label = scales::comma)                   +
  xlab('Cumulative Scans')                                    + 
  ylab('Line-Length')                                         + 
  ggtitle('Results of GAM on Line-Length Data')               +
  graphics_theme_1   

#-----------------------------------------------------------------
# Simulate wait times function
#-----------------------------------------------------------------
f_get_wait_times <- function(seed,n = 300,time,rate1,rate2,rate3){
  set.seed(seed)
  
  order_times       <- rexp(n, rate = rate1)
  payment_times     <- rexp(n, rate = rate2)
  fulfillment_times <- rexp(n, rate = rate3)
  total_time        <- order_times       + 
    payment_times     + 
    fulfillment_times
  
  wait_times <- data.frame(transaction  = seq(1,n, by = 1),
                           orderTimes   = order_times,
                           paymentTimes = payment_times,
                           fulfillTimes = fulfillment_times,
                           totalTime    = total_time)
  wait_times[] <- apply(wait_times,2,function(x) round(x,0))
  return(wait_times)
}

#-----------------------------------------------------------------
# Create wait times data set
#-----------------------------------------------------------------
wait_times_a <- f_get_wait_times(seed  = 755,
                                 n     = 300,
                                 rate1 = .03,
                                 rate2 = .06,
                                 rate3 = .15)

#-----------------------------------------------------------------
# Simulate wait times function
#-----------------------------------------------------------------
wait_dist <- 
  wait_times_a                                          %>% 
  select(orderTimes,paymentTimes,fulfillTimes)          %>%
  tidyr::pivot_longer(cols = c('orderTimes',
                               'paymentTimes',
                               'fulfillTimes'),
                      names_to = "measurment",
                      values_to = "seconds")            %>%
  mutate(scale_seconds = scale(seconds))

w_dist <-  
  ggplot(wait_dist, aes(x = seconds, fill= measurment)) +    
  geom_density(alpha=.75)                               +
  geom_rug(color='coral',sides = "b")                   +
  scale_fill_manual(values=palette)                     + 
  xlab('Seconds')                                       +
  ylab('Density')                                       + 
  ggtitle('Distribution of wait-time components')       +
  graphics_theme_1 

#-----------------------------------------------------------------
# Simulate wait times function
#-----------------------------------------------------------------
wait_times <- FOSBAAS::wait_times_data[1:300,]

#-----------------------------------------------------------------
# Build correlation table
#-----------------------------------------------------------------
wt_cor <- wait_times[,-1]
names(wt_cor) <- c('Order Time','Payment Time',
                   'Fulfillment Time','Total Time')

wt_cor_result      <- cor(wt_cor)
wt_cor_result      <- round(as.data.frame(wt_cor_result), 2)
wt_cor_result$type <- row.names(wt_cor_result)

cor_long <- 
  tidyr::pivot_longer(wt_cor_result,
                      cols = c('Order Time','Payment Time', 
                               'Fulfillment Time','Total Time'))

#-----------------------------------------------------------------
# Build correlation table
#-----------------------------------------------------------------
cor_long$order <- factor(cor_long$type, 
                         levels=c('Fulfillment Time',
                                  'Payment Time',
                                  'Order Time',
                                  'Total Time'))

#-----------------------------------------------------------------
# Correlation table 
#-----------------------------------------------------------------
library(forcats)
correlation_table <- 
  cor_long                                                 %>%
  mutate(name = fct_reorder(name, value, .fun='sum'))      %>%
  ggplot(aes(x = order, y = name,fill = value))          +
  geom_tile()                                            +
  geom_text(aes(label=value))                            +
  scale_fill_gradient2(low  = "mediumseagreen", 
                       mid  = "white", 
                       high = "dodgerblue")              +
  xlab('')                                               + 
  ylab('')                                               + 
  ggtitle('Correlation table')                           +
  graphics_theme_1                                       +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))

#-----------------------------------------------------------------
# Box plot of wait times
#-----------------------------------------------------------------
wait_long <- 
  tidyr::pivot_longer(wait_times,cols = c('orderTimes',
                                          'paymentTimes', 
                                          'fulfillTimes',
                                          'totalTime'))
wait_box <- 
  wait_long                                             %>%
  mutate(name = fct_reorder(name, value, .fun='sum'))   %>%
  ggplot(aes(x = name, y = value)) +
  geom_boxplot(fill = 'dodgerblue') +
  xlab('\n Transaction component')                       + 
  ylab('Wait time in seconds')                           + 
  ggtitle('Variance of transaction times')               +
  graphics_theme_1

#-----------------------------------------------------------------
# Quantiles
#-----------------------------------------------------------------
quantiles <-
  apply(wait_times[,-1],
        2,
        function(x) quantile(x, probs = c(.1,0.25, 0.5, 0.75,.9)))

#-----------------------------------------------------------------
# Simple linear model for total times
#-----------------------------------------------------------------
time_mod <- lm(totalTime ~ fulfillTimes + paymentTimes + 
                 orderTimes,data = wait_times)

stats_time_mod <- 
  tibble::tibble(
    st_error     = unlist(summary(time_mod)[6]),
    r_square     = unlist(summary(time_mod)[8]),
    adj_r_square = unlist(summary(time_mod)[9]),
    f_stat       = unlist(summary(time_mod)$fstatistic[1])
  )

#-----------------------------------------------------------------
# Acess the distribution data
#-----------------------------------------------------------------
library(FOSBAAS)

wait_times_distribution <- FOSBAAS::wait_times_distribution_data

#-----------------------------------------------------------------
# Get a sample of the data
#-----------------------------------------------------------------
set.seed(755)
wait_sample <- wait_times_distribution %>%
  sample_frac(size = .7)

#-----------------------------------------------------------------
# Cumulative distribution function
#-----------------------------------------------------------------
orders <- wait_sample$orderTimes 
cdf <- ecdf(orders) 

#-----------------------------------------------------------------
# Cumulative distribution function
#-----------------------------------------------------------------
cdf_out <- cdf(50)
qua_out <- quantile(wait_sample$orderTimes,probs = cdf_out)

ecdf_quant <- tibble::tibble(ecdf_50 = cdf_out,
                             quantile_ecdf_50 = qua_out)
ecdf_quant

#-----------------------------------------------------------------
# Cumulative distribution function plot
#-----------------------------------------------------------------
cdf_plot <- 
  ggplot(wait_sample, aes(orders))          + 
  stat_ecdf(geom = "step",
            color = 'dodgerblue',
            size = 1.1)                     +
  xlab('\n Orders')                         + 
  ylab('Percentage observations')           + 
  ggtitle('ECDF Order Time')                +
  geom_vline(xintercept = 50.22143,lty = 2) +
  geom_hline(yintercept = .7785714,lty = 2) +
  graphics_theme_1

#-----------------------------------------------------------------
# Simulate orders 
#-----------------------------------------------------------------
set.seed(715)
n <- 400 # observations
sim_orders <- rexp(n, rate=1/mean(wait_sample$orderTimes))

#-----------------------------------------------------------------
# Compare histograms
#-----------------------------------------------------------------
hist_tab <- 
  tibble::tibble(sim_data = sim_orders,
                 actual_data = sample(wait_sample$orderTimes,400)
 )

hist_tab_long <- hist_tab %>% 
  tidyr::pivot_longer(cols      = c('sim_data',
                                    'actual_data'),
                      names_to  = "measurment",
                      values_to = "seconds") 

hist_comp <- 
  ggplot(hist_tab_long , aes(seconds))    + 
  facet_grid(.~measurment)               +
  geom_histogram(fill = 'dodgerblue')    +
  xlab('\n seconds')                     + 
  ylab('count')                          + 
  ggtitle('Simulated vs. actual values') +
  graphics_theme_1

#-----------------------------------------------------------------
# Simulate, Iterate, and aggregate
#-----------------------------------------------------------------
n <- 500 # observations

sim_orders  <- list()
sim_pay     <- list()
sim_fulfill <- list()
# Iterate
for(i in 1:500){
  set.seed(i + 715)
  # Simulate
  sim_orders[[i]]  <-  
    rexp(n, rate=1/mean(wait_sample$orderTimes))
  sim_pay[[i]]     <-  
    rexp(n, rate=1/mean(wait_sample$paymentTimes))
  sim_fulfill[[i]] <-  
    rexp(n, rate=1/mean(wait_sample$fulfillTimes))
}
# Aggregate
mean_order   <- mean(sapply(sim_orders, mean))
mean_pay     <- mean(sapply(sim_pay, mean))
mean_fulfill <- mean(sapply(sim_fulfill, mean))
mean_total   <- mean_order + mean_pay + mean_fulfill

#-----------------------------------------------------------------
# Aggregated results
#-----------------------------------------------------------------
mean_chart <- tibble::tibble(order   = mean_order,
                             payment = mean_pay,
                             fulfill = mean_fulfill,
                             total   = mean_total)

#-----------------------------------------------------------------
# Results with variance
#-----------------------------------------------------------------
total_time_samp <- sim_orders[[1]]  +
  mean_pay[[1]]    + 
  mean_fulfill[[1]]
count_list <- list()
set.seed(715)
for(j in 1:30){
  time            <- 0
  count           <- 1
  seconds_in_hour <- 60*60
  while(time <= seconds_in_hour){
    i           <- sample(total_time_samp,1)
    time        <- time + i
    count       <- count + 1
  }
  count_list[j]   <- count - 1
}

#-----------------------------------------------------------------
# Observe variance in fans serviced per hour
#-----------------------------------------------------------------
counts <- tibble::tibble(fans_serviced = unlist(count_list),
                         simulation    = seq(1:30))

service_per_hour <- 
  ggplot(counts , aes(x = simulation,
                      y = fans_serviced)) + 
  geom_line(color = 'dodgerblue')         +
  xlab('\n simulation')                   + 
  ylab('Fans serviced')                   + 
  ggtitle('Simulated services per hour')  +
  geom_hline(yintercept = 64,lty = 4)     +
  graphics_theme_1

#-----------------------------------------------------------------
# Function to build a frequency table
#-----------------------------------------------------------------
f_build_freq_table <- function(variable){
  
  pr          <- as.data.frame(table(variable))
  pr$prob     <- pr$Freq/sum(pr$Freq)
  pr$variable <- as.numeric(as.character(pr$variable))  
  
  return(pr)
  
}

order_freq <- f_build_freq_table(wait_sample$orderTimes)
#sum(order_freq$prob)

#-----------------------------------------------------------------
# Access frequency table data
#-----------------------------------------------------------------
freq_table  <- FOSBAAS::freq_table_data

freq_table$cumprob <- cumsum(freq_table$prob)
freq_table_graph <- 
  ggplot(freq_table,aes(x = variable,y=cumprob)) +
  geom_line(size = 1.2,color = 'dodgerblue')     +
  xlab('\n Seconds')                             + 
  ylab('Percent of Values')                      + 
  ggtitle('Table of values')                     +
  graphics_theme_1

#-----------------------------------------------------------------
# Apply fits
#-----------------------------------------------------------------
library(mgcv)
freq_table$cumprob <- cumsum(freq_table$prob)
#-----------------------------------------------------------------
# Exponential fit
fit_ex <- 
  nls(variable ~ a*cumprob^m, data = freq_table, 
      start = list(a = 300,m=.15)) 
freq_table$pred_exp <- predict(fit_ex)
#-----------------------------------------------------------------
# Polynomial fit
fit_py <- 
  lm(freq_table$variable~poly(freq_table$cumprob,5,raw=TRUE))
freq_table$pred_poly <- predict(fit_py)
#-----------------------------------------------------------------
# GAM fit
fit_gm <- 
  mgcv::gam(variable ~ s(cumprob),data = freq_table)
freq_table$pred_gam <- predict(fit_gm)

#-----------------------------------------------------------------
# Apply logit fit
#-----------------------------------------------------------------
fit_lt <- nls(variable ~ SSlogis(cumprob, Asym, xmid, scal), 
              freq_table)
cof    <- coef(summary(fit_lt))

fit <- nls(variable ~ A/(1 + exp(((-I+cumprob)/S))), 
           data = freq_table,  
           start = list(A = cof[1],I= cof[2],S = -cof[3]), 
           control = list(maxiter  =  10000), trace=TRUE)

#-----------------------------------------------------------------
# Spline fit
#-----------------------------------------------------------------
fit_sp <- with(freq_table, smooth.spline(cumprob, variable))
freq_table$pred_sp <- predict(fit_sp)$y

#-----------------------------------------------------------------
# Observe fit data
#-----------------------------------------------------------------

dist_fits <- 
  ggplot(freq_table,aes(y = cumprob,x = variable))           +
  geom_point(alpha = .5,size = 1)                            +
  geom_line(aes(x = pred_exp), size = 1.1 , lty = 2, 
            color = 'dodgerblue')                            +
  geom_line(aes(x = pred_poly), size = 1.1, lty = 3, 
            color = 'mediumseagreen')                        + 
  geom_line(aes(x = pred_gam), size = 1.1, lty = 4, 
            color = 'coral')                                 +
  geom_line(aes(x = pred_sp), size = 1.1, lty =5, 
            color = 'orchid')                                +
  ylab('\n Cumulative Probability')                          + 
  xlab('Order time in seconds')                              + 
  ggtitle('Distribution of order times')                     + 
  graphics_theme_1

#-----------------------------------------------------------------
# Compare fits
#-----------------------------------------------------------------
models <- list(expon  = fit_ex,
               poly   = fit_py,
               gam    = fit_gm)

get_diagnostics <- function(mods){
  mods <- models
  aics   <- lapply(mods, function(x) AIC(x))
  bics   <- lapply(mods, function(x) BIC(x))  
  frame <- as.data.frame(matrix(nrow = length(mods), ncol = 3))
  frame[,1] <- names(mods)
  frame[,2] <- unlist(aics)
  frame[,3] <- unlist(bics)
  names(frame) <- c('model','AIC','BIC')
  return(frame)
  
}

models_table <- get_diagnostics(models)

#-----------------------------------------------------------------
# get fit
#-----------------------------------------------------------------
f_get_fifth_degree_fit <- function(new_var,dist_fit){
  var <-  coef(dist_fit)[1]              + 
    (coef(dist_fit)[2] * new_var    + 
       (coef(dist_fit)[3] * new_var^2) + 
       (coef(dist_fit)[4] * new_var^3) +
       (coef(dist_fit)[5] * new_var^4) + 
       (coef(dist_fit)[6] * new_var^5))
  return(var)
}

#-----------------------------------------------------------------
# Equation output
#-----------------------------------------------------------------
f_get_fifth_degree_fit(.7785714,fit_py)

#-----------------------------------------------------------------
# get fit
#-----------------------------------------------------------------
poly_fit <- 
  sapply(seq(0,1,by=.005),
         function(x) f_get_fifth_degree_fit(x,fit_py))

poly_values <- tibble::tibble(y = seq(0,1,by=.005),
                              x = poly_fit)
poly_graph <- 
  ggplot(poly_values,aes(x=x,y=y))           +
  geom_line(size = 1.5, lty = 3, 
            color = 'mediumseagreen')        +
  ylab('\n Cumulative Probability')          + 
  xlab('Order time in seconds')              + 
  ggtitle('Simulated order times')           + 
  graphics_theme_1

#-----------------------------------------------------------------
# Define terms for queuing equation
#-----------------------------------------------------------------
lambda                # Average arrivals per unit of time
mu                    # Average number of units processed 
k      <- 6           # Number of points of sale
N      <- 50          # Number that the queue can accommodate
tau_a  <- 1/lambda    # Average time between arrivals: 110 seconds
tau_s  <- 1/mu        # Average service time: 90 seconds
rho    <- tau_a/tau_s # Utilization ratio
n                     # Units in the system

#-----------------------------------------------------------------
# M/M/k/N Queue Inputs
#-----------------------------------------------------------------
k        = 2          # Number of points of sale at concept
N        = 5          # Number of people the queue can accommodate
tau_a    = 10         # time between arrivals: 40 seconds/60
tau_s    = 8          # Service time: 90 seconds/60 = 1.5
lambda   = 1/tau_a    # Customer Arrivals per minute
mu       = 1/tau_s    # Serviced customers per minute
#-----------------------------------------------------------------
# Translate to per hour figures
#-----------------------------------------------------------------
lambda_h = 60/tau_a   # Per hour
mu_h     = 60/tau_s   # Per hour
rho      = lambda/mu  # Utilization ratio

#-----------------------------------------------------------------
# M/M/k/N Calculating the Probability that n = 0
#-----------------------------------------------------------------
# Create the sequence for the P0 equation
n   = seq(0, N-1, by = 1 )
# Translate the equation into R:
P0 <- 
1/ sum(((rho^n)/factorial(n)) + ((rho^k)/
(factorial(k)*((k^(N-k+1)) - (rho^(N-k+1))/((k-rho)*(k^(N-k)))))))

#-----------------------------------------------------------------
# M/M/k/N  Probability of n units in the system
#-----------------------------------------------------------------

# For n = (0,k)
nk0 = seq(0, k, by = 1 )
Pnk0 <- rho^nk0/factorial(nk0)*P0

sum(Pnk0)

# For n = (k + 1,N)
nk1 = seq(k + 1, N, by = 1 )
Pnk1 <- rho^nk1/(factorial(k)*k^(nk1-k))*P0

sum(Pnk1)

round(sum(Pnk0,Pnk1),2)

#-----------------------------------------------------------------
# Calculate figures
#-----------------------------------------------------------------
lambda_e <- lambda*(1 - Pnk1) # Lambda Effective
rho_e    <- lambda_e/mu       # rho Effective
# Expected in queue
Lq = sum((n-k)*Pnk0)
Lq = sum((n-k)*Pnk1)
Ls = rho_e
# expected units in the system
L = Ls + Lq
# Expected service time
Ws = Ls/lambda_e # minutes in service
Wq = Lq/lambda_e # minutes in queue
W = L/lambda_e   # minutes in system


#-----------------------------------------------------------------
# Calculate figures
#-----------------------------------------------------------------
f_get_MMKN <- function(k,N,ta,ts){
  
  lambda = 1/ta #: per minute
  mu     = 1/ts #: per minute
  rho    = lambda/mu #: utilization ratio
  
  #-----------------------------------------------------------------
  # Probability of n units in the system
  # for
  n = seq(0, N-1, by = 1 )
  P0 <- 1/ sum(((rho^n)/factorial(n)) + 
                 ((rho^k)/(factorial(k)*((k^(N-k+1)) - 
                                           (rho^(N-k+1))/((k-rho)*(k^(N-k)))))))
  
  # Probability of n units in the system
  # for
  n = seq(0, k, by = 1 )
  Pn0 <- rho^n/factorial(n)*P0
  
  # for
  n = seq(k + 1, N, by = 1 )
  Pn1 <- rho^n/(factorial(k)*k^(n-k))*P0
  
  Pn      <- c(Pn0,Pn1)
  
  #-------------------------------------------------------------------
  # calculations
  len     <- max(length(Pn))
  
  lambda_e  <- lambda*(1 - Pn[len])
  rho_e    <- lambda_e/mu
  
  # Expected in queue
  Ls = rho_e #   Ls = 1*Pn[2] + 2*sum(Pn[-c(1,2)])
  
  # for
  n = seq(k+2, N + 1, by = 1 )
  Lq = sum((n-(k+1))*Pn[n]) # Lq = 1*Pn[4] + 2*Pn[5] + 3*Pn[6]
  
  # expected units in the system
  L = Ls + Lq
  
  # Expected service time
  Ws = Ls/lambda_e # minutes in service
  Wq = Lq/lambda_e # minutes in queue
  W  = Wq + Ws   # minutes in system
  
  #-------------------------------------------------------------------
  # Build output
  frame <- data.frame(matrix(nrow = 7,ncol =2))
  names(frame) <- c('Metric','Value')
  
  metric <- c('Servers:','System Capacity:','Time between arrivals:',
              'Average service time:','Minutes in service:',
              'Minutes in queue:','Minutes in system:')
  values <- c(k,N,ta,ts,Ws,Wq,W)
  
  frame[,1] <- metric
  frame[,2] <- values
  
  return(frame)
  
}

#-----------------------------------------------------------------
# Run our function
#-----------------------------------------------------------------
FOSBAAS::f_get_MMKN(2,5,10,8)









>>>>>>> 3591d42dce40b52c2b7a13e4961d6fe0ef98ab9e
