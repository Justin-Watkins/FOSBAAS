#-----------------------------------------------------------------
# aggregated_crm_data
#
# This data set approximates a processed CRM data set on call counts and revenue
#
#-----------------------------------------------------------------


#-----------------------------------------------------------------
# Begin aggregated CRM data
#-----------------------------------------------------------------

ag_sales_data <- as.data.frame(matrix(nrow = 5000,ncol = 3))
names(ag_sales_data) <- c('repID','call','revenue')

set.seed(755)

ag_sales_data$repID <-  rep(sapply(seq(10), function(x) paste(sample(c(0:9, LETTERS),
                                                                     12,
                                                                     replace=TRUE),
                                                              collapse = "")),
                            500)

calls <- sample(1:5,10000,prob = c(.20,.25,.30,.2,.05),replace = TRUE)

revenue <- c(rnorm(25000,3000,800),rnorm(500,10000,1000),
             rnorm(2000,500,60),rnorm(500,50000,8000),
             rep(0,30000))

ag_sales_data$revenue <- sample(revenue,nrow(ag_sales_data))
ag_sales_data$call    <- sample(calls,nrow(ag_sales_data))

#readr::write_csv(ag_sales_data,"aggregated_crm_data.csv")
