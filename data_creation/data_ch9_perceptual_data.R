#-----------------------------------------------------------------
# perceptual_data data set
#
# This data set approximates a processed perceptual survey
#
#-----------------------------------------------------------------


#-----------------------------------------------------------------
# Begin perceptual data
#-----------------------------------------------------------------
# Create our data set
perceptual_data <- as.data.frame(matrix(nrow=3,ncol=10))
names(perceptual_data)      <- c('Friendly','Exciting','Fresh','Inovative','Fun',
                                 'Old','Historic','Winners','Great','Expensive')
row.names(perceptual_data)  <- c('Chicken Hearts','Grizzlies','Predators')

set.seed(2632)
perceptual_data <- apply(perceptual_data,1:2,function(x) round(rnorm(1,3000,1000),0))

#write.csv(perceptual_data,"perceptual_data.csv",row.names=FALSE)
