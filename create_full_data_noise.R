# Set the correct directory
setwd('/home/teodor/Desktop/projects/papers/journal1/code/')

# Add noise to the dataset
dataset <- readr::read_csv("results/full_data.csv") %>% mutate(SD=0) 
accuracy_noise <- as.data.frame(dataset["accuracy"])
for(j in 1:dim(accuracy_noise)[1]){
  accuracy_noise[j,] <-rnorm(n=1,mean = accuracy_noise[j,], sd=3)
  if(accuracy_noise[j,]>=100){
    accuracy_noise[j,] <- 100
  }
}

dataset1 <- readr::read_csv("results/full_data.csv") %>% mutate(SD=3, accuracy=c(accuracy_noise$accuracy))
dataset_complete <- rbind(dataset, dataset1)
write.csv(dataset_complete,"results/full_data_noise.csv")