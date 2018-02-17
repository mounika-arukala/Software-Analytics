file=read.csv("C:/Users/mouni/Desktop/effort.csv",header=T,row.names=1)
summary(file)

foundchange=lm(Change~File,data=file)
foundhour=lm(Hour~File,data=file)


#R^2 
#To access a column in a dataset use $
#r.squared is a column in summary 

summary(foundchange)$r.squared 
summary(foundhour)$r.squared

cross.validation = function(form, dataset, iterations = 75, ratio = 0.10)
{
  resultchange = rep(0,iterations)
  size = nrow(dataset) #total number of rows in the dataset
  test.size = ratio*size #number of rows in the testing set
  set.seed(02236773)
  for(i in 1:iterations)
  {
    
    test.idx = sample.int(size,test.size) #equation1 #will generate few row numbers randomly for testing data
    test.data = dataset[test.idx, ]#data in those row numbers is collected and used as testing data
    train.data = dataset[-test.idx, ]
    model =lm(form, data=train.data)# model is built using training data
    pred.result = predict(model,test.data)
    actualchange.result = test.data$Change
    resultchange[i] = mean(abs(actualchange.result - pred.result)) #mean absolute error
  }
  resultchange
}

foundchange1=cross.validation(Change~File,file) # function call to cross.validation function

mean(foundchange1)


cross.validation1 = function(form, dataset, iterations = 75, ratio = 0.10)
{
  resulthour = rep(0,iterations)
  size = nrow(dataset) #total number of rows in the dataset
  test.size = ratio*size #number of rows in the testing set
  set.seed(02236773)
  for(i in 1:iterations)
  {
    
    test.idx = sample.int(size,test.size) #equation1 #will generate few row numbers randomly for testing data
    test.data = dataset[test.idx, ]#data in those row numbers is collected and used as testing data
    train.data = dataset[-test.idx, ]
    model =lm(form, data=train.data)# model is built using training data
    pred.result = predict(model,test.data)
    actualhour.result=test.data$Hour
    resulthour[i] = mean(abs(actualhour.result - pred.result)) #mean absolute error
  }
  resulthour
}

foundhour1=cross.validation1(Hour~File,file) # function call to cross.validation function


mean(foundhour1)


