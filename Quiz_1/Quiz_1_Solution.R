file=read.csv("C:/Users/rohit/Downloads/JDT.csv",header=T,row.names=1)
summary(file)

#Simple Regression

foundbug=lm(bug~bugfound,data=file)
located=lm(bug~loc,data=file)
version=lm(bug~version,data=file)
everything=lm(bug~bugfound+loc+version,data=file)

#R^2 

summary(foundbug)$r.squared
summary(located)$r.squared
summary(version)$r.squared
summary(everything)$r.squared

#Cross Validation

cross.validation = function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
  size = nrow(dataset)
  test.size = ratio*size
  set.seed(02236773)
  for(i in 1:iterations)
  {
    
    test.idx = sample.int(size,test.size)
    test.data = dataset[test.idx, ]
    train.data = dataset[-test.idx, ]
    model =lm(form, data=train.data)
    pred.result = predict(model,test.data)
    actual.result = test.data$bug
    result[i] = mean(abs(actual.result - pred.result)) #mean absolute error
  }
  result
}

foundbug1=cross.validation(bug~bugfound,file)
located1=cross.validation(bug~loc,file)
version1=cross.validation(bug~version,file)
everything1=cross.validation(bug~bugfound+loc+version,file)


mean(foundbug1)
mean(located1)
mean(version1)
mean(everything1)

