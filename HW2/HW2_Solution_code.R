load.data = function(folder)
{
  old.wd=getwd()
  setwd(folder)
  code=read.csv("single-version-ck-oo.csv",header=T,sep=';',row.names=1)
  bug=read.csv("bug-metrics.csv",header=T,sep=';',row.names=1)
  change=read.csv("change-metrics.csv",header=T,sep=';',row.names=1)
  setwd(old.wd)
  data.frame(code[,1:17], change[,1:15], bug[,1:6])
  
}

jdt = load.data("C:/Users/mouni/Downloads/jdt/eclipse")
pde = load.data("C:/Users/mouni/Downloads/pde")
mylyn = load.data("C:/Users/mouni/Downloads/mylyn")
equinox = load.data("C:/Users/mouni/Downloads/equinox")
lucene = load.data("C:/Users/mouni/Downloads/lucene")

colnames(jdt)
colnames(pde)
colnames(mylyn)
colnames(equinox)
colnames(lucene)

#LOC----------------

cross.validation = function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = mean(abs(actual.result - pred.result)) #mean absolute error
  }
  result
}
a=cross.validation(bugs~numberOfLinesOfCode,jdt)
b=cross.validation(bugs~numberOfLinesOfCode,pde)
c=cross.validation(bugs~numberOfLinesOfCode,mylyn)
d=cross.validation(bugs~numberOfLinesOfCode,equinox)
e=cross.validation(bugs~numberOfLinesOfCode,lucene)
mean(a)
mean(b)
mean(c)
mean(d)
mean(e)

cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = cor.test(actual.result, pred.result, method= "spearman")$estimate #mean absolute error
  }
  result
}
a=cross.validation(bugs~numberOfLinesOfCode,jdt)
b=cross.validation(bugs~numberOfLinesOfCode,pde)
c=cross.validation(bugs~numberOfLinesOfCode,mylyn)
d=cross.validation(bugs~numberOfLinesOfCode,equinox)
e=cross.validation(bugs~numberOfLinesOfCode,lucene)
mean(a)
mean(b)
mean(c)
mean(d)
mean(e)


#NBF -------------------------

cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = mean(abs(actual.result - pred.result))#mean absolute error
  }
  result
}
a=cross.validation(bugs~numberOfBugsFoundUntil.,jdt)
b=cross.validation(bugs~numberOfBugsFoundUntil.,pde)
c=cross.validation(bugs~numberOfBugsFoundUntil.,mylyn)
d=cross.validation(bugs~numberOfBugsFoundUntil.,equinox)
e=cross.validation(bugs~numberOfBugsFoundUntil.,lucene)
mean(a)
mean(b)
mean(c)
mean(d)
mean(e)

cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = cor.test(actual.result, pred.result, method= "spearman")$estimate #mean absolute error
  }
  result
}
a=cross.validation(bugs~numberOfBugsFoundUntil.,jdt)
b=cross.validation(bugs~numberOfBugsFoundUntil.,pde)
c=cross.validation(bugs~numberOfBugsFoundUntil.,mylyn)
d=cross.validation(bugs~numberOfBugsFoundUntil.,equinox)
e=cross.validation(bugs~numberOfBugsFoundUntil.,lucene)
mean(a)
mean(b)
mean(c)
mean(d)
mean(e)

#NOV ------------------------

cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = mean(abs(actual.result - pred.result))
  }
  result
}
a=cross.validation(bugs~numberOfVersionsUntil.,jdt)
b=cross.validation(bugs~numberOfVersionsUntil.,pde)
c=cross.validation(bugs~numberOfVersionsUntil.,mylyn)
d=cross.validation(bugs~numberOfVersionsUntil.,equinox)
e=cross.validation(bugs~numberOfVersionsUntil.,lucene)
mean(a)
mean(b)
mean(c)
mean(d)
mean(e)

cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = cor.test(actual.result, pred.result, method= "spearman")$estimate #mean absolute error
  }
  result
}
a=cross.validation(bugs~numberOfVersionsUntil.,jdt)
b=cross.validation(bugs~numberOfVersionsUntil.,pde)
c=cross.validation(bugs~numberOfVersionsUntil.,mylyn)
d=cross.validation(bugs~numberOfVersionsUntil.,equinox)
e=cross.validation(bugs~numberOfVersionsUntil.,lucene)
mean(a)
mean(b)
mean(c)
mean(d)
mean(e)

# Single code metric and change metric

cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = mean(abs(actual.result - pred.result)) #mean absolute error
  }
  result
}
a=cross.validation(bugs~numberOfLinesOfCode+numberOfVersionsUntil.,jdt)
b=cross.validation(bugs~numberOfAttributes+numberOfVersionsUntil.,pde)
c=cross.validation(bugs~fanOut+numberOfVersionsUntil.,mylyn)
d=cross.validation(bugs~cbo+numberOfVersionsUntil.,equinox)
e=cross.validation(bugs~lcom+linesAddedUntil.,lucene)
mean(a)
mean(b)
mean(c)
mean(d)
mean(e)

cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = cor.test(actual.result, pred.result, method= "spearman")$estimate #mean absolute error
  }
  result
}
a=cross.validation(bugs~numberOfLinesOfCode+numberOfVersionsUntil.,jdt)
b=cross.validation(bugs~numberOfAttributes+numberOfVersionsUntil.,pde)
c=cross.validation(bugs~fanOut+numberOfVersionsUntil.,mylyn)
d=cross.validation(bugs~cbo+numberOfVersionsUntil.,equinox)
e=cross.validation(bugs~lcom+linesAddedUntil.,lucene)
mean(a)
mean(b)
mean(c)
mean(d)
mean(e)



# All code metric -------------------------

cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = mean(abs(actual.result - pred.result)) #mean absolute error
  }
  result
}
a=cross.validation(bugs~.,jdt[c(1:17,38)])
b=cross.validation(bugs~.,pde[c(1:17,38)])
c=cross.validation(bugs~.,mylyn[c(1:17,38)])
d=cross.validation(bugs~.,equinox[c(1:17,38)])
e=cross.validation(bugs~.,lucene[c(1:17,38)])
mean(a)
mean(b)
mean(c)
mean(d)
mean(e)

cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = cor.test(actual.result, pred.result, method= "spearman")$estimate #mean absolute error
  }
  result
}
a=cross.validation(bugs~.,jdt[c(1:17,38)])
b=cross.validation(bugs~.,pde[c(1:17,38)])
c=cross.validation(bugs~.,mylyn[c(1:17,38)])
d=cross.validation(bugs~.,equinox[c(1:17,38)])
e=cross.validation(bugs~.,lucene[c(1:17,38)])
mean(a)
mean(b)
mean(c)
mean(d)
mean(e)

#Change metrics ---------------------------------------

cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = mean(abs(actual.result - pred.result)) #mean absolute error
  }
  result
}
a=cross.validation(bugs~.,jdt[c(18:32,38)])
b=cross.validation(bugs~.,pde[c(18:32,38)])
c=cross.validation(bugs~.,mylyn[c(18:32,38)])
d=cross.validation(bugs~.,equinox[c(18:32,38)])
e=cross.validation(bugs~.,lucene[c(18:32,38)])
mean(a)
mean(b)
mean(c)
mean(d)
mean(e)

cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = cor.test(actual.result, pred.result, method= "spearman")$estimate #mean absolute error
  }
  result
}
a=cross.validation(bugs~.,jdt[c(18:32,38)])
b=cross.validation(bugs~.,pde[c(18:32,38)])
c=cross.validation(bugs~.,mylyn[c(18:32,38)])
d=cross.validation(bugs~.,equinox[c(18:32,38)])
e=cross.validation(bugs~.,lucene[c(18:32,38)])
mean(a)
mean(b)
mean(c)
mean(d)
mean(e)

#All available metrics

cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = mean(abs(actual.result - pred.result)) #mean absolute error
  }
  result
}
a=cross.validation(bugs~.,jdt)
b=cross.validation(bugs~.,pde)
c=cross.validation(bugs~.,mylyn)
d=cross.validation(bugs~.,equinox)
e=cross.validation(bugs~.,lucene)
mean(a)
mean(b)
mean(c)
mean(d)
mean(e)

cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = cor.test(actual.result, pred.result, method= "spearman")$estimate #mean absolute error
  }
  result
}
a=cross.validation(bugs~.,jdt)
b=cross.validation(bugs~.,pde)
c=cross.validation(bugs~.,mylyn)
d=cross.validation(bugs~.,equinox)
e=cross.validation(bugs~.,lucene)
mean(a)
mean(b)
mean(c)
mean(d)
mean(e)


#PCA for all metrics ----------------------------------------

#JDT

pca.jdt = princomp(jdt)
summary(pca.jdt)
pca.jdt.data = data.frame(bugs = jdt$bugs, pca.jdt$scores[,1:2])
cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = cor.test(actual.result, pred.result, method= "spearman")$estimate #mean absolute error
  }
  result
}
pca.res = cross.validation(bugs ~ ., data = pca.jdt.data)
mean(pca.res)
cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = mean(abs(actual.result - pred.result)) #mean absolute error
  }
  result
}
pca.res = cross.validation(bugs ~ ., data = pca.jdt.data)
mean(pca.res)

#PDE

pca.pde = princomp(pde)
summary(pca.pde)
pca.pde.data = data.frame(bugs = pde$bugs, pca.pde$scores[,1:3])
cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = cor.test(actual.result, pred.result, method= "spearman")$estimate #mean absolute error
  }
  result
}
pca.res = cross.validation(bugs ~ ., data = pca.pde.data)
mean(pca.res)
cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = mean(abs(actual.result - pred.result)) #mean absolute error
  }
  result
}
pca.res = cross.validation(bugs ~ ., data = pca.pde.data)
mean(pca.res)

#MYLYN

pca.mylyn = princomp(mylyn)
summary(pca.mylyn)
pca.mylyn.data = data.frame(bugs = mylyn$bugs, pca.mylyn$scores[,1:2])
cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = cor.test(actual.result, pred.result, method= "spearman")$estimate #mean absolute error
  }
  result
}
pca.res = cross.validation(bugs ~ ., data = pca.mylyn.data)
mean(pca.res)
cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = mean(abs(actual.result - pred.result)) #mean absolute error
  }
  result
}
pca.res = cross.validation(bugs ~ ., data = pca.mylyn.data)
mean(pca.res)

#EQUINOX

pca.equinox = princomp(equinox)
summary(pca.equinox)
pca.equinox.data = data.frame(bugs = equinox$bugs, pca.equinox$scores[,1:3])
cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = cor.test(actual.result, pred.result, method= "spearman")$estimate #mean absolute error
  }
  result
}
pca.res = cross.validation(bugs ~ ., data = pca.equinox.data)
mean(pca.res)
cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = mean(abs(actual.result - pred.result)) #mean absolute error
  }
  result
}
pca.res = cross.validation(bugs ~ ., data = pca.equinox.data)
mean(pca.res)

# LUCENE
pca.lucene = princomp(lucene)
summary(pca.lucene)
pca.lucene.data = data.frame(bugs = lucene$bugs, pca.lucene$scores[,1:3])
cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = cor.test(actual.result, pred.result, method= "spearman")$estimate #mean absolute error
  }
  result
}
pca.res = cross.validation(bugs ~ ., data = pca.lucene.data)
mean(pca.res)
cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = mean(abs(actual.result - pred.result)) #mean absolute error
  }
  result
}
pca.res = cross.validation(bugs ~ ., data = pca.lucene.data)
mean(pca.res)

# All change metrics --------------------------------

#JDT

pca.jdt = princomp(jdt[,18:32])
summary(pca.jdt)
pca.jdt.data = data.frame(bugs = jdt$bugs, pca.jdt$scores[,1])
cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = cor.test(actual.result, pred.result, method= "spearman")$estimate #mean absolute error
  }
  result
}
pca.res = cross.validation(bugs ~ ., data = pca.jdt.data)
mean(pca.res)
cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = mean(abs(actual.result - pred.result)) #mean absolute error
  }
  result
}
pca.res = cross.validation(bugs ~ ., data = pca.jdt.data)
mean(pca.res)

#PDE

pca.pde = princomp(pde[ ,18:32])
summary(pca.pde)
pca.pde.data = data.frame(bugs = pde$bugs, pca.pde$scores[,1:2])
cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = cor.test(actual.result, pred.result, method= "spearman")$estimate #mean absolute error
  }
  result
}
pca.res = cross.validation(bugs ~ ., data = pca.pde.data)
mean(pca.res)
cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = mean(abs(actual.result - pred.result)) #mean absolute error
  }
  result
}
pca.res = cross.validation(bugs ~ ., data = pca.pde.data)
mean(pca.res)

#MYLYN

pca.mylyn = princomp(mylyn[ ,18:32])
summary(pca.mylyn)
pca.mylyn.data = data.frame(bugs = mylyn$bugs, pca.mylyn$scores[,1])
cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = cor.test(actual.result, pred.result, method= "spearman")$estimate #mean absolute error
  }
  result
}
pca.res = cross.validation(bugs ~ ., data = pca.mylyn.data)
mean(pca.res)
cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = mean(abs(actual.result - pred.result)) #mean absolute error
  }
  result
}
pca.res = cross.validation(bugs ~ ., data = pca.mylyn.data)
mean(pca.res)

#EQUINOX

pca.equinox = princomp(equinox[ ,18:32])
summary(pca.equinox)
pca.equinox.data = data.frame(bugs = equinox$bugs, pca.equinox$scores[,1:2])
cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = cor.test(actual.result, pred.result, method= "spearman")$estimate #mean absolute error
  }
  result
}
pca.res = cross.validation(bugs ~ ., data = pca.equinox.data)
mean(pca.res)
cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = mean(abs(actual.result - pred.result)) #mean absolute error
  }
  result
}
pca.res = cross.validation(bugs ~ ., data = pca.equinox.data)
mean(pca.res)

# LUCENE
pca.lucene = princomp(lucene[ ,18:32])
summary(pca.lucene)
pca.lucene.data = data.frame(bugs = lucene$bugs, pca.lucene$scores[,1:2])
cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = cor.test(actual.result, pred.result, method= "spearman")$estimate #mean absolute error
  }
  result
}
pca.res = cross.validation(bugs ~ ., data = pca.lucene.data)
mean(pca.res)
cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = mean(abs(actual.result - pred.result)) #mean absolute error
  }
  result
} 
pca.res = cross.validation(bugs ~ ., data = pca.lucene.data)
mean(pca.res)

# All code metrics ------------------------------------

#JDT

pca.jdt = princomp(jdt[,1:17])
summary(pca.jdt)
pca.jdt.data = data.frame(bugs = jdt$bugs, pca.jdt$scores[,1])
cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = cor.test(actual.result, pred.result, method= "spearman")$estimate #mean absolute error
  }
  result
}
pca.res = cross.validation(bugs ~ ., data = pca.jdt.data)
mean(pca.res)
cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = mean(abs(actual.result - pred.result)) #mean absolute error
  }
  result
}
pca.res = cross.validation(bugs ~ ., data = pca.jdt.data)
mean(pca.res)

#PDE

pca.pde = princomp(pde[ ,1:17])
summary(pca.pde)
pca.pde.data = data.frame(bugs = pde$bugs, pca.pde$scores[,1:3])
cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = cor.test(actual.result, pred.result, method= "spearman")$estimate #mean absolute error
  }
  result
}
pca.res = cross.validation(bugs ~ ., data = pca.pde.data)
mean(pca.res)
cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = mean(abs(actual.result - pred.result)) #mean absolute error
  }
  result
}
pca.res = cross.validation(bugs ~ ., data = pca.pde.data)
mean(pca.res)

#MYLYN

pca.mylyn = princomp(mylyn[ ,1:17])
summary(pca.mylyn)
pca.mylyn.data = data.frame(bugs = mylyn$bugs, pca.mylyn$scores[,1:2])
cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = cor.test(actual.result, pred.result, method= "spearman")$estimate #mean absolute error
  }
  result
}
pca.res = cross.validation(bugs ~ ., data = pca.mylyn.data)
mean(pca.res)
cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = mean(abs(actual.result - pred.result)) #mean absolute error
  }
  result
}
pca.res = cross.validation(bugs ~ ., data = pca.mylyn.data)
mean(pca.res)

#EQUINOX

pca.equinox = princomp(equinox[ ,1:17])
summary(pca.equinox)
pca.equinox.data = data.frame(bugs = equinox$bugs, pca.equinox$scores[,1])
cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = cor.test(actual.result, pred.result, method= "spearman")$estimate #mean absolute error
  }
  result
}
pca.res = cross.validation(bugs ~ ., data = pca.equinox.data)
mean(pca.res)
cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = mean(abs(actual.result - pred.result)) #mean absolute error
  }
  result
}
pca.res = cross.validation(bugs ~ ., data = pca.equinox.data)
mean(pca.res)

# LUCENE
pca.lucene = princomp(lucene[ ,1:17])
summary(pca.lucene)
pca.lucene.data = data.frame(bugs = lucene$bugs, pca.lucene$scores[,1:2])
cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = cor.test(actual.result, pred.result, method= "spearman")$estimate #mean absolute error
  }
  result
}
pca.res = cross.validation(bugs ~ ., data = pca.lucene.data)
mean(pca.res)
cross.validation= function(form, dataset, iterations = 75, ratio = 0.10)
{
  result = rep(0,iterations)
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
    actual.result = test.data$bugs
    result[i] = mean(abs(actual.result - pred.result)) #mean absolute error
  }
  result
}
pca.res = cross.validation(bugs ~ ., data = pca.lucene.data)
mean(pca.res)