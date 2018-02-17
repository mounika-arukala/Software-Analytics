options(warn=-1)
cross.validation = function(form, dataset, iterations = 60, ratio = 0.35) {
  res = rep(0, iterations)
  size = nrow(dataset)
  test.size = ratio * size
  set.seed(02236773)
  for (i in 1:iterations) {
    test.idx = sample.int(size, test.size)
    test.data = dataset[test.idx, ]
    train.data = dataset[-test.idx, ]
    model = lm(form, data = train.data)
    predict.res = predict(model, test.data)
    actual.res = test.data$bug
    res[i] = cor.test(actual.res, predict.res, method = 'spearman')$estimate
  }
  res
}

equinox=read.csv("C:/Users/mouni/Desktop/topic/equinox/topic5log.csv")
jdt=read.csv("C:/Users/mouni/Desktop/topic/jdt/topic5log.csv")
lucene=read.csv("C:/Users/mouni/Desktop/topic/lucene/topic5log.csv")
mylyn=read.csv("C:/Users/mouni/Desktop/topic/mylyn/topic5log.csv")
pde=read.csv("C:/Users/mouni/Desktop/topic/pde/topic5log.csv")

#Equinox
equinox.loc=cross.validation(bug~loc,equinox)
equinox.topics=cross.validation(bug~V1+V2+V3+V4+V5,equinox)

mean(equinox.loc)
mean(equinox.topics)

#JDT
jdt.loc=cross.validation(bug~loc,jdt)
jdt.topics=cross.validation(bug~V1+V2+V3+V4+V5,jdt)

mean(jdt.loc)
mean(jdt.topics)

#Mylyn
mylyn.loc=cross.validation(bug~loc,mylyn)
mylyn.topics=cross.validation(bug~V1+V2+V3+V4+V5,mylyn)

mean(mylyn.loc)
mean(mylyn.topics)

#Lucene
lucene.loc=cross.validation(bug~loc,lucene)
lucene.topics=cross.validation(bug~V1+V2+V3+V4+V5,lucene)

mean(lucene.loc)
mean(lucene.topics)

#PDE
pde.loc=cross.validation(bug~loc,pde)
pde.topics=cross.validation(bug~V1+V2+V3+V4+V5,pde)

mean(pde.loc)
mean(pde.topics)

#ALL

all.loc=c(equinox.loc,jdt.loc,mylyn.loc,lucene.loc,pde.loc)
mean(all.loc)
all.topics=c(equinox.topics,jdt.topics,mylyn.topics,lucene.topics,pde.topics)
mean(all.topics)

equinox=read.csv("C:/Users/mouni/Desktop/topic/equinox/topic10log.csv")
jdt=read.csv("C:/Users/mouni/Desktop/topic/jdt/topic10log.csv")
lucene=read.csv("C:/Users/mouni/Desktop/topic/lucene/topic10log.csv")
mylyn=read.csv("C:/Users/mouni/Desktop/topic/mylyn/topic10log.csv")
pde=read.csv("C:/Users/mouni/Desktop/topic/pde/topic10log.csv")

#Equinox
equinox.loc=cross.validation(bug~loc,equinox)
equinox.topics=cross.validation(bug~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10,equinox)

mean(equinox.loc)
mean(equinox.topics)

#JDT
jdt.loc=cross.validation(bug~loc,jdt)
jdt.topics=cross.validation(bug~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10,jdt)

mean(jdt.loc)
mean(jdt.topics)

#Mylyn
mylyn.loc=cross.validation(bug~loc,mylyn)
mylyn.topics=cross.validation(bug~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10,mylyn)

mean(mylyn.loc)
mean(mylyn.topics)

#Lucene
lucene.loc=cross.validation(bug~loc,lucene)
lucene.topics=cross.validation(bug~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10,lucene)

mean(lucene.loc)
mean(lucene.topics)

#PDE
pde.loc=cross.validation(bug~loc,pde)
pde.topics=cross.validation(bug~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10,pde)

mean(pde.loc)
mean(pde.topics)

#all

all.loc=c(equinox.loc,jdt.loc,mylyn.loc,lucene.loc,pde.loc)
mean(all.loc)
all.topics=c(equinox.topics,jdt.topics,mylyn.topics,lucene.topics,pde.topics)
mean(all.topics)