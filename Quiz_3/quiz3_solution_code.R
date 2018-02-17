t1 = scan('D:/Spring 2017/Software Analytics/quiz3.txt')

plot(t1/sum(t1), type = 'l', color = 'b')
lines(t1/sum(t1))


mt1 = nls(bug ~ vol * dweibull(time, shape = 2, scale = lamda), data = data.frame(time = 1:length(t1), bug = t1) )
mt1

plot(dweibull(1:25,shape = 2, scale = 4), color = 'red') 
lines(t1/sum(t1))

v1 = t1[1]/dweibull(1, shape = 2, scale = 4)
v1
n1  =sum(t1)
abs(n1-v1)/n1

v1 = sum(t1[1:2])/sum(dweibull(1:2, shape = 2, scale = 4))
v1
n1  =sum(t1)
abs(n1-v1)/n1

v1=1416.091 #after 2nd quarter
x=v1*dweibull(3, shape = 2, scale = 4)
p=x-t1[3]
p/x

x=v1*dweibull(4, shape = 2, scale = 4)
p=x-t1[4]
p/x
