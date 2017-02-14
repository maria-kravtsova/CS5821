library(boot)
attach(Default)
set.seed(1)
glm.fit = glm(default ~ income + balance, data=Default, family="binomial")
summary(glm.fit)

boot.fn=function(data,index){
  return (coef(glm(default ~ income + balance, data=data, subset=index, family="binomial")))
}
boot(Default,boot.fn,1000)