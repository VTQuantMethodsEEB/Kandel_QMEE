#4
ppois(12, lambda = 10, lower.tail = F)

#3
qnbinom(p= .85, size=1, mu=300)

#2
set.seed(100)
ns<- rbinom(n = 1000, size =1, prob = 0.98)/100
ne <- rbinom(n=ns, size=1, prob = 0.01)
sum(ne)

#1
pupstot = rpois(500, lambda = 4)
livebirths = rbinom(500, size = pupstot, p=.8)
sum(livebirths)







