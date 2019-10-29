# QM1-Session-3-

#1a    (x, x), (x, sqrt(x)), (x, xˆ2), (x, xˆ3) and (x, exp(x) - 1)
x <- seq(.01, 2, .01)
plot(x,x, type = "l", col=2, ylim = c(0,8), lwd=c(2), main= "Random Graphs")
lines(x, sqrt(x), type = "l",lwd=c(2), col=3)
lines(x, x^2, type = "l",lwd=c(2), col=4)
lines(x, x^3, type = "l",lwd=c(2), col=5)
lines(x, (exp(x) - 1),lwd=c(2), type = "l", col=6)
for (n in seq(.5,1.5, .5)) {
  abline(v= n, lty = c(2))
}
legend(   
  "top", 
  title = "Graphs",
  horiz = TRUE, 
  c("(x, x)", "(x, sqrt(x))", "(x, x^2)", "(x, x^3)","(x, exp(x) - 1)"),
  col= c(2,3,4,5,6),
  lwd=c(3),
  #bty = "n"
  cex = .55
)

#1b    (x, log(x)), (x, log(sqrt(x))), (x, log(x^2)), (x, log(x^3))
plot(x, log(x), type = "l", col=2, ylim = c(-10,8), lwd=c(2), main= "Random Graphs")
lines(x, log(sqrt(x)), type = "l",lwd=c(2), col=3)
lines(x, log(x^2), type = "l",lwd=c(2), col=4)
lines(x, log(x^3), type = "l",lwd=c(2), col=5)
lines(x, (exp(x) - 1),lwd=c(2), type = "l", col=6)
for (n in seq(.5,1.5, .5)) {
  abline(v= n, lty = c(2))
}
legend(   
  "top", 
  title = "Graphs",
  horiz = TRUE, 
  c("(x, log(x))", "(x, log(sqrt(x)))", "(x, log(x^2))", "(x, log(x^3))"),
  col= c(2,3,4,5,6),
  lwd=c(3),
  #bty = "n"
  cex = .55
)

#2
  relDiff = function(x1 = 100 ,x2,x3){
    for (n in 1:3) {
    }
    c((x2-x1)/x1,(x3-x2)/x2)
   
}
#b
relDiff(80, 50, 100)
relDiff(x3 = 10, x2 = 40)
#relDiff(70, 90)

#c
relDiff = function(x){
  n = length(x)
  print(n)
  w = c()
  for (k in 1:(n-1)) {
    w[k] = ((x[k+1]-x[k])/x[k])
  }
  w
}

relDiff(c(1,2,3,4,5,6,7))

#d

x <- 1:100
diff(x)
relDiff(x)
diff(log(x))
relDiff(exp(x))

#3
powerFun <- function(x, r= 1, A= 1) {
  if (x>0){
    A*(x^r)
  }
}
powerFun(x=1)
x= seq(.5,2,.01)
r = c(-2,-1,-0.5, 0.5,1,2)

#b
plot(x,powerFun(x,r, A=1), type = "p", col=2, ylim = c(0,4), lwd=c(2), main= "yeah")

legend(   
  "top", 
  title = "Graphs",
  horiz = TRUE, 
  c("powerFun(x,r, A=1)"),
  col= c(2),
  lwd=c(3),
  #bty = "n"
  #cex = .55
)

#c
plot(log(x),log(powerFun(x,r, A=1)), type = "p", col=2, ylim = c(-2,4), lwd=c(2), main= "yeah 2")

legend(   
  "top", 
  title = "Graphs",
  horiz = TRUE, 
  c("log(x),log(powerFun(x,r, A=1)"),
  col= c(2),
  lwd=c(3),
  #bty = "n"
  #cex = .55
)

#4
polyEval = function(a,b){
  n = length(b)
  r= b[1]
  for (k in 2:n) {
    r = b[k]*a^(k-1) + r
  }
  r
}
polyEval(2, c(2,3,1))
polyEval(1, c(0,1,0,1,0,1))

#5
f = function(x) exp(x)
f1 = function(x) f(-x)
f2 = function(x) f(x+2)
f3 = function(x) 1.5*f(x)
f4 = function(x) -f(x)
f5 = function(x) f(x) - 1

x <- seq(-6,8,.5)
plot(x = c(), y = c(), type = "l", xlab = "x", ylab = "y", ylim = c(-8,13), xlim = c(-6,8), main= "Random Graphs")
lines(x, f(x), type = "l",lwd=c(2), col=2)
lines(x, f1(x), type = "l",lwd=c(2), col=3)
lines(x, f2(x), type = "l",lwd=c(2), col=4)
lines(x, f3(x), type = "l",lwd=c(2), col=5)
lines(x, f4(x),lwd=c(2), type = "l", col=6)
lines(x, f5(x),lwd=c(2), type = "l", col=7)

names = c("y = exp(x)", "y = exp(-x)", "y = exp(x+2)", "y = 1.5*exp(x)","y = -exp(x)" ,"y = exp(x)-1")

legend( "topleft", legend = names, title = "Graphs", lty = 1, lwd = 2, col = c(2,3,4,5,6,7), ncol = length(names)/2, cex = 0.75 )

#6
x = (-10:10)
y = (-10:10)
plot(x , exp(x) + exp(y), type = "l", xlab = "x",col = 2, ylab = "y", ylim = c(-8,13), xlim = c(-6,8), main= "Random Graphs")
lines(x, ((exp(x) + exp(y))^.5)*2, type = "l")

#c
x= seq(-.5,4, .001)
c7= function(x) {
  exp(-(x^2)) + x/5 -0.7
}

 x[c7(x) == 0.000]
 plot(x, c7(x), type = "l")
 abline(h=0)

#7 f (x) = exp(−(x − 2)^2

f= function(x) exp(-(x - 2)^2)
plot(x , f(x), type = "l", xlab = "x",col = 2, ylab = "y", ylim = c(0,1.2), xlim = c(-2,5), main= "Random Graphs")
abline(v=2)
abline(h=0)
abline(h=1)

#7b,c 8 

#8
#I in 1000 eur
T1 = function(income){
  ifelse(income > 30,
        0.2*30+0.4*(income - 30),
  income*0.2)
}

T2 = function(income){
  ifelse(income > 20,
         ifelse(income > 30,
                0.15*20 + 0.3*(30-20) + 0.6*(income-30),
                0.15*20 + 0.3*(income-20)),
  0.15*income)
}

I = 0:50
I[T1(I) == T2(I)]

plot(I, T1(I),type = "l", col = 2, lwd= 2, xlab = "I, income in EUR 1000", ylab = "Tax", main = "T1(I) & T2(I)")
lines(I, T2(I), col = 3, lwd= 2)


T1(2000)
T2(2000)
"T1 favors the people with high incomes"
T1(20)
T2(20)
"T2 favors the people with lower incomes"

P1.1 = function(income) T1(income - 2)
P1.2 = function(income) T2(income - 2)
P2.1 = function(income) T1(income) - 2
P2.2 = function(income) T2(income) - 2

A1.1 = function(income) T1(0,95*income)
A1.2 = function(income) T2(0,95*income)
A2.1 = function(income) 0.9*T1(income)
A2.2 = function(income) 0.9*T2(income)

lines(I, P1.1(I), col = 2, lty = 5, lwd= 3)
lines(I, P1.2(I), col = 3, lty = 5, lwd= 4)
lines(I, P2.1(I), col = 2, lty = 3, lwd= 3)
lines(I, P2.2(I), col = 3, lty = 3, lwd= 4)

lines(I, A1.1(I), col = 2, lty = 6, lwd= 5)
lines(I, A1.2(I), col = 3, lty = 6, lwd= 6)
lines(I, A2.1(I), col = 2, lty = 2, lwd= 5)
lines(I, A2.2(I), col = 3, lty = 2, lwd= 6)(d)

names = c("System 1", "Idea 1(d)","Idea 2(d)", "Idea 3(e)", "Idea 4(e)", "System 2","Idea 1(d)","Idea 2(d)", "Idea 3(e)", "Idea 4(e)")
legend( x = 0, y = 14,legend = names, title = "Taxational systems", lty = c(1,3,5,6,2,1,3,5,6,2),ncol = length(names)/5, lwd = c(2,3,4,5,6, 2,3,4,5,6), col = c(2,2,2,2,2,3,3,3,3,3), 
        cex = .9, bty = "n")

