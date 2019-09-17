install.packages("PerformanceAnalytics")
install.packages("readr")
library(PerformanceAnalytics)
library(readr)
library(xts)

prices <- read.csv("O:/Administration/Employee Folders/Jason's Folder/R Data/OPCM.csv")
date <- as.Date(prices$Date, format = "%m/%d/%Y")
prices$Date <- date
prices.xts <- xts(prices[,-1], order.by = prices[,1])

#calculate returns
returns.xts <- Return.calculate(prices.xts, method = c("log"))
returns.xts <- returns.xts[-1,]

#sub set years to look at
returns2yr <- returns.xts['2016-11::']
returns4yr <- returns.xts['2014-11::']
returns5yr <- returns.xts['2013-11::']

#find missing data
M <- sapply(returns2yr, function(x) sum(is.na(x))); M[M>0]
M <- sapply(returns4yr, function(x) sum(is.na(x))); M[M>0]
M <- sapply(returns5yr, function(x) sum(is.na(x))); M[M>0]
M <- sapply(returns.xts, function(x) sum(is.na(x))); M[M>0]

#remove tickers with missing data
returns5yr <- subset(returns5yr, select = -c(jo, baba, syf, dnow))
returns2yr <- subset(returns2yr, select = -c(jo))



#clustering code from https://rviews.rstudio.com/2017/08/22/stocks/
log_returns <- returns2yr

X = cor(log_returns)

L = eigen(X, symmetric=TRUE)

plot(L$values, ylab="eigenvalues")
abline(v=9)
N = 10 # (use 1st 10 eigenvectors, set N larger to reduce regularization)
P = L$vectors[, 1:N] %*% ((1 / L$values[1:N]) * t(L$vectors[, 1:N]))
P = P / tcrossprod(sqrt(diag(P)))

install.packages("igraph")
install.packages("shiny")
install.packages("threejs")
install.packages("threejs", repos="http://cran.rstudio.com/", dependencies=TRUE)
install.packages("httpuv")
library(igraph)
library(threejs)
library(shiny)

threshold = 0.925
Q = P * (P > quantile(P, probs=threshold))                           # thresholded precision matrix
g = graph.adjacency(Q, mode="undirected", weighted=TRUE, diag=FALSE) # ...expressed as a graph

# The rest of the code lumps any singletons lacking edges into a single 'unassociated' group shown in gray
# (also assigning distinct colors to the other groups).
x = groups(cluster_louvain(g))
i = unlist(lapply(x, length))
d = order(i, decreasing=TRUE)
x = x[d]
i = i[d]
j = i > 1
s = sum(j)
names(x)[j] = seq(1, s)
names(x)[! j] = s + 1
grp = as.integer(rep(names(x), i))
clrs = c(rainbow(s), "gray")[grp[order(unlist(x))]]
g = set_vertex_attr(g, "color", value=clrs)

graphjs(g, vertex.size=0.2, vertex.shape=colnames(X), edge.alpha=0.5)
