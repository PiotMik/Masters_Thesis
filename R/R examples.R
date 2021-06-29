# Main package
library(copula)
library(rvinecopulib)
# 4 plotting:
library(scatterplot3d)
library(ggplot2)
library(grid)
library(plotly)
set.seed(235)


# Define some copulas
normal <- normalCopula(param =0.7, dim = 2)
stc <- tCopula(param = 0.5, dim = 2, df = 2)
frank <- frankCopula(dim = 2, param =8)
gumbel <- gumbelCopula(dim =2, param = 5.6)
clayton <- claytonCopula(dim = 2, param = 19)

print(frank)

#The copula package provides a nice set of functions
# (mvdc, dMvdc, pMvdc and rMvdc) 
# for modelling multivariate distributions
# using a copula.


## mvdc( ) => Generate multivariate distribution:
cp <- claytonCopula(param= c(3.4), dim =2)
multivariate_dist <- mvdc(copula = cp,
                          margins= c("norm","t"),
                          paramMargins = list(list(mean=2,sd=3),
                                              list(df=2)) )
print(multivariate_dist)


## rCopula( ) => Generate random samples from copula ("correlated uniform", that you can transform with selected marginals)
## rMvdc( )   => Generate random variates from multivariate distribution (actual samples, because marginals are specified inside Mvdc object)

fr <- rCopula(2000, frank)
gu <- rCopula(2000, gumbel)
cl <- rCopula(2000, clayton)

p1 <- qplot(fr[,1], fr[,2], colour = fr[,1], main = "Frank copula random samples (theta = 8)", xlab = "u", ylab = "v")
p2 <- qplot(gu[,1], gu[,2], colour = gu[,1], main = "Gumbel copula random samples (theta = 5.6)", xlab = "u", ylab = "v")
p3 <- qplot(cl[,1], cl[,2], colour = cl[,1], main = "Clayton copula random samples (theta = 19)", xlab = "u", ylab = "v")
#( qplot== quick plot )

pushViewport(viewport(layout = grid.layout(1,3)))
print(p1, vp = viewport(layout.pos.row =1, layout.pos.col = 1))
print(p2, vp = viewport(layout.pos.row =1, layout.pos.col = 2))
print(p3, vp = viewport(layout.pos.row =1, layout.pos.col = 3))

samples <- rMvdc(2000,multivariate_dist)
scatterplot3d(samples[,1],samples[,2],color = "blue", pch=".")


## dCopula( ) => PDF
## pCopula( ) => CDF
coef_ <- 0.8
mycopula <- normalCopula(coef_ ,dim  =2)

# Using only copula structure, w/o marginals
u <- rCopula(2000, mycopula)
pdf_ <- dCopula(u, mycopula)
cdf_ <- pCopula(u, mycopula)

# Using Mvdc object (marginals specified)
v <- rMvdc(2000, multivariate_dist)
pdf_mvd <- dMvdc(v, multivariate_dist)
cdf_mvd <- pMvdc(v, multivariate_dist)


## USEFUL PLOTS:
# Density:
par(mfrow=c(1,3))
scatterplot3d(u[,1], u[,2], pdf_, color="red", main = "Density", xlab= "u1", ylab = "u2", zlab="dCopula" )
persp(mycopula, dCopula, main="Density")
contour(mycopula, dCopula, xlim = c(0,1), ylim =c(0,1), main ="Contour plot")

# CDF
par(mfrow=c(1,3))
scatterplot3d(u[,1], u[,2], cdf_, color="red", main = "CDF", xlab= "u1", ylab = "u2", zlab="pCopula" )
persp(mycopula, pCopula, main="CDF")
contour(mycopula, pCopula, xlim = c(0,1), ylim =c(0,1), main ="Contour plot")


# For a Mvdc object
par(mfrow=c(1,2))
scatterplot3d(v[,1], v[,2], pdf_mvd, color="red", main = "density", xlab= "u1", ylab = "u2", zlab="dCopula" )
scatterplot3d(v[,1], v[,2], cdf_mvd, color="red", main = "CDF", xlab= "u1", ylab = "u2", zlab="pCopula" )

persp(multivariate_dist, dMvdc, xlim=c(-4,8), ylim=c(-2,2), main="Density")
persp(multivariate_dist, pMvdc, xlim=c(-4,8), ylim=c(-2,2), main="CDF")

contour(multivariate_dist, dMvdc, xlim = c(-4,8), ylim =c(-4,3), main ="Contour plot")
contour(multivariate_dist, pMvdc, xlim = c(-4,8), ylim =c(-4,3), main ="Contour plot")




### SOME SPECIFIC EXAMPLES:
frank <- frankCopula(dim = 2, param = 3)
clayton <- claytonCopula(dim = 2, param = 1.2)
gumbel <- gumbelCopula(dim = 2, param = 1.5)

par(mfrow = c(1, 3))

# Density plot
persp(frank, dCopula, main ="Frank copula density")
persp(clayton, dCopula, main ="Clayton copula density")
persp(gumbel, dCopula, main ="Gumbel copula density")

# Contour plot of the densities
contour(frank, dCopula, xlim = c(0, 1), ylim=c(0, 1), main = "Contour plot Frank")
contour(clayton, dCopula, xlim = c(0, 1), ylim=c(0, 1), main = "Contour plot Clayton")
contour(gumbel, dCopula, xlim = c(0, 1), ylim=c(0, 1), main = "Contour plot Gumbel")