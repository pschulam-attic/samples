library(MASS)
library(mclust)
library(ggplot2)

generate.data <- function(n, k, prior.mean, prior.var)
{
    p <- length(prior.mean)

    locs <- mvrnorm(k, mu=prior.mean, Sigma=diag(prior.var, p))
    vars <- rgamma(k, shape=1, scale=0.25)

    obs <- matrix(0, nrow=n, ncol=p)
    z <- numeric(n)

    for (i in 1:n)
    {
        z[i] <- sample(1:k, 1)
        obs[i,] <- mvrnorm(1, mu=locs[z[i],], Sigma=diag(vars[z[i]], p))
    }
    list(locs=locs, vars=vars, z=z, obs=obs)
}

plot.mixture <- function(locs, z, obs)
{
    stopifnot(dim(obs)[2]==2)
    z <- as.factor(z)
    df1 <- data.frame(x=obs[,1], y=obs[,2], z=z)
    df2 <- data.frame(x=locs[,1], y=locs[,2])
    p <- ggplot()
    p <- p + geom_point(data=df1, aes(x=x, y=y, colour=z), shape=16, size=2, alpha=0.75)
    p <- p + geom_point(data=df2, aes(x=x, y=y), shape=16, size=3)
    p <- p + opts(legend.position="none")
    p
}
