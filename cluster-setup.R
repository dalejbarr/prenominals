## replace with your own hostnames you have setup with ssh, e.g.:
## hosts <- rep(c("localhost", "host1", "host2", "host3"),
##              c(6, 8, 8, 8))
hosts <- rep("localhost", parallel::detectCores() - 2L)
