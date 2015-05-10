library("MASS")

library(foreach)
library(doSNOW)

registerDoSNOW(cl)

cores <- 2
cl <- makeCluster(cores, type = "SOCK")
registerDoSNOW(cl)




distance.fun <- function(x1, x2) {
    sqrt(sum((x1 - x2)^2))
}



system.time({
    result.cl.1 <- foreach(i = 1:(n - 1), .combine = c) %:% foreach(j = (i + 
        1):n, .combine = c) %dopar% {
        distance.fun(x[i, ], x[j, ])
    }
})

stopCluster(cl)