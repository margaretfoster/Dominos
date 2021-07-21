



## Small world group

source("initSW.R") 
test1 <- initSWGroup(
    r.seed <- 1235,
    num.group <- 15,
    init.ideo.high <- 1,
    init.ideo.low <- .7,
    nei.init=5,
    rw.prob=.01)


test1

### Preferential Attachment

source("initPA.R")

test <- initPAGroup(r.seed=1253,
                    num.group=10,
                    init.ideo.high=1,
                    init.ideo.low=.6)

## ER-Recruits
## ER- Group

source("initBothER.R")

r.test <- initER(r.seed=332,
                 num.nodes=10,
                 init.ideo.high=.8,
                 init.ideo.low=.3,
                 lower.bound.thresh=.6,
                 upper.bound.thresh=.8)

r.test

g.test <- initER(r.seed=112280,
                 num.nodes=15,
                 init.ideo.high=1,
                 init.ideo.low=.6,
                 lower.bound.thresh=.6,
                 upper.bound.thresh=.8)

g.test
