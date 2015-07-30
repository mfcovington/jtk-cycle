
batch <- 128  
records <- 8*batch
replicates <- 2                               # independent replicates per timepoint
timepoints <- 24                              # maintain 48 datapoints per record
datapoints <- timepoints*replicates
cos.period.min <- 10                          # corresponds to previous 20
cos.period.max <- 15                          # corresponds to previous 30
cos.amplitude.max <- 6                        # these are easily seen and detected
cos.amplitude.min <- 1                        # are these reliably detected using replicates??
outlier.amplitude <- 20  

set.seed(99) 

noise <- rnorm(records*datapoints)
dim(noise) <- c(records,datapoints)

outliers.pos <- rep(c(outlier.amplitude,rep(0,datapoints-1)),2*batch)
outliers.neg <- rep(c(-outlier.amplitude,rep(0,datapoints-1)),2*batch)
outliers.non <- rep(0,4*batch*datapoints)
outliers <- c(outliers.pos,outliers.neg,outliers.non)
dim(outliers) <- c(datapoints,records)
outliers <- t(apply(outliers,2,sample))       # randomize outlier timing
outliers <- outliers[sample(1:records),]      # randomize outlier type (pos, neg, non)

cos.amplitudes <- c(runif(4*batch,cos.amplitude.min,cos.amplitude.max),rep(0,4*batch))
cos.periods <- c(runif(4*batch,cos.period.min,cos.period.max),rep(1,4*batch))
cos.lag.factors <- c(runif(4*batch),rep(0,4*batch))
cos.parameters <- as.matrix(cbind(cos.amplitudes,cos.periods,
                                  cos.lag.factors*cos.periods,
                                  rowSums(outliers)
))

signals <- t(apply(cos.parameters,1,function(par) {
                   rep(par[1]*cos((1:timepoints -1 -par[3])*2*pi/par[2]),ea=replicates)
}))

shuffle <- sample(1:records)
signals <- signals[shuffle,]
cos.parameters <- cos.parameters[shuffle,]

test.data5 <- signals +noise +outliers

