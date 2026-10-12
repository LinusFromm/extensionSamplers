#### Setup ####
A = extensionSamplers::highway4$A

x = extensionSamplers::highway4$x
y = extensionSamplers::highway4$y
  
nSample = 1e+05
nBurnin = 2e+04

#### 4ti2 Basis ####

FMB = extensionSamplers::highway4$FMB

#### Lexicographic Basis ####

A2Idx = extensionSamplers::highway4$A2IdxLB
LB = extensionSamplers::highway4$LB

#### Uniform Samplers ####

set.seed(2027)
samplesUniformFMB <- extensionSamplers::sampler(A = A,
                                          y = y,
                                          B = FMB,
                                          Model = "uniform",
                                          extension = "none",
                                          nChains = 4,
                                          nSample = nSample,
                                          nBurnin = nBurnin,
                                          includeBurnin = FALSE)

write.csv(samplesUniformFMB, "4-highway_uniform_FMB.csv", row.names = FALSE)

set.seed(2027)
samplesUniform1MB <- extensionSamplers::sampler(A = A,
                                          y = y,
                                          B = LB,
                                          Model = "uniform",
                                          extension = "p",
                                          p = ifelse(1:10 %in% A2Idx, 0, 1),
                                          loggamma = 0,
                                          w = 0,
                                          nChains = 4,
                                          nSample = nSample,
                                          nBurnin = nBurnin,
                                          includeBurnin = FALSE)

write.csv(samplesUniform1MB, "4-highway_uniform_1MB.csv", row.names = FALSE)

positiveProportion1MB <- sum(complete.cases(extensionSamplers::onlyPositive(samplesUniform1MB)))/(4*nSample)

set.seed(2027)
samplesUniformHR <- extensionSamplers::sampler(A = A,
                                               y = y,
                                               Model = "uniform",
                                               extension = "hyperrectangle",
                                               A2Idx = A2Idx,
                                               loggamma = 0,
                                               w = 0.7,
                                               nChains = 4,
                                               nBurnin = nBurnin,
                                               nSample = nSample,
                                               includeBurnin = FALSE)

write.csv(samplesUniformHR, "4-highway_uniform_HR.csv", row.names = FALSE)
positiveProportionHR <- sum(complete.cases(extensionSamplers::onlyPositive(samplesUniformHR)))/(4*nSample)
