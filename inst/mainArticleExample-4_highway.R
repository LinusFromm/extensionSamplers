#### Function for Testing whether dominating (Theorem 2 Hazelton + Karimi 2024) ####

isDominating <- function(B) {
  for (k in seq_len(min(dim(B)))) {
    rowSets <- combn(nrow(B), k)

    for (i in seq_len(ncol(rowSets))) {
      S <- B[rowSets[, i], , drop = FALSE]
      mixed <- colSums(S > 0) > 0 & colSums(S < 0) > 0

      if (sum(mixed) >= k) return(FALSE)
    }
  }
  TRUE
}

testCplb <- function(A, tol = 1e-9) {
  A <- as.matrix(A)
  stopifnot(all(is.finite(A)), all(abs(A - round(A)) < tol))
  A <- round(A)

  # Select independent rows
  rowQr <- qr(t(A), tol = tol, LAPACK = FALSE)
  r <- rowQr$rank
  n <- ncol(A)
  stopifnot(r > 0L, r < n)

  A <- A[rowQr$pivot[seq_len(r)], , drop = FALSE]
  partitions <- combn(n, r)

  dominating <- logical(ncol(partitions))
  partition <- character(ncol(partitions))
  keep <- logical(ncol(partitions))
  nonInteger <- 0L

  for (i in seq_len(ncol(partitions))) {
    J <- partitions[, i]
    K <- setdiff(seq_len(n), J)
    AJ <- A[, J, drop = FALSE]

    if (qr(AJ, tol = tol)$rank < r) {
      if(!(i %% 1000)){
        cat("\r", i, "/", ncol(partitions))
      }
      next
    }

    head <- -solve(AJ, A[, K, drop = FALSE])

    if (any(abs(head - round(head)) > tol)) {
      nonInteger <- nonInteger + 1L

      if(!(i %% 1000)){
        cat("\r", i, "/", ncol(partitions))
      }
      next
    }

    # Row permutations do not affect domination
    dominating[i] <- isDominating(round(head))
    partition[i] <- paste(
      paste(J, collapse = ","),
      paste(K, collapse = ","),
      sep = " | "
    )
    keep[i] <- TRUE

    if(!(i %% 1000)){
      cat("\r", i, "/", ncol(partitions))
    }
  }

  if (nonInteger > 0L)
    warning(nonInteger, " partitions excluded: noninteger CPLB head.")

  list(dominating = dominating[keep], partition = partition[keep])
}

#### Setup ####
A = extensionSamplers::highway4$A

nSample = 1e+05
nBurnin = 2e+04

M = extensionSamplers::highway4$FMB

## There are only 4 partitions that form MB 1,5,8,9; 1,5,8,10; 2,5,8,9 and 2,5,8,10 all produce bad convergence

# A6 trunk road in Leicester, UK
lambda = extensionSamplers::highway4$lambda
x = extensionSamplers::highway4$x
y = A%*%x

xStart <- lpSolve::lp(direction = "max",
                      objective.in = rep(1, 10),
                      const.dir = "==",
                      const.mat = A,
                      const.rhs = y,
                      all.int = TRUE)$solution

A2Idx = extensionSamplers::highway4$A2IdxLB
LB = extensionSamplers::highway4$LB

#### Fibre Sampler MB ####

set.seed(2027)
MB_samples_burnin <- extensionSamplers::sampler(A = A,
                                                y = y,
                                                B = M,
                                                Model = "poisson",
                                                lambda = lambda,
                                                xStart = xStart,
                                                nSample = nSample,
                                                nBurnin = nBurnin,
                                                nChains = 4,
                                                includeBurnin = TRUE)

MB_samples <- MB_samples_burnin[c((nBurnin+1):(nBurnin+nSample),
                                  (2*nBurnin+1+nSample):(2*nBurnin+2*nSample),
                                  (3*nBurnin+1+2*nSample):(3*nBurnin+3*nSample),
                                  (4*nBurnin+1+3*nSample):(4*nBurnin+4*nSample)),]
MB_samples[,12] <- rep(1:nSample, 4)
MB_samples[,13] <- 1:(4*nSample)

write.csv(MB_samples_burnin, "MB_samples.csv", row.names = FALSE)

#### 1 MB Sampler ####

set.seed(2027)
LB_samples_burnin <- extensionSamplers::sampler(A = A,
                                                y = y,
                                                B = LB,
                                                Model = "poisson",
                                                lambda = lambda,
                                                extension = "p",
                                                p = ifelse(1:10 %in% A2Idx, 0, 1),
                                                loggamma = 0,
                                                w = 2,
                                                xStart = xStart,
                                                nSample = nSample,
                                                nBurnin = nBurnin,
                                                nChains = 4,
                                                includeBurnin = TRUE)

LB_samples <- LB_samples_burnin[c((nBurnin+1):(nBurnin+nSample),
                                  (2*nBurnin+1+nSample):(2*nBurnin+2*nSample),
                                  (3*nBurnin+1+2*nSample):(3*nBurnin+3*nSample),
                                  (4*nBurnin+1+3*nSample):(4*nBurnin+4*nSample)),]
LB_samples[,12] <- rep(1:nSample, 4)
LB_samples[,13] <- 1:(4*nSample)

LB_samples_pos <- extensionSamplers::onlyPositive(LB_samples)

positive_portion_LB <- sum(complete.cases(LB_samples_pos))/(4*nSample)
write.csv(LB_samples_burnin, "LB_samples.csv", row.names = FALSE)


#### Hyperrectangle sampler ####

set.seed(2027)
hyperrectangle_LB_burnin <- extensionSamplers::sampler(A,
                                                       y,
                                                       lambda = lambda,
                                                       A2Idx = A2Idx,
                                                       Model = "poisson",
                                                       extension = "hyperrectangle",
                                                       w = 2,
                                                       nSample = nSample,
                                                       nBurnin = nBurnin,
                                                       nChains = 4,
                                                       includeBurnin = TRUE)

hyperrectangle_LB <- hyperrectangle_LB_burnin[c((nBurnin+1):(nBurnin+nSample),
                                                (2*nBurnin+1+nSample):(2*nBurnin+2*nSample),
                                                (3*nBurnin+1+2*nSample):(3*nBurnin+3*nSample),
                                                (4*nBurnin+1+3*nSample):(4*nBurnin+4*nSample)),]

hyperrectangle_LB[,12] <- rep(1:nSample, 4)
hyperrectangle_LB[,13] <- 1:(4*nSample)

hyperrectangle_LB_pos <- extensionSamplers::onlyPositive(hyperrectangle_LB)

(positive_portion_hyperrectangle_LB <- sum(complete.cases(hyperrectangle_LB_pos))/(4*nSample))
write.csv(hyperrectangle_LB_burnin, "hyperrectangle_LB.csv", row.names = FALSE)

#### Results ####

MB_samples_draws <- extensionSamplers::to_draws_df(MB_samples)
LB_samples_draws <- extensionSamplers::to_draws_df(LB_samples)
hyperrectangle_LB_draws <- extensionSamplers::to_draws_df(hyperrectangle_LB)

ess_MB <- posterior::summarise_draws(MB_samples_draws, "ess_bulk")$ess_bulk
ess_LB <- posterior::summarise_draws(LB_samples_draws, "ess_bulk")$ess_bulk
ess_HR_LB <- posterior::summarise_draws(hyperrectangle_LB_draws, "ess_bulk")$ess_bulk

ess_LB_pos <- positive_portion_LB*ess_LB
ess_HR_LB_pos <- positive_portion_hyperrectangle_LB*ess_HR_LB

pdf("fig6-1.pdf", width = 6, height = 5)
barplot(log(rbind(ess_MB, ess_LB_pos, ess_HR_LB_pos))/log(10),
        beside = TRUE,
        legend.text = c("FMB", "LB", "HR LB"),
        args.legend = list(bg = "NA", x = "topright", horiz = TRUE, cex = 0.8),
        ylab = expression(log[10](ESS)),
        ylim = c(0, 5),
        xlab = "Variable",
        names.arg = parse(text = paste0("x[", 1:10, "]")))
box()
dev.off()

nBurnin = 2e+04
nExtra = 1e+04

i = 9

pdf("fig6-2.pdf", width = 6, height = 5)
xRange = range(MB_samples_burnin[c(1:(nBurnin+nExtra),
                                   (nBurnin+nSample) + 1:(nBurnin+nExtra),
                                   2*(nBurnin+nSample) + 1:(nBurnin+nExtra),
                                   3*(nBurnin+nSample) + 1:(nBurnin+nExtra)),i])

extensionSamplers::traceplot(MB_samples_burnin[c(1:(nBurnin+nExtra),
                                                 (nBurnin+nSample) + 1:(nBurnin+nExtra),
                                                 2*(nBurnin+nSample) + 1:(nBurnin+nExtra),
                                                 3*(nBurnin+nSample) + 1:(nBurnin+nExtra)),], i, FALSE, ylim = xRange)
rect(0, -100000, nBurnin, 100000, col = "#00000010", border = "#00000000")
dev.off()


pdf("fig6-3.pdf", width = 6, height = 5)
xRange = range(LB_samples_burnin[c(1:(nBurnin+nExtra),
                                    (nBurnin+nSample) + 1:(nBurnin+nExtra),
                                    2*(nBurnin+nSample) + 1:(nBurnin+nExtra),
                                    3*(nBurnin+nSample) + 1:(nBurnin+nExtra)),i])

extensionSamplers::traceplot(LB_samples_burnin[c(1:(nBurnin+nExtra),
                                                 (nBurnin+nSample) + 1:(nBurnin+nExtra),
                                                 2*(nBurnin+nSample) + 1:(nBurnin+nExtra),
                                                 3*(nBurnin+nSample) + 1:(nBurnin+nExtra)),], i, TRUE, 0.2, ylim = xRange)
rect(0, -100000, nBurnin, 100000, col = "#00000010", border = "#00000000")
dev.off()

pdf("fig6-4.pdf", width = 6, height = 5)
xRange = range(hyperrectangle_LB_burnin[c(1:(nBurnin+nExtra),
                                          (nBurnin+nSample) + 1:(nBurnin+nExtra),
                                          2*(nBurnin+nSample) + 1:(nBurnin+nExtra),
                                          3*(nBurnin+nSample) + 1:(nBurnin+nExtra)),i])

extensionSamplers::traceplot(hyperrectangle_LB_burnin[c(1:(nBurnin+nExtra),
                                                        (nBurnin+nSample) + 1:(nBurnin+nExtra),
                                                        2*(nBurnin+nSample) + 1:(nBurnin+nExtra),
                                                        3*(nBurnin+nSample) + 1:(nBurnin+nExtra)),], i, TRUE, 0.2, ylim = xRange)

rect(0, -100000, nBurnin, 100000, col = "#00000010", border = "#00000000")
dev.off()
