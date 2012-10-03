#Problem 1, Part A.
paramVect <- c(0.05,0.10,0.25);
simulate = function(paramVect, numTrials) {
  numOver20 = 0;
  for(i in 1:numTrials) {
    totalValue = sum(sapply(paramVect, function(coinValue) {
      if(rbinom(1,1,0.5) == 1) {
        return(coinValue);
      } else {
        return((-1)*coinValue);
      }
    }));
    #cat(sprintf("totalValue:%f\n",totalValue));
    if(totalValue >= 0.20) {
      numOver20 = numOver20 + 1;
    }
  }
  answer = (numOver20 / numTrials);
  #cat(sprintf("answer:%d,%d,%f\n", numOver20, numTrials, answer));
  return(answer);
}
simulate(paramVect, 100);
simulate(paramVect, 1000);
simulate(paramVect, 10000);

#Problem 1, Part B.


#Problem 2, Setup.
paramMatrix <- matrix(c(0.0,1.0,2.0,3.0,0.5, 0.25, 0.15, 0.10), nrow=4);

#Problem 2, Part A.
probability <- sum(apply(paramMatrix, 1, function(row_a) {
  return(sum(apply(paramMatrix, 1, function(row_b) {
    #cat(sprintf("%f,%f,%d\n", row_a[2], row_b[2], abs(row_b[1] - row_a[1])));
    if(abs(row_b[1] - row_a[1]) <= 1.25) {
      return(row_b[2] * row_a[2]);
    } else {
      return(0);
    }
  })));
}));
probability

#Problem 2, Part B.
ret3 <- apply(paramMatrix, 1, function(row_a) {
  ret2 <- apply(paramMatrix, 1, function(row_b) {
    cat(sprintf("P(A): %f | P(B): %f | T(A): %d, T(B): %d, abs(T(B)-T(A)): %d\n", row_a[2], row_b[2], row_a[1], row_b[1], abs(row_b[1] - row_a[1])));
    ret <- c();
    arrival_diff = row_b[1] - row_a[1];
    if(abs(arrival_diff) <= 1.25) {
      if(arrival_diff > 0 && arrival_diff <= 1.25) {
        ret[1]=1;
      } else {
        ret[1]=0;
      }
      ret[2]=1;
    } else {
      ret[1]=0;
      ret[2]=0;
    }
    return(ret);
  });
  #cat(print(ret2));
  #cat(sprintf("\n"));
  #return(ret2);
  return(c(sum(ret2[1,]),sum(ret2[2,])));
});
probability = (sum(ret3[1,]) / sum(ret3[2,]))
probability
