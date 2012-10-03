#Problem 1, Setup.
paramVect <- c(0.05,0.10,0.25);

#Problem 1, Part A.
simulate1A = function(paramVect, numTrials) {
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
simulate1A(paramVect, 100);
simulate1A(paramVect, 1000);
simulate1A(paramVect, 10000);

#Problem 1, Part B.
simulate1B = function(paramVect, numTrials) {
  numOver20 = 0;
  numOver20AndNickel = 0;
  for(i in 1:numTrials) {
    totalValue = 0;
    nickelAdded = 0;
    for(coinValue in c(0.25,0.10)) {
      if(rbinom(1,1,0.5) == 1) {
        totalValue = totalValue + coinValue;
      } else {
        totalValue = totalValue - coinValue;
      }
    }
    
    coinValue = 0.05;
    nickel = 0;
    if(rbinom(1,1,0.5) == 1) {
      nickel = 1;
      totalValue = totalValue + coinValue;
    } else {
      totalValue = totalValue - coinValue;
    }
    
    if(totalValue >= 0.20) {
      numOver20 = numOver20 + 1;
      if(nickel == 1) {
        numOver20AndNickel = numOver20AndNickel + 1;
      }
    }
  }
  answer = (numOver20AndNickel / numOver20);
  #cat(sprintf("answer:%d,%d,%f\n", numOver20, numTrials, answer));
  return(answer);
}
simulate1B(paramVect, 100);
simulate1B(paramVect, 1000);
simulate1B(paramVect, 10000);
simulate1B(paramVect, 100000);

#Problem 2, Setup.
paramMatrix <- matrix(c(0.0,1.0,2.0,3.0,0.5, 0.25, 0.15, 0.10), nrow=4);

#Problem 2, Part A.
p_of_c <- sum(apply(paramMatrix, 1, function(row_a) {
  return(sum(apply(paramMatrix, 1, function(row_b) {
    #cat(sprintf("%f,%f,%d\n", row_a[2], row_b[2], abs(row_b[1] - row_a[1])));
    if(abs(row_b[1] - row_a[1]) <= 1.25) {
      return(row_b[2] * row_a[2]);
    } else {
      return(0);
    }
  })));
}));
p_of_c

#Problem 2, Part B.
b = 0
simulate2B <- function(paramMatrix, p_of_c) {
  ret3 <- apply(paramMatrix, 1, function(row_a) {
    ret2 <- apply(paramMatrix, 1, function(row_b) {
      #cat(sprintf("P(A): %f | P(B): %f | T(A): %d, T(B): %d, abs(T(B)-T(A)): %d\n", row_a[2], row_b[2], row_a[1], row_b[1], abs(row_b[1] - row_a[1])));
      arrival_diff = row_b[1] - row_a[1];
      if(abs(arrival_diff) <= 1.25) {
        if(arrival_diff > 0) {
          return(row_b[2]*row_a[2]);
        } else {
          return(0);
        }
      } else {
        return(0);
      }
    });
    return(sum(ret2));
  });
  cat(print(ret3));cat(sprintf("\n"));
  answer = (sum(ret3) / p_of_c);
  return(answer);
}
simulate2B(paramMatrix, p_of_c);