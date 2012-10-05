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
    if(totalValue >= 0.20) {
      numOver20 = numOver20 + 1;
    }
  }
  answer = (numOver20 / numTrials);
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
  return(answer);
}
simulate1B(paramVect, 100);
simulate1B(paramVect, 1000);
simulate1B(paramVect, 10000);
simulate1B(paramVect, 100000);

#Problem 2, Setup.
paramMatrix <- matrix(c(0.0,1.0,2.0,3.0,0.5, 0.25, 0.15, 0.10), nrow=4);

#Problem 2, Part A.
p_of_c1 <- sum(apply(paramMatrix, 1, function(row_a) {
  # compute the sum of the probabilities that comprise P(|T_A - T_B| <= 1.25)
  return(sum(apply(paramMatrix, 1, function(row_b) {
    # if bus A and bus B arrived within 1.25 minutes of each other
    if(abs(row_b[1] - row_a[1]) <= 1.25) {
      return(row_b[2] * row_a[2]); # return P(T_A=__ ^ T_B=__)
    } else {
      return(0);
    }
  })));
}));
p_of_c1

p_of_c2 <- sum(apply(paramMatrix, 1, function(row_a) {
  # compute the sum of the probabilities to get P(T_A - T_B <= 1.25)
  return(sum(apply(paramMatrix, 1, function(row_b) {
    # if bus A arrived before bus B or within 1.25 minutes of bus B leaving
    if(row_a[1] - row_b[1] <= 1.25) {
      return(row_a[2] * row_b[2]); # return P(T_A=__ ^ T_B=__)
    } else {
      return(0);
    }
  })));
}));
p_of_c2

#Problem 2, Part B.

simulate2B1 <- function(paramMatrix, p_of_c) {
  ret3 <- apply(paramMatrix, 1, function(row_a) {
    ret2 <- apply(paramMatrix, 1, function(row_b) {
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
simulate2B1(paramMatrix, p_of_c1);

simulate2B2 <- function(paramMatrix, p_of_c) {
  ret3 <- apply(paramMatrix, 1, function(row_a) {
    ret2 <- apply(paramMatrix, 1, function(row_b) {
      arrival_diff = row_a[1] - row_b[1];
      if(arrival_diff <= 1.25) {
        if(arrival_diff < 0) {
          return(row_a[2]*row_b[2]);
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
simulate2B2(paramMatrix, p_of_c2);