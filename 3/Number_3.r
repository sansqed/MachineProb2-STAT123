
number_3 = function(n){
  Paul = 6 * n
  Yves = 6 * (n + 1)
  Paul_roll = sample(1:6, Paul, replace = T)
  Yves_roll = sample(1:6, Yves, replace = T)
  
  p = table(Paul_roll)
  y = table(Yves_roll)
  
  Paul_sixes = p[names(p) == 6]
  Yves_sixes = y[names(y) == 6]
  
  print(paste("Number of Rolls (Paul):",Paul))
  print(paste("Number of Rolls (Yves):",Yves))
  
  print(paste("Number of Sixes (Paul):",Paul_sixes))
  print(paste("Number of sixes (Yves):",Yves_sixes))
  
  probP_Paul = Paul_sixes/Paul
  probP_Yves = Yves_sixes/Yves
  
  print(paste("Success (Paul):", probP_Paul))
  print(paste("Fail (Paul):", probQ_Paul))A
  print(paste("Success (Yves):", probP_Yves))
  print(paste("Fail (Yves):", probQ_Yves))
  
  
  prob_Paul = 1 - pbinom(n - 1, size = Paul, prob = probP_Paul)
  prob_Yves = 1 - pbinom(n, size = Yves, prob = probP_Yves)
  
  print(prob_Paul)
  print(prob_Yves)
}