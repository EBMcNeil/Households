simulate <- function(n, HH_size, HH_type, HH_head, HH_child, HH_elder, HH_age, age.threshold=0.001, age.partner=0.75) {
  # R code to simulate n households based on  census data
  # The age of household members ranges from 0-9 (code=1), 10-19 (code=2), up to 80+ (code=9)
  # Based on a modified version of algorithm 3 by Gargiulo: 
  # https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0008828
  
  # The results can be used as input to model infectious disease dynamics. See, for example:
  # https://github.com/BDI-pathogens/OpenABM-Covid19/blob/master/documentation/covid19.md
  
  # This is a work in progress. Please send comments/feedback to Edward McNeil (emcneil@cuhk.edu.hk)
  
  # Issues to be resolved:
  # 1. I am having difficulty with choosing the constant rejection rate as proposed by Robert Hinch (see GitHub link above): "The threshold changes dynamically with each sample depending on whether it was accepted or rejected to keep a constant rejection rate throughout the sampling."
  # No matter what threshold I choose, the rejection rate is always about 50%!
  
  # Other issues to be considered are:
  # 1. The difference in ages between household heads and their partners
  #    (currently set to 0-9 or 10-19 years with 75% and 25% probability, respectively), 
  # 2. The age gap between multiple births, 
  # 3. Keeping the proportion of household types similar to that of the population,
  # 4. Allow more than 1 household head,
  # 5. Getting more data from census concerning household composition (eg. grandparents),
  # 6. Including gender (census does give this information),
  # 7. Improving the code. 
  
  # Start
  Start.time <- Sys.time()
  
  # Generate initial target sample (n rows and 9 columns)
  hh <- array(0, dim=c(n, 9)) 
  
  # Initialise the household types as these will be saved in the results
  Types <- NULL
  SS <- NULL
  
  # Number of rejections based on age discrepancy between sample and population marginals
  rejections.age <- 0
  
  # Initialise some parameters
  n_child <- 0
  n_parents <- 0
  
  i <- 1; k <- 1 # i is the accepted household ID, k is the overall number of iterations
  
  size <- NULL
  SS_ages <- 100 # Initial sum of squared differences between sample and population for the first household
  
  f <- 2 # this is my initial "fudge factor" for keeping a constant rejection rate. I need more time to understand this.
  
  # Step 1.
  # n households are sampled at random from all households with probability equal to the proportion in the population
  Sizes <- sample(HH_size$hh_size, size=n, replace=TRUE, prob=HH_size$p)
  
  # Check that this sample is similar to the population.
  p_size <- tabulate(Sizes) / sum(tabulate(Sizes))
  if(length(p_size)<6) {
    stop("\nn is too small (not all household sizes were sampled). Try a larger value.")
  }
  
  SS_size <- sum((HH_size$p - p_size)^2)

  while(SS_size > 0.001) {
    Sizes <- sample(HH_size$hh_size, size=n, replace=TRUE, prob=HH_size$p)
    p_size <- tabulate(Sizes) / sum(tabulate(Sizes))
    if(length(p_size)<6) {
      stop("\nn is too small (not all household sizes were sampled). Try a larger value.")
    }
    SS_size <- sum((HH_size$p - p_size)^2)
  }
  
  # Start the iteration
  while(i <= n) {
    # Select a household size from the list. (I think I can just use Sizes[i] instead of size throughout).
    size <- Sizes[i]
    
    # Step 2: Select the age of the household head (conditional on the household size)
    HH <- filter(HH_head, hh_size==size)
    age <- sample(HH$Age, size=1, prob=HH$p)
    
    # Function to convert the actual age groups recorded in the census data to an integer code for easier programming
    # If age is 65+ then assign one of the 3 highest age groups at random with probability based on HK census for households of size 1 (the 60-69 age group (code 7) gets less probability)
    get_age <- function(age){
      age_head <- ifelse(age %in% c("0 - 24", "25 - 29"), 3,
                         ifelse(age %in% c('30 - 34', '35 - 39'), 4,
                                ifelse(age %in% c('40 - 44', '45 - 49'), 5,
                                       ifelse(age %in% c('50 - 54', '55 - 59'), 6,
                                              ifelse(age=='60 - 64', 7,
                                                     sample(7:9, size=1, prob=c(0.194/2, 0.124, 0.114)))))))
    }
    
    age_head <- get_age(age)

    # Assign to the target
    hh[i, age_head] <- 1
    
    if(size==1) {
      type <- 7 # Type can only be a single household if the size is 1. End of this iteration.
    }
    else {
      # Step 3: Select the type of household (conditional on size of household and age of household head)
      HH <- filter(HH_type, hh_size==size & Age==age)
      type <- sample(HH$hh_type, 1, prob=HH$p)
      
      # If the age of the household head exceeds 7 (age 70+), the household type cannot be 4 or 5 (households with parents).
      while(age_head>7 & type %in% c(4,5)) {
          type <- sample(HH$hh_type, 1, prob=HH$p)
      }
      
      # Households with couples (with or without children, with or without their parents)
      if(type %in% c(1,2,4,5)) { 
        # The gap in ages between couples (assume there is no gap 75% of the time).
        # and assume the gap is no more than 1 age group (10 years).
        age_gap <- rbinom(1, 1, prob=1-age.partner) 
        age_partner <- min(9, max(3, age_head-age_gap)) # clamping
        
        # Assign the target
        hh[i, age_partner] <- hh[i, age_partner] + 1
      }
      else { # Household does not have a couple (other relationship or complex household types)
        if(type %in% c(6,8)) {
          # The other household members are either children aged <15 yrs, adults aged 16-65, or elders aged 65+.
          # Use the census to determine the probability of n children and n elders (n=0,1,2,3) living in households of this size/type
          HH_c <- filter(HH_child, hh_size==size & hh_type==type)
          p_child <- HH_c$p # Probabilities for n=0,1,2,3 children (aged <15) in this household
          HH_e <- filter(HH_elder, hh_size==size & hh_type==type)
          p_elder <- HH_e$p # Probabilities for n=0,1,2,3 elders (age 65+) in this household
          n_child <- n_elder <- size # Initialise for the while loop 
          while(n_child+n_elder>=size) { # Try again if sampling results in too many children and/or elders
            n_child <- sample(HH_c$child, 1, prob=p_child) # Choose number of children in this household
            n_elder <- sample(HH_e$elder, 1, prob=p_elder) # Choose number of elders in this household
          }
          # The Number of adults (apart from head) is now perfectly determined
          # It may be 0 if n_child + n_elder == size. Subtract 1 for the household head.
          n_adult <- size - n_child - n_elder - 1 
          
          # Select the ages of the child(ren). Assign a lower probability for those age 10-19 since children are <15.
          for(j in seq_len(n_child)) { 
            age_child <- sample(1:2, 1, prob=c(2, 1)/3)
            hh[i, age_child] <- hh[i, age_child] + 1
          }
          # Select the ages of the elders. Assign a lower probability for those age 60-69 since elders are 65+.
          for(j in seq_len(n_elder)) { 
            age_elder <- sample(7:9, 1, prob=c(1, 2, 2)/5)
            hh[i, age_elder] <- hh[i, age_elder] + 1
          }
          # For adults, the 2 outer age groups (10-19 and 60-69) have a lower probability than the 4 inner ones 
          for(j in seq_len(n_adult)) { 
            age_adult <-  sample(2:7, 1, prob=c(0.1, 0.2, 0.2, 0.2, 0.2, 0.1))
            hh[i, age_adult] <- hh[i, age_adult] + 1
          }
        } # household type is 6 (other) or 8 (complex)
      }
      
      # Determine number of children and parents (1 or 2) based on household type and size
      if(type %in% c(2,3,5)) { 
        if(type == 5) { # number of children depends on whether there are 1 or 2 (grand)parents
          # Choose the number of (grand)parents based on household size.
          # Maybe households with a higher size should have a higher probability of having both parents?
          if(size==4) 
            n_parents <- 1 # households of size 4 with 2 parents and 1 child can only have 1 (grand)parent.
          else # otherwise, randomly choose 1 or 2 (grand)parents with equal probability.
            n_parents <- 1 + rbinom(1, 1, prob=0.5)
          
          # The number of children is now perfectly determined in these types of households
          n_child <- size - n_parents - 2
        }
        else # type is either 2 (both parents) or 3 (single parent) with child(ren)
          # The number of children depends on whether there are 1 (type=3) or 2 (type=2) parents
          n_child <- size - (4 - type)
        
        # Assign the child's age (assume it is 30 years less than the head or partner's age +/- 5 years).
        # Based on the census, which states that the median age of women at first childbirth is 31.8 years.
        # (https://www.women.gov.hk/download/research/HK_Women2019_e.pdf)
        
        # Decide who to use as the "mother"
        if(type==3) # single parent
          age_parent <- age_head
        else # both parents
          age_parent <- age_partner
        
        # Mother's age when the first child was delivered 
        # Assume normally distributed with mean 30 +/- 10 but clamped between 10 and current age (2 and age_parent)
        age_deliver <- min(age_parent, max(2, round(rnorm(1, mean=3, sd=1))))
        
        age_child <- NULL
        age_child[1] <- max(1, age_parent - age_deliver) # This needs checking carefully. I think it works.
        hh[i, age_child[1]] <- 1
        
        if(n_child>1) {
          for(j in 2:n_child) {
            age_deliver <- min(age_parent, max(2, round(rnorm(1, mean=3, sd=1))))
            age_child[j] <- max(1, age_parent - age_deliver)
            
            # Ensure all ages are in the same or adjacent age group before assigning to the target
            # This can be modified if there are a large proportion of children in Hong Kong who are aged >10 years apart
            while(var(age_child) > 0.5) { # I will double-check this later. I *think() it is correct.
              age_deliver <- max(2, min(age_parent, round(rnorm(1, mean=3, sd=1))))
              age_child[j] <- max(1, age_parent - age_deliver)
            }
            hh[i, age_child[j]] <- hh[i, age_child[j]] + 1
          }
        }
        
        # Parents of household head. Their age must be 10-50 years older than the household head.
        # Assume age of parent follows a normal distribution with mean 20 years higher than the household head 
        # and sd 10 years
        # Based on historical trends of median age of women at first childbirth.
        if(type %in% c(4,5)) { 
          if(type==4) n_parents <- size - 2 # for type=5, the number of parents has been determined (line 156)
          if(age_head==9) n_parents <- 0  # adults who are 80+ cannot live with their parents.
          for(j in seq_len(n_parents)) { 
            # Clamp age range of parents at 4-9, and 1-5 (10-50 years) higher than head
            age_parent <- max(4, min(9, age_head + max(1, min(5, round(rnorm(1, mean=2, sd=1))))))
            hh[i, age_parent] <- hh[i, age_parent] + 1
          }
        }
        
        # Step 6.
        # There may be other non-related persons, for example domestic helpers.
        # Their ages can be chosen based on census data. Assume not a child.
        n_others <- size - sum(hh[i,])
        if(n_others>0) {
          # Create an "Other" database from the census based on type and size
          HH_other <- within(subset(HH_type, subset=hh_type==type & hh_size==size), {
            p=n/sum(n)
            age=c(3,3,4,4,5,5,6,6,7,8) } )
          HH_other <- aggregate(p~age, FUN=sum, data=HH_other)
          # Calculate the probability for each age group
          for(j in seq_len(n_others)) {
            age_other <- sample(HH_other$age, size=1, prob=HH_other$p)
            if(age_other==8) # The census doesn't give all the age groups. It stops at 65+
              age_other <- sample(8:9, 1)
            hh[i, age_other] <- hh[i, age_other] + 1
          }
        }
      } # End of block: household has children (type in {2,3,5})
    } # End of block: assigning individuals to this household
    
    # Calculate the household members' age proportions and sum of squared differences with population.
    p_age <- colSums(hh) / sum(colSums(hh))
    SS_age <- sum((HH_age$p - p_age)^2)
    
    # Check that this new household does not increase the discrepancy with the target by more than a threshold .
    # If it does, reject this household sample.
    if(SS_age - SS_ages > age.threshold) {
      rejections.age <- rejections.age + 1
      
      f <- f + 1 # fudge factor to keep a constant rejection rate. Seems to be about 50% at the moment.
      # Higher rejections mean more accuracy but longer computation time (in theory).

      hh[i,] <- 0  # Reject this household. Undo everything, and try again.
    }
    else {
      # Save the household types
      Types[i] <- type
      
      # Adjust the fudge factor to keep the rejection rate constant.
      f <- max(1, f - 1)
      
      # Next household
      i <- i + 1
    }
    
    k <- k + 1 # increment the number of iterations
    
    # Save previous value for comparison with next one
    SS_ages <- SS_age
    SS[i] <- SS_age
    
    # Modify the threshold such that the rejection rate is constant. Needs more work!
    # Currently, the line below results in a constant rejection rate of around 50%. Not sure how to change this.
    age.threshold <- age.threshold * (f / k) 

    ctime <- Sys.time() - Start.time
    
    # Show current progress
    Sys.sleep(0.001)
    cat("\r i:", k, " of", n,
        "SS (age):", sprintf("%.6f", SS_ages), 
        "Accepted:", i, "(", sprintf("%.1f", 100*i/k), "%), ", 
        "Rejected:", k-i, "(", sprintf("%.1f", 100*(k-i)/k), "%)",
        "Time:", ctime)
  
    # Abort if there are too many rejections/iterations.
    #if((rejections.age)/k > 1 | k > 3 * n){
    #  cat("\nToo many rejections. Try larger threshold values.")
    #  return(NULL)
    #}
  } # End of block: iterations
  
  # Check proportion of household types
  p_type <- tabulate(Types) / sum(tabulate(Types))
  if(length(p_type)<8) {
    stop("\nn is too small (not all household types were sampled). Try a larger value.")
  }
  
  # Calculate the proportion of household types and sum of squared differences with population.
  HH_types <- within(aggregate(n~hh_type, FUN=sum, data=HH_type), p <- n/sum(n))
  SS_types <- sum((HH_types$p - p_type)^2)
  cat("\nDiscrpepancy in household types between sample and population is:", SS_types, "\n")

  # Finished
  print(ctime)
  
  # Save results 
  colnames(hh) <- paste(10*0:8, 9+10*0:8, sep=" - ")
  rownames(hh) <- 1:n
  list(hh=hh, type=Types, SS=SS)
}
