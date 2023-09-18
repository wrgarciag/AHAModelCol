##..............................................................................
# 3.1 Initial state ----
##..............................................................................

source('31MInitial.R')

##..............................................................................
# 3.2 Simulation ----
##..............................................................................

for (tt in TIME) {
  
  dt <- copy(pop0)
  
  #
  for(pp in POLICY){
    
    ## Demographic----
    
    
    ## Epidemiology----
    
    #CFc is the cause-specific transition probability 
    #from sick to dead from cause c 
    
    #BGmx,t is the cause-specific transition from sick to dead from any non-c cause 
    
    ## Economic----
    
    
  }
  
}