### generate diets based on a given environment
### number of individuals, number of bites, and sampling procedure

diet_gen = function(sad, method, n, inds, order=NA, rawonly=FALSE){
  
  diets = matrix(0, ncol=length(sad), nrow=inds)
  
  colnames(diets) = names(sad)
  
  if(method=='random'){
    
    for(i in 1:inds){
      
      x = table(sample(names(sad), size=n, replace=TRUE, prob=sad))
      
      ranab = x/sum(x)
      
      diets[i,match(names(ranab), colnames(diets))] = ranab
      
    }
    
    if(rawonly==TRUE) return(diets)
    
    if(rawonly==FALSE) return(list(diets=diets, av=colMeans(diets)))
    
  }else if(method == 'random_threshold'){
    
    for(i in 1:inds){
      
      x <- table(sample(names(sad), size=n, replace=TRUE))
      
      ranab <- x/sum(x)
      
      diets[i,match(names(ranab), colnames(diets))] <- ranab
      
    }
    
    if(rawonly==TRUE) return(diets)
    
    if(rawonly==FALSE) return(list(diets=diets, av=colMeans(diets)))
    
  }else if(method == 'sto_norm'){
    
    for(i in 1:inds){
      
      regs <- rnorm(n=length(sad), mean=1, sd=0.4)
      
      if(any(regs < 0)) regs[which(regs < 0)] <- 0
      
      regs <- regs[order(regs)]
      
      names(regs) <- order
      
      x <- table(sample(names(sad), size=n, replace=TRUE, prob=(sad*regs[match(names(sad), names(regs))])))
      
      ranab <- x/sum(x)
      
      diets[i,match(names(ranab), colnames(diets))] <- ranab
      
    }
    
    if(rawonly==TRUE) return(diets)
    
    if(rawonly==FALSE) return(list(diets=diets, av=colMeans(diets)))
    
    
    
  }else if(method == 'corr_norm'){
    
    for(i in 1:inds){
      
      regs <- rnorm(n=length(sad), mean=1, sd=0.4)
      
      if(any(regs < 0)) regs[which(regs < 0)] <- 0
      
      regs <- regs[order(regs)]
      
      names(regs) <- names(sad)[order(sad)]
      
      x <- table(sample(names(sad), size=n, replace=TRUE,  prob=(sad*regs[match(names(sad), names(regs))])))
      
      ranab <- x/sum(x)
      
      diets[i,match(names(ranab), colnames(diets))] <- ranab
      
    }
    
    if(rawonly==TRUE) return(diets)
    
    if(rawonly==FALSE) return(list(diets=diets, av=colMeans(diets)))
    
  }else if(method == 'sto_exp'){
    
    for(i in 1:inds){
      
      regs <- rexp(n=length(sad), rate=1.5)
      
      if(any(regs < 0)) regs[which(regs < 0)] <- 0
      
      regs <- regs[order(regs)]
      
      names(regs) <- order
      
      x <- table(sample(names(sad), size=n, replace=TRUE, prob=(sad*regs[match(names(sad), names(regs))])))
      
      ranab <- x/sum(x)
      
      diets[i,match(names(ranab), colnames(diets))] <- ranab
      
    }
    
    if(rawonly==TRUE) return(diets)
    
    if(rawonly==FALSE) return(list(diets=diets, av=colMeans(diets)))
    
  }else if(method == 'corr_exp'){
    
    for(i in 1:inds){
      
      regs <- rexp(n=length(sad), rate=1.5)
      
      if(any(regs < 0)) regs[which(regs < 0)] <- 0
      
      regs <- regs[order(regs)]
      
      names(regs) <- names(sad)[order(sad)]
      
      x <- table(sample(names(sad), size=n, replace=TRUE,  prob=(sad*regs[match(names(sad), names(regs))])))
      
      ranab <- x/sum(x)
      
      diets[i,match(names(ranab), colnames(diets))] <- ranab
      
    }
    
    if(rawonly==TRUE) return(diets)
    
    if(rawonly==FALSE) return(list(diets=diets, av=colMeans(diets)))
    
  }
  
}
