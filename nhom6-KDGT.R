library(MASS) 

#3.1. Kiem dinh gia thuyet ve ky vong trung binh

#TH1: mu # mu0, TH2: mu> mu0, TH3: mu<mu0

KDGTveKV<-function(xbar,s,mu0,sgma,n,TH1,TH2,TH3,alpha,bietPS)
{
  #biet phuong sai
  if(bietPS==TRUE){
    
    uqs <- (xbar-mu0)*sqrt(n)/sgma
    
    if(TH1==TRUE){
      
      zstar <- qnorm(1-alpha/2)
      Wa1 <- c(-Inf,-round(zstar, 4))
      Wa2 <- c(round(zstar, 4), Inf)
      cat("uqs=",round(uqs, 4),"\n")
      cat("Wa=","(",-Inf,";",round(-zstar, 4),")","U","(",round(zstar, 4),";",Inf,")","\n")
      if(uqs < -zstar||uqs > zstar){
        cat("Bac bo gia thuyet H0")
      }else if(bietPS==FALSE){
        cat("Chua co co so bac gia thuyet H0")
      }
      
    }else if(TH2==TRUE){
      
      zstar <- qnorm(1-alpha)
      Wa <- c(zstar,Inf)
      cat("uqs=",round(uqs, 4),"\n")
      cat("Wa=","(",round(zstar, 4),";",Inf,")","\n")
      if(uqs > zstar){
        cat("Bac bo gia thuyet H0")
      }else{
        cat("Chua co co so bac bo gia thuyet H0")
      }
      
    }else if(TH3==TRUE){
      zstar <- qnorm(1-alpha)
      Wa <- c(-Inf,-zstar)
      cat("uqs=",round(uqs, 4),"\n")
      cat("Wa=","(",-Inf,";",round(-zstar, 4),")","\n")
      if(uqs < -zstar){
        cat("Bac bo gia thuyet H0")
      }else{
        cat("Chua co co so bac bo gia thuyet H0")
      }
    }
    
  }else{
    # chua biet phuong sai
    tqs <- (xbar-mu0)*sqrt(n)/s
    
    if(TH1==TRUE){
      zstar <- qt(1-alpha/2,df=n-1)
      Wa1 <- c(-Inf,-round(zstar, 4))
      Wa2 <- c(round(zstar, 4), Inf)
      cat("tqs=",round(tqs, 4),"\n")
      cat("Wa=","(",-Inf,";",round(-zstar, 4),")","U","(",round(zstar, 4),";",Inf,")","\n")
      if(tqs < -zstar||tqs > zstar){
        cat("Bac bo gia thuyet H0")
      }else{
        cat("Chua co co so bac gia thuyet H0")
      }
      
    }else if(TH2==TRUE){
      zstar <- qt(1-alpha, df=n-1)
      Wa <- c(zstar, Inf)
      cat("tqs=",round(tqs, 4),"\n")
      cat("Wa=","(",round(zstar, 4),";",Inf,")","\n")
      if(tqs > zstar){
        cat("Bac bo gia thuyet H0")
      }else{
        cat("Chua co co so bac bo gia thuyet H0")
      }
      
    }else if(TH3==TRUE){
      zstar <- qnorm(1-alpha,df=n-1)
      Wa <- c(-Inf,-zstar)
      cat("tqs=",round(tqs, 4),"\n")
      cat("Wa=","(",-Inf,";",round(-zstar, 4),")","\n")
      if(uqs < -zstar){
        cat("Bac bo gia thuyet H0")
      }else{
        cat("Chua co co so bac bo gia thuyet H0")
      }
    }
  }
  
  cat("Phuong phap p-giatri:\n")
  if(TH1 == TRUE)
    p_giatri=2*(1-pnorm(abs(uqs),mean=0,sd=1))
  else if(TH2 == TRUE)
    p_giatri=1-pnorm(uqs,mean=0,sd=1)
  else 
    p_giatri=pnorm(uqs,mean=0,sd=1)
  if(p_giatri<0.05)
    cat("p-giatri=",p_giatri,"\t=>Bac bo gia thuyet Ho\n")
  else
    cat("p-giatri=",p_giatri,"\t=>Chua du co so bac bo Ho\n")
}
#VD:
KDGTveKV(xbar = 8900, sgma = 2600,n = 25,mu0 = 8500, TH1 = TRUE, TH2 = FALSE, TH3 = FALSE, alpha = 0.05, bietPS = TRUE)


#3.2. Kiem dinh gia thuyet ve ty le

#TH1:p # p0 TH2: p > p0 TH3: p< p0

KDGTveTyle <- function(m,n,p0,alpha,TH1,TH2,TH3)
{
  
  f <- m/n
  uqs <- (f-p0)*sqrt(n)/sqrt(p0*(1-p0))
  
  cat("uqs=",round(uqs, 4),"\n")
  if(TH1==TRUE){
    
    zstar <- qnorm(1-alpha/2)
    cat("Wa=","(",-Inf,";",round(-zstar, 4),")","U","(",round(zstar, 4),";",Inf,")","\n")
    if(uqs < -zstar || uqs > zstar){
      cat("Bac bo gia thuyet H0")
    }else{
      cat("Chua co co so bac bo gia thuyet H0")
    }
    
  }else if(TH2==TRUE){
    zstar <- qnorm(1-alpha)
    cat("Wa=","(",round(zstar, 4),";",Inf,")","\n")
    if(uqs > zstar){
      cat("Bac bo gia thuyet H0")
    }else{
      cat("Chua co co so bac bo gia thuyet H0")
    }
    
  }else if(TH3==TRUE){
    zstar <- qnorm(1-alpha)
    cat("Wa=","(",-Inf,";",round(-zstar, 4),")","\n")
    if(uqs < -zstar){
      cat("Bac bo gia thuyet H0")
    }else{
      cat("Chua co co so bac bo gia thuyet H0")
    }
  }
  cat("Phuong phap p-giatri:\n")
  if(TH1 == TRUE)
    p_giatri=2*(1-pnorm(abs(uqs),mean=0,sd=1))
  else if(TH2 == TRUE)
    p_giatri=1-pnorm(uqs,mean=0,sd=1)
  else 
    p_giatri=pnorm(uqs,mean=0,sd=1)
  if(p_giatri<0.05)
    cat("p-giatri=",p_giatri,"\t=>Bac bo gia thuyet Ho\n")
  else
    cat("p-giatri=",p_giatri,"\t=>Chua du co so bac bo Ho\n")
}
#VD: 
KDGTveTyle(m = 10, n = 85, p0 = 0.1, alpha = 0.05, TH1 = FALSE, TH2 = TRUE, TH3 = FALSE)



#3.3. Kiem dinh gia thuyet ve phuong sai

#Chua biet ky vong

#TH1: sgma # sgma0 TH2: sgma > smga0 TH3: sgma < sgma0

KDGTvePS_chuabietKV <- function(s,n,sgma0,TH1,TH2,TH3,alpha)
{
  
  X_2_qs <- (n-1)*(s^2)/sgma0
  cat("X_2_qs=",round(X_2_qs, 4),"\n")
  
  if(TH1==TRUE){
    
    t1 <- qchisq(alpha/2, df=n-1)
    t2 <- qchisq(1-alpha/2, df=n-1)
    cat("Wa=","(",-Inf,";",round(t1, 4),")","U","(",round(t2, 4),";",Inf,")","\n")
    if(X_2_qs < t1 || X_2_qs > t2){
      cat("Bac bo gia thuyet H0")
    }else{
      cat("Chua co co so bac bo gia thuyet H0")
    }
    
  }else if(TH2==TRUE){
    t <- qchisq(1-alpha, df=n-1)
    cat("Wa=","(",round(t, 4),";",Inf,")","\n")
    if(X_2_qs > t){
      cat("Bac bo gia thuyet H0")
    }else{
      cat("Chua co co so bac bo gia thuyet H0")
    }
    
  }else if(TH3==TRUE){
    t <- qchisq(alpha, df=n-1)
    cat("Wa=","(",-Inf,";",round(t, 4),")","\n")
    if(X_2_qs < t){
      cat("Bac bo gia thuyet H0")
    }else{
      cat("Chua co co so bac bo gia thuyet H0")
    }
  }
}
#VD:
h <- c(3.48,3.52,3.5,3.47,3.49,3.54,3.51,3.62,3.46,3.45,3.55,3.48,3.51,3.52,3.50)
KDGTvePS_chuabietKV(n=15,s=sd(h),sgma0=0.02,TH2=TRUE,TH1=FALSE,alpha=0.05)

#Da biet ky vong

#TH1: sgma # sgma0 TH2: sgma > sgma0 TH3: sgma < sgma0

KDGTvePSP_bietKV <- function(x,mu0,sgma0,TH1,TH2,TH3,alpha)
{
  
  X_2_qs <- sum((x-mu0)^2)/(sgma0^2)
  cat("X_2_qs=",round(X_2_qs, 4),"\n")
  
  if(TH1==TRUE){
    
    t1 <- qchisq(alpha/2,df=n)
    t2 <- qchisq(1-alpha/2,df=n)
    cat("Wa=","(",-Inf,";",round(t1, 4),")","U","(",round(t2, 4),";",Inf,")","\n")
    if(X_2_qs < t1 || X_2_qs > t2){
      cat("Bac bo gia thuyet H0")
    }else{
      cat("Chua co co so bac bo gia thuyet H0")
    }
    
  }else if(TH2==TRUE){
    t <- qchisq(1-alpha, df=n)
    cat("Wa=","(",round(t, 4),";",Inf,")","\n")
    if(X_2_qs > t){
      cat("Bac bo gia thuyet H0")
    }else{
      cat("Chua co co so bac bo gia thuyet H0")
    }
    
  }else if(TH3==TRUE){
    t <- qchisq(alpha, df=n)
    cat("Wa=","(",-Inf,";",round(t, 4),")","\n")
    if(X_2_qs < t){
      cat("Bac bo gia thuyet H0")
    }else{
      cat("Chua co co so bac bo gia thuyet H0")
    }
  }
}
#VD:


#3.3. so sanh 2 tham so 
#3.3.1. so sanh 2 ky vong

#TH1: mu1-mu2 # 0, TH2: mu1-mu2 > 0, TH3: mu1-mu2 < 0

SS2KV <- function(n1,n2,xbar1,xbar2,sgma1,sgma2,s1,s2,TH1,TH2,TH3,bietPS,alpha)
{
  #phuong sai da biet
  if(bietPS == TRUE){
    uqs <- (xbar1 - xbar2)/sqrt((sgma1^2/n1)+(sgma2^2/n2))
    if(TH1 == TRUE){
      zstar <- qnorm(1-alpha/2)
      Wa1 <- c(-Inf,-round(zstar, 4))
      Wa2 <- c(round(zstar, 4), Inf)
      cat("uqs=",round(uqs, 4),"\n")
      cat("Wa=","(",-Inf,";",round(-zstar, 4),")","U","(",round(zstar, 4),";",Inf,")","\n")
      if(uqs < -zstar||uqs > zstar){
        cat("Bac bo gia thuyet H0")
      }else if(bietPS==FALSE){
        cat("Chua co co so bac gia thuyet H0")
      }
    }
    
    else if(TH2==TRUE){
      zstar <- qnorm(1-alpha)
      Wa <- c(zstar,Inf)
      cat("uqs=",round(uqs, 4),"\n")
      cat("Wa=","(",round(zstar, 4),";",Inf,")","\n")
      if(uqs > zstar){
        cat("Bac bo gia thuyet H0")
      }else{
        cat("Chua co co so bac bo gia thuyet H0")
      }
    }
    
    else if(TH3==TRUE){
      zstar <- qnorm(1-alpha)
      Wa <- c(-Inf,-zstar)
      cat("uqs=",round(uqs, 4),"\n")
      cat("Wa=","(",-Inf,";",round(-zstar, 4),")","\n")
      if(uqs < -zstar){
        cat("Bac bo gia thuyet H0")
      }else{
        cat("Chua co co so bac bo gia thuyet H0")
      }
    }
  }
  
  #chua biet phuong sai voi dk n1<30, n2<30
  # DK: sgma1^2 = sgma2^2 = sgma0^2 
  else if(n1<30 & n2<30){
    
    tqs <- (xbar1 - xbar2)/sqrt(((1/n1)+(1/n2))*(((n1-1)*s1^2 +(n2-1)*s2^2)/(n1+n2-2)))
    
    if(TH1==TRUE){
      zstar <- qt(1-alpha/2,df=n1+n2-2)
      Wa1 <- c(-Inf,-round(zstar, 4))
      Wa2 <- c(round(zstar, 4), Inf)
      cat("tqs=",round(tqs, 4),"\n")
      cat("Wa=","(",-Inf,";",round(-zstar, 4),")","U","(",round(zstar, 4),";",Inf,")","\n")
      if(tqs < -zstar||tqs > zstar){
        cat("Bac bo gia thuyet H0")
      }else{
        cat("Chua co co so bac gia thuyet H0")
      }
    }
    
    else if(TH2==TRUE){
      zstar <- qt(1-alpha, df=n1+n2-2)
      Wa <- c(zstar, Inf)
      cat("tqs=",round(tqs, 4),"\n")
      cat("Wa=","(",round(zstar, 4),";",Inf,")","\n")
      if(tqs > zstar){
        cat("Bac bo gia thuyet H0")
      }else{
        cat("Chua co co so bac bo gia thuyet H0")
      }
    }
    
    else if(TH3==TRUE){
      zstar <- qnorm(1-alpha,df=n1+n2-2)
      Wa <- c(-Inf,-zstar)
      cat("tqs=",round(tqs, 4),"\n")
      cat("Wa=","(",-Inf,";",round(-zstar, 4),")","\n")
      if(uqs < -zstar){
        cat("Bac bo gia thuyet H0")
      }else{
        cat("Chua co co so bac bo gia thuyet H0")
      }
    }
    
  }
  #chua biet phuong sai voi dk n1 >=30, n2 >= 30
  else{
    s <- sqrt((s1^2/n1)+(s2^2/n2))
    uqs <- (xbar1 - xbar2)/s
    
    if(TH1 == TRUE){
      zstar <- qnorm(1-alpha/2)
      Wa1 <- c(-Inf,-round(zstar, 4))
      Wa2 <- c(round(zstar, 4), Inf)
      cat("uqs=",round(uqs, 4),"\n")
      cat("Wa=","(",-Inf,";",round(-zstar, 4),")","U","(",round(zstar, 4),";",Inf,")","\n")
      if(uqs < -zstar||uqs > zstar){
        cat("Bac bo gia thuyet H0")
      }else if(bietPS==FALSE){
        cat("Chua co co so bac gia thuyet H0")
      }
    }
    
    else if(TH2==TRUE){
      zstar <- qnorm(1-alpha)
      Wa <- c(zstar,Inf)
      cat("uqs=",round(uqs, 4),"\n")
      cat("Wa=","(",round(zstar, 4),";",Inf,")","\n")
      if(uqs > zstar){
        cat("Bac bo gia thuyet H0")
      }else{
        cat("Chua co co so bac bo gia thuyet H0")
      }
    }
    
    else if(TH3==TRUE){
      zstar <- qnorm(1-alpha)
      Wa <- c(-Inf,-zstar)
      cat("uqs=",round(uqs, 4),"\n")
      cat("Wa=","(",-Inf,";",round(-zstar, 4),")","\n")
      if(uqs < -zstar){
        cat("Bac bo gia thuyet H0")
      }else{
        cat("Chua co co so bac bo gia thuyet H0")
      }
    }
  }
}
#VD TH da biet phuong sai
SS2KV(n1=10,n2=10,xbar1 = 121,xbar2 = 112, sgma1 = 8, sgma2 = 8,TH1 = TRUE, TH2 = FALSE, TH3 = FALSE, bietPS = TRUE, alpha = 0.05)

# VD Th chua biet phuong sai voi dk n1,n2 < 30
c1 <- c(47.2, 43.1, 35.7, 47.0, 5.7, 42.6, 46.7, 42.3)
print(c1[1])
c2 <- c(47.9, 48.9, 43.5, 53.1, 50.8, 46.1, 41.1, 43.0, 41.0, 48.5, 47.7)
SS2KV(n1 = 8, n2 = 11, xbar1 = mean(c1) , xbar2 = mean(c2), s1 = sd(c1), s2 = sd(c2),TH1 = FALSE, TH2 = TRUE, TH3 = FALSE, bietPS = FALSE, alpha = 0.05) 

#VD TH chua biet phuong sai voi dk n1,n2 >= 30
SS2KV(n1 = 31, n2 = 31, xbar1 = 12, xbar2 = 12.3, s1 = 1.2, s2 = 1.4, TH1 = TRUE, TH2 = FALSE, TH3 = FALSE, bietPS = FALSE, alpha = 0.05)

#3.3.2. so sanh 2 ty le 

kiemdinh2tyle=function(x,y,n1,n2,alpha,dk)
{
  p=(x+y)/(n1+n2)
  p1=x/n1
  p2=y/n2
  u_qs=(p1-p2)/sqrt(p*(1-p)*(1/n1+1/n2))
  cat("u_qs= ",u_qs,"\n")
  if(dk=='p1!=p2'){
    u=qnorm(1-alpha/2,mean=0,sd=1)
    cat("Mien bac bo W(",alpha,") = (-inf;-",u,")&(",u,";+inf)\n")
    if(u_qs>u||u_qs<(-u))
      cat("Bac bo gia thuyet Ho\n")
    else
      cat("Chua du co so bac bo gia thuyet Ho\n")
  }
  else if(dk=='p1>p2'){
    u=qnorm(1-alpha,mean=0,sd=1)
    cat("Mien bac bo W(",alpha,") = (",u,";+inf)\n")
    if(u_qs>u)
      cat("Bac bo gia thuyet Ho\n")
    else
      cat("Chua du co so bac bo gia thuyet Ho\n")
  }
  else if(dk=='p1<p2'){
    u=qnorm(1-alpha,mean=0,sd=1)
    cat("Mien bac bo W(",alpha,") = (-inf;-",u,")\n")
    if(u_qs<(-u))
      cat("Bac bo gia thuyet Ho\n")
    else
      cat("Chua du co so bac bo gia thuyet Ho\n")
  }
  cat("Phuong phap p-giatri:\n")
  if(dk=='p1!=p2')
    p_giatri=2*(1-pnorm(abs(u_qs),mean=0,sd=1))
  else if(dk=='p1>p2')
    p_giatri=1-pnorm(u_qs,mean=0,sd=1)
  else 
    p_giatri=pnorm(u_qs,mean=0,sd=1)
  if(p_giatri<0.05)
    cat("p-giatri=",p_giatri,"\t=>Bac bo gia thuyet Ho\n")
  else
    cat("p-giatri=",p_giatri,"\t=>Chua du co so bac bo Ho\n")
}

#VD:
kiemdinh2tyle(20,30,1000,900,0.05,'p1!=p2')



#3.3.3. so sanh 2 phuong sai
kiemdinh2phuongsai=function(n1,n2,s1,s2,alpha,dk)
{
  f_qs=s1^2/s2^2
  cat("f_qs= ",f_qs,"\n")
  if(dk=='sd1^2!=sd2^2'){
    u1=qf(alpha/2,n1-1,n2-1)
    u2=qf(1-alpha/2,n1-1,n2-1)
    cat("Mien bac bo: w(",alpha,")= (-inf;-",u,")&(",u,";+inf)\n")
    if(f_qs<u1||f_qs>u2)
      cat("Bac bo gia thuyet Ho\n")
    else
      cat("Chua du co so bac bo gia thuyet Ho\n")
  }
  else{
    u=qf(alpha,n1-1,n2-1)
    cat("Mien bac bo: w(",alpha,")= (",u,";+inf)\n")
    if(f_qs>u)
      cat("Bac bo gia thuyet Ho\n")
    else
      cat("Chua du co so bac bo gia thuyet Ho\n")
  }
}

#VD: 
kiemdinh2phuongsai(10,10,50,60,0.05,'sd1^2>sd2^2')




















