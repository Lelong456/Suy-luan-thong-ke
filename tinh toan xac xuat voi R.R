setwd("D:/ôn thi hoc ki/Suy luan thong ke/báo cáo")
Data <- read.csv("marketing_campaign.csv", header = TRUE)
attach(Data)

y = Data[["Income"]]
x1 = Data[["MntFruits"]]
x2 = Data[["MntGoldProds"]]
x3 = Data[["MntFishProducts"]]
x4 = Data[["MntMeatProducts"]]

X_1 <- gl(1,length(y))
X_2 <- x1
X_3 <- x2
X_4 <- x3 
X_5 <- x4
X <- data.frame(X_1,X_2,X_3,X_4,X_5)
colnames(X) <- c("1","2","3","4","5")
X <- as.matrix(X)
Y <- y
print(X)
print(Y)
#Xac dinh cac he so hoi quy mau va phuong trinh hoi quy
# Tinh beta mu
B_mu <- solve(t(X) %*% X) %*% (t(X) %*% Y)
cat("B_mu0 = ", B_mu[1,1], "\n")
cat("B_mu1 = ", B_mu[2,1], "\n")
cat("B_mu2 = ", B_mu[3,1], "\n")
cat("B_mu3 = ", B_mu[4,1], "\n")
cat("B_mu0 = ", B_mu[5,1], "\n")

# tinh y_mu
Y_mu <- 0
Y_mu <- B_mu[1,1] + B_mu[2,1]*x1 + B_mu[3,1]*x2 + B_mu[4,1]*x3 + B_mu[5,1]*x4

# Tinh phan du 
e <- 0 
e <- Y - Y_mu

# tinh tong binh phuong sai so 
SSE <- 0
SSE <- sum(e^2)
cat("SSE =", SSE, "\n")

#tinh tong binh phuong cho hoi quy
SSR <- 0
SSR <- sum((Y_mu-mean(Y))^2)
cat("SSR =", SSR, "\n")

#Tinh so giao dong trong mau cua bien phu thuoc
SST <- SSE + SSR
cat("SST =", SST, "\n")

#sai so tieu chuan cua ham hoi quy
MSE <- 0 
MSE <- SSE/(15-4-1)  # sse / (n - k - 1 )
cat("MSE =",MSE,"\n")
#trung binh binh phuong hoi quy
MSR <- SSR/4

#He so xac dinh cua ham hoi quy
R_mu2 = SSR/SST

#2.lap bang ANOVa
bac_tu_do <- c(4, 10, 14)
SS <- c(SSR,SSE,SST)
MS <- c(MSR,MSE, "")
f <- c("","",MSR/MSE)
ANOVA <- data.frame(bac_tu_do,SS,MS,f)
rownames(ANOVA) <- c("Hoi quy (R)","sai so (E)","tong")
colnames(ANOVA) <- c("bac tu do", "SS","MS","f")
print(ANOVA)

#Nhan xet su phu hop cua mo hinh 
if(sqrt(R_mu2)> 0.85){
  cat("Mo hinh tuong quan hoan hao")
}else if(sqrt(R_mu2)>= 0.76 & sqrt(R_mu2)<= 0.85){
  cat ("mo hinh tuong quan rat manh")
}else if(sqrt(R_mu2)>= 0.51 & sqrt(R_mu2)<= 0.75){
  cat ("mo hinh tuong quan manh")
}else if(sqrt(R_mu2)>= 0.26 & sqrt(R_mu2)<= 0.50){
  cat ("mo hinh tuong quan trung binh den manh")
}else if(sqrt(R_mu2)>= 0.11 & sqrt(R_mu2)<= 0.25){
  cat ("mo hinh tuong quan yeu den trung  binh")
}else if(sqrt(R_mu2)>= 0.11 & sqrt(R_mu2)<= 0.10){
  cat ("mo hinh tuong quan yeu")
}else if(sqrt(R_mu2)==0){
  cat ("khong tuong quan")
}

#khoang tin cay doi xung hoi quy muc y nghia 95%
SE <- c(0)
for (i in 1:5){
  SE[i] <- sqrt(MSE*be_1[i,i])
}

# cho muc y nghia 5%
cat("H0: Beta1 = Beta2 = Beta3 = Beta4 = 0", "\n", "H1: ton tai Betaj # 0, j = 1,2,3,4", sep = "","\n")
f <- qf(p = 0.95, df1 = 4, df2 = 15-4-1)
fqs <- MSR/MSE
cat("Mien bac bo: Wa = (", round(f, 4), " ; ", Inf, ")", "\n")
if(fqs > f){
  cat("Vi fqs thuoc Wa => bac bo gia thuyet H0 .Chap nhan thuyet doi H1. Ton tai Betaj # 0, j = 1,2,3,4")
}else{
  cat("Vi fqs khong thuoc Wa => Chua co co so bac bo gia thuyet H0. Co the coi Beta1 = Beta2 = Beta3 = Beta4 = 0")
}

for (i in 1:4){
  tqs <- 0
  cat("H0: Beta", i, " = 0", "\n", "H1: Beta", i, " # 0", "\n", sep = "")
  k <- qt(p = 0.975, df = 15-4-1)
  cat("Mien bac bo gia thuyet H0: Wa = (", -Inf, " ; ", round(-k,4), ")", " U ", "(", round(k,4), " ; ", Inf, ")", "\n", sep = "")
  tqs <- (B_mu[i+1,1]-0)/SE[i]
  cat("Gia tri quan sat: tqs = ",round(tqs, 4), "\n", sep = "")
  if(i==1){
    if(tqs < -k || tqs > k){
      cat("tqs thuoc Wa => Bac bo gia thuyet H0, chap nhan thuyet doi H1 -> Thu nhap ho gia dinh co tac dong den so tien chi cho hoa qua.", "\n")
    }else{
      cat("tqs thuoc Wa => Chua co co so bac bo gia thuyet H0.", "\n")
    }
  }else if(i==2){
    if(tqs < -k || tqs > k){
      cat("tqs thuoc Wa => Bac bo gia thuyet H0,Chap nhan thuyet doi H1 -> Thu nhap ho gia dinh co tac dong den so tien chi cho vang.", "\n")
    }else{
      cat("tqs thuoc Wa => Chua co co co bac bo gia thuyet H0.", "\n")
    }
  }else if(i==3){
    if(tqs < -k || tqs > k){
      cat("tqs thuoc Wa => bac bo gia thuyet H0, chap nhan thuyet doi H1 -> Thu nhap ho gia dinh co tac dong den so tien chi cho ca.", "\n")
    }else{
      cat("tqs thuoc Wa => Chua co co so bac bo gia thuyet H0.", "\n")
    }
  }else if(i==4){
    if(tqs < -k || tqs > k){
      cat("tqs thuoc Wa => bac bo gia thuyet H0, chao nhan thuyet doi H1 -> Thu nhap ho gia dinh co tac dong den so tien chi cho thit.", "\n")
    }else{
      cat("tqs thuocc Wa => Chua co co so bac bo gia thuyet H0 H0.", "\n")
    }
  }
  cat("\n")
}


annova = function (dataList,alpha){
  total = 0
  average = 0
  totalLength = 0
  averageVector = c(0)
  totalVector = c(0)
  lengthVector = c(0)
  #Tinh trung binh mau x_i va trung binh mau x
  for (data in dataList) {
    totalData = sum(data)
    total = total + totalData
    totalLength = totalLength + length(data)
    totalVector = append(totalVector,totalData)
    averageVector = append(averageVector,totalData/length(data))
    lengthVector = append(lengthVector,length(data))
  }
  average = sum(totalVector)/totalLength
  totalVector = totalVector[-c(1)]
  averageVector = averageVector[-c(1)]
  lengthVector = lengthVector[-c(1)]
  print(lengthVector)
  #Tinh tong binh phuong chung SST
  SST = 0
  for (i in 1:length(dataList)) {
    for (j in 1:lengthVector[i]){
      SST = SST + (dataList[[i]][j] - average)^2
    }
  }
  print(SST)
  #Tinh tong binh phuong do nhan to SSF
  SSF = 0
  for (i in 1:length(dataList)) {
    SSF = SSF + (lengthVector[i])*(averageVector[i]-average)^2
  }
  print(SSF)
  #Tinh tong binh phuong do sai so
  SSE = SST -SSF
  #Tinh phuong sai do nhan to MSF
  MSF = SSF / (length(dataList)-1)
  #Tinh phuong sai do sai so
  MSE = SSE / (totalLength - length(dataList))
  #Tinh fqs 
  Fqs = MSF / MSE
  annovaTable = rbind(c(SSF,length(dataList)-1,MSF,0),c(SSE,totalLength - length(dataList),MSE,0),c(SST,totalLength - length(dataList) + length(dataList)-1,0,Fqs))
  print(annovaTable)
}

annova(list(x1,x2,x3))