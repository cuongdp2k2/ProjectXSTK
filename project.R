#1
dirty_data <- read.csv("C:/Users/C/OneDrive/Desktop/dirty_data.csv")
str(dirty_data)

#2
new_data <- dirty_data[,c("nearest_warehouse","order_price","delivery_charges",
                          "coupon_discount","order_total","season",
                          "is_expedited_delivery",
                          "distance_to_nearest_warehouse","is_happy_customer")]
head(new_data ,10)

apply(is.na(new_data),2,which)

unique(new_data$nearest_warehouse)
new_data$nearest_warehouse[new_data$nearest_warehouse == 'thompson'] <- 'Thompson'
new_data$nearest_warehouse[new_data$nearest_warehouse == 'nickolson'] <- 'Nickolson'
unique(new_data$nearest_warehouse)
unique(new_data$season)
new_data$is_expedited_delivery[new_data$is_expedited_delivery == 'True'] <- 1
new_data$is_expedited_delivery[new_data$is_expedited_delivery == 'False'] <- 0
new_data$is_happy_customer[new_data$is_happy_customer == 'True'] <- 1
new_data$is_happy_customer[new_data$is_happy_customer == 'False'] <- 0
new_data$is_expedited_delivery<-as.numeric(new_data$is_expedited_delivery)
new_data$is_happy_customer<-as.numeric(new_data$is_happy_customer)

#3

des_function <- function(x) {c(mean(x),median(x),sd(x),min(x),max(x))}
des_table <-apply(new_data[,c("order_price","delivery_charges",
                              "coupon_discount","order_total",
                              "distance_to_nearest_warehouse")],2,des_function)
rownames(des_table)=c("mean","median","sd","min","max")
des_table

table(new_data$nearest_warehouse)

table(new_data$season)

table(new_data$is_expedited_delivery)

table(new_data$is_happy_customer)

barplot(table(new_data$nearest_warehouse),main="Plot of nearest_warehouse")

pie(table(new_data$nearest_warehouse),main="Plot of nearest_warehouse")

barplot(table(new_data$is_happy_customer),main="Plot of is_happy_customer")

pie(table(new_data$is_happy_customer),main="Plot of is_happy_customer")

hist(new_data$order_total,main="Histogram of order_total")

boxplot(order_total~nearest_warehouse,new_data)

rm.out <- function(x, na.rm = TRUE, ...){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm) 
  y <- x
  y[x < (qnt[1] - H)] <- NA 
  y[x > (qnt[2] + H)] <- NA 
  y
}

Bakers_data = subset(new_data,new_data$nearest_warehouse =="Bakers")
Bakers_data$order_total = rm.out(Bakers_data$order_total)

Nickolson_data = subset(new_data,new_data$nearest_warehouse =="Nickolson")
Nickolson_data$order_total = rm.out(Nickolson_data$order_total)

Thompson_data = subset(new_data,new_data$nearest_warehouse =="Thompson")
Thompson_data$order_total = rm.out(Thompson_data$order_total)

new_data_2 <- rbind(Bakers_data,Nickolson_data,Thompson_data)  

apply(is.na(new_data_2),2,sum)  
apply(is.na(new_data_2),2,mean)  

new_data_2<-na.omit(new_data_2)

boxplot(order_total~nearest_warehouse,new_data_2)

boxplot(order_total~season,new_data)

Spring_data = subset(new_data,new_data$season =="Spring")
Spring_data$order_total = rm.out(Spring_data$order_total)

Summer_data = subset(new_data,new_data$season =="Summer")
Summer_data$order_total = rm.out(Summer_data$order_total)

Autumn_data = subset(new_data,new_data$season =="Autumn")
Autumn_data$order_total = rm.out(Autumn_data$order_total)

Winter_data = subset(new_data,new_data$season =="Winter")
Winter_data$order_total = rm.out(Winter_data$order_total)

new_data_3 <- rbind(Spring_data,Summer_data,Autumn_data,Winter_data)  

apply(is.na(new_data_3),2,sum)  
apply(is.na(new_data_3),2,mean) 

new_data_3<-na.omit(new_data_3)

boxplot(order_total~season,new_data_3)

Bakers_data_2<-subset(new_data_2,new_data_2$nearest_warehouse =="Bakers")
qqnorm(Bakers_data_2$order_total)
qqline(Bakers_data_2$order_total)
shapiro.test(Bakers_data_2$order_total)

Nickolson_data_2<-subset(new_data_2,new_data_2$nearest_warehouse =="Nickolson")
qqnorm(Nickolson_data_2$order_total)
qqline(Nickolson_data_2$order_total)
shapiro.test(Nickolson_data_2$order_total)

Thompson_data_2<-subset(new_data_2,new_data_2$nearest_warehouse =="Thompson")
qqnorm(Thompson_data_2$order_total)
qqline(Thompson_data_2$order_total)
shapiro.test(Thompson_data_2$order_total)

library(car)
leveneTest(order_total~as.factor(nearest_warehouse),data=new_data_2)

anova_model_1 <- aov(order_total~nearest_warehouse,data=new_data_2)
summary(anova_model_1)

Spring_data_2<-subset(new_data_3,new_data_3$season =="Spring")
qqnorm(Spring_data_2$order_total)
qqline(Spring_data_2$order_total)
shapiro.test(Spring_data_2$order_total)

Summer_data_2<-subset(new_data_3,new_data_3$season =="Summer")
qqnorm(Summer_data_2$order_total)
qqline(Summer_data_2$order_total)
shapiro.test(Summer_data_2$order_total)

Autumn_data_2<-subset(new_data_3,new_data_3$season =="Autumn")
qqnorm(Autumn_data_2$order_total)
qqline(Autumn_data_2$order_total)
shapiro.test(Autumn_data_2$order_total)

Winter_data_2<-subset(new_data_3,new_data_3$season =="Winter")
qqnorm(Winter_data_2$order_total)
qqline(Winter_data_2$order_total)
shapiro.test(Winter_data_2$order_total)

leveneTest(order_total~as.factor(season),data=new_data_3)

anova_model_2 <- aov(order_total~season,data=new_data)
summary(anova_model_2)

xtabs( ~ nearest_warehouse+is_happy_customer, data=new_data)
table <- xtabs( ~ nearest_warehouse+is_happy_customer, data=new_data)
chisq.test(table)

chisq.test(table)$expected

new_data_4<-new_data[,c("nearest_warehouse","coupon_discount",
                        "order_total","season","is_expedited_delivery",
                        "is_happy_customer")]
head(new_data_4,10)

set.seed(123)
train.rows <- sample(rownames(new_data_4), dim(new_data_4)[1]*0.8)
train_data <- new_data_4[train.rows, ]
test.rows <- setdiff(rownames(new_data_4), train.rows)
test_data <- new_data_4[test.rows, ]

model <- glm(is_happy_customer~., family="binomial", data=train_data)
reseach=step(model)

summary(reseach)

library(rpart)
library(rpart.plot)
fit <- rpart(is_happy_customer~., data = train_data, method = 'class')
rpart.plot(fit, extra = 106)

test_data$predicted <-predict(fit, test_data, type = 'class')
head(test_data,10)

Observation<-table(test_data$is_happy_customer)
Predicted_value<-table(test_data$predicted)
rbind(Observation,Predicted_value)

library(InformationValue)
misClassError(test_data$is_happy_customer, test_data$predicted)

#uoc luong order_total ---------------------------------------------------

dirty_data <- read.csv("C:/Users/C/OneDrive/Desktop/dirty_data.csv")
str(dirty_data)

new_data <- dirty_data[,c("nearest_warehouse","order_price","delivery_charges",
                          "coupon_discount","order_total","season",
                          "is_expedited_delivery",
                          "distance_to_nearest_warehouse","is_happy_customer")]
head(new_data ,10)

apply(is.na(new_data),2,which)

unique(new_data$nearest_warehouse)
new_data$nearest_warehouse[new_data$nearest_warehouse == 'thompson'] <- 'Thompson'
new_data$nearest_warehouse[new_data$nearest_warehouse == 'nickolson'] <- 'Nickolson'
unique(new_data$nearest_warehouse)
unique(new_data$season)
new_data$is_expedited_delivery[new_data$is_expedited_delivery == 'True'] <- 1
new_data$is_expedited_delivery[new_data$is_expedited_delivery == 'False'] <- 0
new_data$is_happy_customer[new_data$is_happy_customer == 'True'] <- 1
new_data$is_happy_customer[new_data$is_happy_customer == 'False'] <- 0
new_data$is_expedited_delivery<-as.numeric(new_data$is_expedited_delivery)
new_data$is_happy_customer<-as.numeric(new_data$is_happy_customer)

n<-length(new_data$order_price)
xtb<-mean(new_data$order_price)
s<-sd(new_data$order_price)
data.frame(n,xtb,s)

#kiem tra phan phoi chuan
qqnorm(new_data$order_price)
qqline(new_data$order_price)


shapiro.test(new_data$order_price)

E = qnorm(p=.05/2,lower.tail = FALSE)*s/sqrt(n)
qnorm(p=.05/2,lower.tail = FALSE)
E

data.frame(Left_CI = xtb-E , Right_CI = xtb+E)

# truong hop phan phoi chuan
Est = qt(p=.05/2, df=n-1 , lower.tail = FALSE)*s/sqrt(n)
Est

data.frame(Left_CI = xtb - Est , Right_CI = xtb + Est)

#2 mau
new_data$Group <- ifelse(new_data$is_expedited_delivery == 1 , "Group_1" ,"Group_2")
Group_1_data <- subset(new_data,Group=="Group_1")
Group_2_data <- subset(new_data,Group=="Group_2")

n1<-length(Group_1_data$order_price)
xtb1<-mean(Group_1_data$order_price)
s1<-sd(Group_1_data$order_price)

n2<-length(Group_2_data$order_price)
xtb2<-mean(Group_2_data$order_price)
s2<-sd(Group_2_data$order_price)

data.frame(n1,xtb1,s1,n2,xtb2,s2)

# kiem tra pp chuan Group_1
qqnorm(Group_1_data$order_price)
qqline(Group_1_data$order_price)

shapiro.test(Group_1_data$order_price)

# kiem tra pp chuan Group_2
qqnorm(Group_2_data$order_price)
qqline(Group_2_data$order_price)

shapiro.test(Group_2_data$order_price)

z0 = (xtb1 - xtb2)/sqrt(s1^2/n1 + s2^2/n2)
z0

qnorm(p=.01, lower.tail = FALSE)

var.test(Group_1_data$order_price,Group_2_data$order_price,alternative = "less")

