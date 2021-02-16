# X1 <- rnorm(3, 0, 1)
# X2 <- rnorm(7, 1, 0.3)

# X3 <- c(X1, X2)
X3 <- c(-1.8860550, 0.2352393, 1.1899925, 0.2270021, 1.0713954, 1.0281517, 0.9381665, 0.9329905, 0.8782543, 1.1614841)
print(X3)

myCV <- function(h, X) {
	conSum = 0
	curN = length(X)
    for (i in 1:curN) 
    {
    	for (j in 1:curN) 
	    {
	    	curT <- (X[j] - X[i]) / h
	    	curNum <- 0
	    	if ((-2 <= curT) && (curT <= 0))
	    	{
			   curNum <- (curT + 2) / 4
			}
			else if ((0 < curT) && (curT <= 2))
			{
				curNum <- (2 - curT) / 4
			}
			conSum <- conSum + curNum
			# cat (i, j, "t: ", curT, "curNum: ", curNum, "\n")
		}
	}
	conSum <- conSum / (curN^2 * h)
	kSum = 0
    for (i in 1:curN) 
    {
    	for (j in 1:curN) 
	    {
	    	if (j == i)
	    	{
	    		next
	    	}
	    	curT <- (X[j] - X[i]) / h
	    	curNum <- 0
	    	if ((-1 <= curT) && (curT <= 1))
	    	{
			   curNum <- 0.5
			}
			kSum <- kSum + curNum
		}
	}
	kSum <- -2 * kSum / (curN * (curN - 1) * h)
	# cat ("conSum: ", conSum, "\n")
	# cat("h: ", h, "val: ", conSum + kSum, "\n")
	return (conSum + kSum)
}

png(file = "result.jpg")

# 生成自变量序列
x = seq(0.05, 1, length=1000)
# x = c(0.0001, 0.001, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 10)
# x = c(0.0001, 0.001, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5)

# 生成因变量序列
minVal = 1
minId = 0
y = rep(0, length(x))
j = 1
for (i in x) {
    y[j] = myCV(i, X3)
    if (minVal > y[j])
    {
    	minVal = y[j]
    	minId = j
    }
    j = j + 1
}

cat ("minVal: ", minVal, "\n")
cat ("minId: ", minId, "\n")
cat ("x[minId]: ", x[minId], "\n")
# 绘制图像
plot(x, y, type='l')
# plot(1:length(x), y, type='l')