# X1 <- rnorm(3, 0, 1)
# X2 <- rnorm(7, 1, 0.3)

# X3 <- c(X1, X2)
hcv = 0.1622122
X3 <- c(-1.8860550, 0.2352393, 1.1899925, 0.2270021, 1.0713954, 1.0281517, 0.9381665, 0.9329905, 0.8782543, 1.1614841)
print(X3)

intF <- function(x) {
	sNor = 0.3 * dnorm(x)
	nsNor = 0.7 * dnorm(x, 1, 0.3)
	return (sNor + nsNor)
}

intHatF <- function(x, X, h) {
	n = length(X)
	sum = 0
	for (val in X) {
	    if ((x - h <= val) && (val <= x + h))
	    {
	    	sum <- sum + 1
	    }
	}
	return (sum / (2 * h * n))
}

png(file = "result.jpg")

# 生成自变量序列
x = seq(-2, 2.5, length=1000)
# x = c(0.0001, 0.001, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 10)
# x = c(0.0001, 0.001, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5)

# 生成因变量序列
y1 = rep(0, length(x))
j = 1
for (i in x) {
    y1[j] = intF(i)
    j = j + 1
}
y2 = rep(0, length(x))
j = 1
for (i in x) {
    y2[j] = intHatF(i, X3, hcv)
    j = j + 1
}

# 绘图
plot(x, y1, type = "l", col="#f0932b", ylab = "Density", lwd=2, ylim = c(0,2.5), xlim = c(-2,2.5))
lines(x, y2, lwd=2, col="#4834d4")
legend("topright", c("X~f", "X~hatF"), col = c("#f0932b", "#4834d4"), lty = c(1),text.font = 12)