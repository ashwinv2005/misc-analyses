seeds = readMat('C:/Ashwin/Matlab Workspaces/seeds.mat')
seedsh = readMat('C:/Ashwin/Matlab Workspaces/seedsh.mat')
x = 1:100
y = 1:100
g = expand.grid(x = x, y = y)
z =   as.vector(seedsh$seeds)
z[z == max(z)] = max(z)/4
g$z = z
newcols = colorRampPalette(c("grey90", "grey10"))
wireframe(z ~ x * y, data = g, zlab = list("Seed rain", rot = 96, cex = 1.5), xlab = list("x", cex = 1.5), ylab = list("y",cex = 1.5), drape = FALSE, colorkey = FALSE)
z1 = matrix(log(z), 100, 100)
persp3d(x, y, z1, col = 'skyblue')

ggsave(file = "C:/Ashwin/Figure 6.pdf", dpi = 2000)
