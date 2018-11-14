library("png")
library("grid")
library("gridExtra")

numeroCores = 15

imagem = readPNG(file.choose())
grid.newpage()
grid.raster(imagem)

# arranjar imagem num data frame
df = data.frame(
  red = matrix(imagem[,,1], ncol=1),
  green = matrix(imagem[,,2], ncol=1),
  blue = matrix(imagem[,,3], ncol=1)
)

kMeansResult = kmeans(df, numeroCores)
df$label = kMeansResult$cluster

colors = data.frame(
  label = 1:nrow(kMeansResult$centers),
  R = kMeansResult$centers[,"red"],
  G = kMeansResult$centers[,"green"],
  B = kMeansResult$centers[,"blue"]
)

df$order = 1:nrow(df)
df = merge(df, colors)
df = df[order(df$order),]
df$order = NULL

# get mean color channel values for each row of the df.
R = matrix(df$R, nrow=dim(imagem)[1])
G = matrix(df$G, nrow=dim(imagem)[1])
B = matrix(df$B, nrow=dim(imagem)[1])

# reconstitute the segmented image in the same shape as the input image
imagem.segmented = array(dim=dim(imagem))
imagem.segmented[,,1] = R
imagem.segmented[,,2] = G
imagem.segmented[,,3] = B
if (dim(imagem)[3] == 4) { # copiar o canal alpha, se houver
  imagem.segmented[,,4] = imagem[,,4]
}

# View the result
grid.newpage()
grid.raster(imagem.segmented)

library("rgl")
# color space plot of image
open3d()
plot3d(df$red, df$green, df$blue, 
       col=rgb(df$red, df$green, df$blue),
       xlab="R", ylab="G", zlab="B",
       size=3, box=FALSE, axes=TRUE)
#play3d( spin3d(axis=c(1,1,1), rpm=3), duration = 10 )

# color space plot of segmented image
open3d()
plot3d(df$red, df$green, df$blue, 
       col=rgb(df$R, df$G, df$B),
       xlab="R", ylab="G", zlab="B",
       size=3, box=FALSE)
#play3d( spin3d(axis=c(1,1,1), rpm=3), duration = 10 )
