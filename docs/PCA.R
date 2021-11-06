#======== PCA =========== 
cor2cov <- function(R,S){
    diag(S) %*% R %*% diag(S)
} # fungsi konversi matriks korelasi ke kovarian
s <- cor2cov(r,s) # contoh
# Buat matriks jika diketahui r ----
r <- tibble::tribble(
         ~`1`, ~`2`, ~`3`, ~`4`, ~`5`, ~`6`,
           1L, 0.88, 0.96, 0.94,  0.9,  0.9,
           NA,    1,  0.9, 0.91, 0.94, 0.95,
           NA,   NA,    1, 0.96, 0.93, 0.92,
           NA,   NA,   NA,    1, 0.93, 0.94,
           NA,   NA,   NA,   NA,    1, 0.95,
           NA,   NA,   NA,   NA,   NA,    1
         )

r <- as.matrix(r)
r[lower.tri(r)] = t(x)[lower.tri(r)]
r
# lakukan PCA jika diketahui korelasi
p <- ncol(r)
eig.val <- eigen(r)$value
eig.vect <- eigen(r)$vectors
stdev <- sqrt(eig.val) # standar deviasi didapat dari akar nilai eigen
tot.var <- sum(eig.val) # varian total didapat dari jumlah nilai eigen
prop.var <- eig.val/tot.var # proporsi varian
cum.var <- c(rep(NA,p)) # varian komulatif
x.temp <- 0
for (i in 1:p){
    x.temp <- x.temp+prop.var[i]
    cum.var[i] <- x.temp
}

variabel <- c(1:p)
vect.name <- c()
for ( i in 1:p){ # Membuat nama komponen
    vect.name[i] <- paste0("Comp.",i) 
}
eig.vec <- as.data.frame(eig.vect)
names(eig.vec) <- vect.name
# pembuatan tabel std,prop.varians,dan prop.varians komulatif.
tabel.ic <- rbind(eig.val,stdev,prop.var,cum.var)
tabel.ic <- as.data.frame(tabel.ic)
row.names(tabel.ic) <- c("Eigen value","Standar deviation",
                         "Proportion of Variance",
                         "Cumulative Proportion")
colnames(tabel.ic) <- vect.name
loadings <- data.frame(variabel,round(eig.vec,5)) # Membuat tabel Loadings
row.names(loadings) <- loadings$variabel
loadings$variabel <- NULL
hasil.pca <- function(){ # membuat fungsi untuk menampilkan hasil PCA
    writeLines("Importance of components:")
    print(tabel.ic)
    writeLines("")
    writeLines("Loadings:")
    print(loadings)
}
# Hasil PCA ----
hasil.pca() # Cetak hasil PCA Manual

# PCA princomp----
princomp(data.awal,cor=F)

