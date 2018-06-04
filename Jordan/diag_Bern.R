# Programa extraído do site: https://www.ime.usp.br/~giapaula/textoregressao.htm
# Créditos: Prof. Dr. Gilberto Alvarenga Paula
# Adaptado por Caio L. N. Azevedo

diagBern<-function(fit.model){
  # fit.model: objeto com o resultado do ajuste do MLG obtido através da função glm
  X <- model.matrix(fit.model)
  n <- nrow(X)
  p <- ncol(X)
  w <- fit.model$weights
  W <- diag(w)
  H <- solve(t(X)%*%W%*%X)
  H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
  h <- diag(H)
  ts <- resid(fit.model,type="pearson")/sqrt(1-h)
  td <- resid(fit.model,type="deviance")/sqrt(1-h)
  di <- (h/(1-h))*(ts^2)
  a <- max(td)
  b <- min(td)
  par(mfrow=c(2,2))
  plot(td,xlab="Indice", ylab="Residuo Componente do Desvio",
       ylim=c(b-1,a+1), pch=16,cex.axis=1.1,cex.lab=1.1,cex=1.1,cex.axis=1.1,cex.lab=1.1)
  abline(2,0,lty=2)
  abline(-2,0,lty=2)
  abline(0,0,lty=2)
  
  # identify(td, n=1)
  # title(sub="(c)")
  fited = fitted(fit.model)
  plot(fited ,td,xlab="valor ajustado (média)", ylab="Residuo Componente do Desvio",ylim=c(b-1,a+1), pch=16,
       main="",cex=1.1,cex.axis=1.1,cex.lab=1.1)
  abline(2,0,lty=2)
  abline(-2,0,lty=2)
  abline(0,0,lty=2)
  
  #
  hist(td,xlab="Resíduo Componente do Desvio",ylab="densidade",probability=TRUE,main="",cex=1.1,cex.axis=1.1,cex.lab=1.1)
  #
  eta = predict(fit.model)
  z = eta + resid(fit.model, type="pearson")/sqrt(w)
  plot(predict(fit.model),z,xlab="Preditor Linear",ylab="Variavel z", pch=16,main="",cex=1.1,cex.axis=1.1,cex.lab=1.1)
  lines(smooth.spline(predict(fit.model), z, df=2))
  #
  #---------------------------------------------------------------#
}