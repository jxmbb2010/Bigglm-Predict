predict.bigglmXM <- function(object, newdata, type = c("link", "response"),
                           se.fit = FALSE, make.function=FALSE, ...)
{
  type <- match.arg(type)
  
  intercept<-attr(object$terms,"intercept")!=0
  
  if(intercept && make.function)
  {
    switch(type,
           link =
           {
             fit <- function(x) cbind(1,x) %*% coef(object)
             se <- function(x) sqrt(diag(cbind(1,x) %*% vcov(object) %*% t(cbind(1,x))))
           },
           response =
           {
             fit <- function(x) family(object)$linkinv( cbind(1,x) %*% coef(object))
             se <- function(x)
             {
               temp <- cbind(1,x) %*% vcov(object) %*% t(cbind(1,x))
               temp <- sqrt(diag(temp) * (family(object)$mu.eta( cbind(1,x) %*% coef(object) ))^2)
               return(temp)
             }
           })
  }
  else
  {
    switch(type,
           link =
           {
             fit <- function(x) x %*% coef(object)
             se <- function(x) sqrt(diag(x %*% vcov(object) %*% t(x)))
           },
           response =
           {
             fit <- function(x) family(object)$linkinv( x %*% coef(object) )
             se <- function(x) sqrt(diag(x %*% vcov(object) %*% t(x)) * (family(object)$mu.eta(x %*%coef(object)))^2) 
           })
  }
  
  if (make.function) {
    if (se.fit)
      return(list(fit=fit, se=se))
    else
      return(fit)
  }
  
  newmf<-model.frame(object$terms,newdata)
  newmm<-model.matrix(object$terms,newmf)
  
  if (!se.fit) {
    ## No standard errors
    pred <- fit(newmm) 
  } else {
    fit <- fit(newmm)
    se <- se(newmm)
    pred<-list(fit=fit,se=se)
  }
  pred
}