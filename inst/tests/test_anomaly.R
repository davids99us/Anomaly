 
test_that("wrapper runs on example code", {
  values=rnorm(400)+c(rep(-0.5,400/2),rep(0.5,400/2))
  o=sumseg(values,max.breaks=1)
  plot(index(values),values)
  lines(o$data$index,o$data$fitted,col=2,lwd=4)
  print(o$breaks) 
}) 