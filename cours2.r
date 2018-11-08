sturges = function(n) {
  x<-floor(1 + log2(n))
  if (x < 5) {
    x<-5
  }
  if (x > 20) {
    x<-20
  }
  return(x)
}

histogram = function(x) {
  n<-length(x)
  k<-sturges(n)
  sorted<-sort(x)
  a_o<-sorted[1] - 0.025*(sorted[n] - sorted[1])
  a_k<-sorted[n] + 0.025*(sorted[n] - sortdataed[1]) + k
  h<-(a_k-a_o)/k
  
  bks<-seq(a_o, a_k, h)
  hist <- hist(sorted, prob=T, breaks=bks)
  
  lines(hist$mids, hist$density, lwd=2)
}

histogramPasVariable = function(x) {
  n<-length(x)
  k<-sturges(n)
  sorted<-sort(x)
  step<-n/k
  a<-0
  for (i in 1:k) {
    a<-c(a,sorted[i*step])
  }
  hist <- hist(sorted, prob=T, breaks=a)
  
  lines(hist$mids, hist$density, lwd=2)
}

testerExpo = function(data) {
  print('c\'est à peu près')
  plot(sort(data),log(1-seq(1:length(data)-1)/length(data)))
}

testNormale = function(data) {
  print('c\'est à peu près')
  plot(sort(data), qnorm(seq(1:length(data)-1)/length(data)))
}

testUniforme = function(data) {
  print('c\'est à peu près, hein')
  plot(sort(data), qunif(seq(1:length(data)-1)/length(data)))
}
