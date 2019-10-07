drawSpiderDiagram <- function (data,colXY,outLSCP,coverage.mat){
  # a function to make a spider diagram for LCSP 
  # inputs: 
  #       - data: a data frame with x, y coordinates and id
  #       - colXY: index of x, y, and id
  #       - colXY: index of x, y, and id
  #       - outLCSP: output of LCSP with binary outputs
  #       - coverage.mat: a binary coverage matrix
  #
  # Written by Enki Yoo
  # 09/20/2016
  #
  ix = colXY[1]
  iy = colXY[2]
  iz = colXY[3]
  plot(data[,ix],data[,iy],cex=.5,xlab='Lattitude',ylab='Longitude')
  text(data[,ix],data[,iy],data[,iz],cex=.8,pos=4,
       xlab='Lattitude',ylab='Longitude')
  id = which(outLSCP == 1)
  points(data[id,ix],dat[id,iy],col=2,pch=1,cex=1.5)
  for (i in id){
    j = which(coverage.mat[i,] == 1)
    p1 = data[i,c(ix,iy)];
    p2 = data[j,c(ix,iy)];
    np2 = nrow(p2)
    p1.list = p1[rep(seq_len(nrow(p1)), each=np2),]
    segments(p1.list[,1],p1.list[,2],p2[,1],p2[,2],col='red')
  }
}


