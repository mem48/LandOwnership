find_onedrive <- function(){
  op1 <- "C:/Users/malco/OneDrive - University of Leeds/Data"
  op2 <- "D:/OneDrive - University of Leeds/Data"
  op3 <- "E:/Users/earmmor/OneDrive - University of Leeds/Data"
  
  if(dir.exists(op1)){
    return(op1)
  } else if (dir.exists(op2)){
    return(op2)
  } else if (dir.exists(op3)){
    return(op3)
  } else {
    stop("Can't find OneDrive")
  }
}
