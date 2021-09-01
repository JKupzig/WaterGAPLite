#' @title Definition of Neighbouring Cells
#' @description Function to determine neighbouringh cells which is a pre-requesite for routeorder
#' @param basinObject basinObject that needs to be updated by informaiton of neighbouring cells
#' @return updated basinObject

basin.getNeighbourcells <- function(basinObject){
  
  #RowOrder:
  # 6 7 8
  # 4 - 5
  # 1 2 3
  array_size <- basinObject@array_size
  GCRC <- basinObject@GCRC
  GC <- basinObject@GC
  GR <- basinObject@GR
  #using GC and GR to find neighboruign cells
  NeighbourCells <- matrix(NA, nrow=8, ncol=array_size)
  
  for (i in 1:array_size){
    gcrc_ID <- GCRC[i]
    col_i <- GC[i]
    row_i <- GR[i]
    
    #finding positions 
    ID1 <- which((GC == col_i+1) & (GR == row_i-0))
    ID2 <- which((GC == col_i+1) & (GR == row_i-1))
    ID3 <- which((GC == col_i+0) & (GR == row_i-1))
    ID4 <- which((GC == col_i-1) & (GR == row_i-1))
    ID5 <- which((GC == col_i-1) & (GR == row_i-0))
    ID6 <- which((GC == col_i-1) & (GR == row_i+1))
    ID7 <- which((GC == col_i-0) & (GR == row_i+1))
    ID8 <- which((GC == col_i+1) & (GR == row_i+1))
    
    NeighbourCells[1, i] <- ifelse(length(ID1)==0, 0, ID1)
    NeighbourCells[2, i] <- ifelse(length(ID2)==0, 0, ID2)
    NeighbourCells[3, i] <- ifelse(length(ID3)==0, 0, ID3)
    NeighbourCells[4, i] <- ifelse(length(ID4)==0, 0, ID4)
    NeighbourCells[5, i] <- ifelse(length(ID5)==0, 0, ID5)
    NeighbourCells[6, i] <- ifelse(length(ID6)==0, 0, ID6)
    NeighbourCells[7, i] <- ifelse(length(ID7)==0, 0, ID7)
    NeighbourCells[8, i] <- ifelse(length(ID8)==0, 0, ID8)
  }
  
  basinObject@NeighbouringCells <- NeighbourCells
  #assign("NeighbouringCells", NeighbourCells , pos=".GlobalEnv") 
  
  return(basinObject)
  
}