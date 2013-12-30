genGLCM <-
function(direction, distance, rawmat)
{
   #distance: 1=east(right)  2=south(down)  3=west(left)  4=north(up)
   number_coloums<-max(rawmat)+1
   number_rows   <-max(rawmat)+1

   #generate new GLCM-Matrix after calculation of size of GLCM
   #GLCM<-matrix(0, ncol=number_coloums, nrow=number_rows) 
   GLCM<-matrix(0, ncol=255, nrow=255) 
   (GLCM)

   #count occurrences and fill the matrix
   if(direction==1)   ### east-calculation
   {
     for(i in 1:number_coloums-1)   #for all coloumns 
     {
         for(a in 1:number_rows)    #for all rows
         {        
            GLCM[rawmat[a,i]+1,rawmat[a,i+1]+1]<-GLCM[rawmat[a,i]+1,rawmat[a,i+1]+1]+1
         } 
     }#eo for 
   }#eo east calculation 

   if(direction==2)   ### south-calculation
   {
     for(i in 1:number_coloums)   #for all coloumns 
     {
         for(a in 1:number_rows-1)    #for all rows
         {        
            GLCM[rawmat[a,i]+1,rawmat[a+1,i]+1]<-GLCM[rawmat[a,i]+1,rawmat[a+1,i]+1]+1
         } 
     }#eo for 
   }#eo east calculation 

   #add the matrix to its transponse to make it symmetrical
   transGLCM<-t(GLCM)
   print("INVERTIERT")
   GLCM<-GLCM+transGLCM

   #normalize the matrix to turn it into probabilities
   GLCMprob<-round(GLCM/sum(GLCM), digits=4)   

   return(GLCMprob)
}
