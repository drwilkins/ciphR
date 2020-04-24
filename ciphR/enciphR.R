#nonShiny ciphR code for enciphering a string
# x= string to be encoded
# alg= algorithm for cipher/code (could be +/1 N for alphabetic shift)
# default output is converted vector
# if key=T, output will be list w/ converted text & key as items

enciphR<-function(x,alg,key=F){
  x.vec<-tolower(unlist(strsplit(x,fixed=T,split="")))#all lower case as vector
     
     #define new alphabet
      alphabet<-1:26+alg
      alphabet.shifted.idx<-sapply(alphabet,function(x) {if(x>26){x-26}else{ if(x<1){x+26}else{x}}})
      alphabet.shifted<-letters[alphabet.shifted.idx]
      
      keyMat=cbind(IN=letters,OUT=alphabet.shifted)
      
    #encipher
      x1.1<-as.vector(sapply(x.vec,function(s) {
         if(!s%in%letters){s}else{#If nonletter, leave it alone, else...
         keyMat[match(s,keyMat[,"IN"]),"OUT"]
         }},USE.NAMES = F))
      
      x2<-paste0(x1.1,collapse="")
  if(key){
    out<-list(IN=x,OUT=x2,KEY=keyMat)}
      else{
      out<-x2
      }
      return(out)
}

enciphR("Hello, my name is Simon3223",alg=+3,key=T)
      
