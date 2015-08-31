contact <- function(x , y = NULL){
    library(xlsx)
    library(data.table)
    contact.xlsx <- read.xlsx("contact.xlsx" , sheetIndex = 1 , header = TRUE)
    contact.dt <- as.data.table(contact.xlsx)
    setkey(contact.dt ,老社员名单)
    find.target <- contact.dt[x]
    detail <- as.matrix(find.target)
    if(is.na(detail[1,1])){
        stop("name not found")
    }
    if(!is.null(y)){
        transform.matrix <- as.matrix(find.target)
        if(y == "instrument"){
            print(transform.matrix[,c(1,2)])
        }else if(y == "major"){
            print(transform.matrix[,c(1,3)])
        }else if(y == "year"){
            print(transform.matrix[,c(1,4)])
        }else if(y == "number"){
            print(transform.matrix[,c(1,5)])
        }else if(y == "email"){
            print(transform.matrix[,c(1,6)])
        }else if (y == "else"){
            print(transform.matrix[,c(1,7)])
        }
    }else if(is.null(y)){
        print(find.target)
    }
}
