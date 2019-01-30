
#ransforming R scientific numbers into x times 10 to power LaTeX Output
as.scientific <- function(number){
  num <- format(number, scientific = TRUE, digits = 3)
  p <- as.integer(gsub(".*e+", "", num))
  b <- as.numeric(gsub( "e+.*", "", num))
  paste0('\\mathit{', b, '}', '\\times', '{',10, '}', '^', '{', p, '}')
}

#Anova BF print function
printBF <- function(BF, Hypothesis = 1, index = 1, OutputSize = 99999.99, HStyle = 0, N = FALSE, id = NULL){
  if(Hypothesis == "1" & as.vector(BF[index]) >= 1) return("\\linebreak  __BayesFactor larger 1, but Output for H1 selected__ \\linebreak ")
  if(Hypothesis == "0" & as.vector(BF[index]) < 1 & HStyle == 0) return("\\linebreak  __BayesFactor smaller 1, but Output for H0 selected__ \\linebreak ")
  s <- " "
  if(N == TRUE) s <- paste0("(", as.character(length(unique(aovBF5@data[, id]))), ") ")
  if(Hypothesis == "0") return(ifelse(as.vector(BF[index])>OutputSize, paste0('$\\mathit{BF}_{01}', s, '= ', as.scientific(as.vector(BF[index])), '$'), paste0('$\\mathit{BF}_{01}', s, '= ', printnum(as.vector(BF[index])), '$')))
  if(Hypothesis == "1") return(ifelse(1/as.vector(BF[index])>OutputSize, paste0('$\\mathit{BF}_{10}', s, '= ', as.scientific(1/as.vector(BF[index])), '$'), paste0('$\\mathit{BF}_{10}', s, '= ', printnum(1/as.vector(BF[index])), '$')))
}

#t test print function
printBFt <- function(BF, HStyle = 0, index = 1, OutputSize = 99999.99 , postit = 1000000, N = FALSE){
  if(as.vector(BF[index]) < 1 & HStyle == 0){
    b <- 1/as.vector(BF[index])
    num <- "01"
  }else{
    b <- as.vector(BF[index])
    num <- "10"
  }
  s <- " "
  if(N == TRUE) s <- paste0('(', nrow(BF@data), ') ') 
  if(as.character(class(BF@numerator[[names(BF@numerator)[index]]])) == "BFoneSample"){
    rBF <- BayesFactor::ttestBF(BF@data[,1], mu = BF@numerator[[names(BF@numerator)[index]]]@prior$mu, rscale = BF@numerator[[names(BF@numerator)[index]]]@prior$rscale)
  }
  if(as.character(class(BF@numerator[[names(BF@numerator)[1]]])) == "BFindepSample"){
    rBF <- BayesFactor::ttestBF(subset(BF@data, BF@data[,2] == "x")[,1] , subset(BF@data, BF@data[,2] == "y")[,1], rscale = BF@numerator[[names(BF@numerator)[index]]]@prior$rscale, paired = FALSE)
  }
  post <- BayesFactor::posterior(rBF, index = index, iterations = postit)
  d <- median(post[, "delta"])
  HDI <- coda::HPDinterval(post[, "delta"])
  ifelse(b > OutputSize, paste0('$\\mathit{BF}_{', num, '}', s, '= ', as.scientific(b), '$', ', ', '$d = ', printnum(d), '$', ', ', '95% HDI [', printnum(HDI[1]), ', ', printnum(HDI[2]), ']'), paste0('$\\mathit{BF}_{', num, '}', s, '= ', printnum(b), '$', ', ', '$d = ', printnum(d), '$', ', ', '95% HDI [', printnum(HDI[1]), ', ', printnum(HDI[2]), ']'))
}

#Function reading data from github (does not work for main Experiment, as there are more than 1000 data files in the folder and not all are shown on git)
read_gitdata <- function(page, extension){
  github_page <- read_html(page)
  file_nodes <- html_nodes(github_page, ".content .css-truncate-target .js-navigation-open")
  file_names <- html_text(file_nodes)
  file_url <- html_attr(file_nodes, "href")[grep(extension, file_names)]
  file_names <- gsub(".txt", "", file_names)
  file_url <- paste0("https://raw.githubusercontent.com", file_url)
  file_url <- gsub("blob/", "", file_url)
  #add data to a data frame
  data <- lapply(file_url, read.delim)
  data <- do.call("rbind", data)
  data
}


#calculate HDI and plotting plot
BFtHDIlow <- function(value, xpos = 1, postit = 100000){
  post <- BayesFactor::posterior(BayesFactor::ttestBF(value, mu = 0), iterations = postit)
  HDI <- coda::HPDinterval(post[, "mu"])
  arrows(x0 = xpos, y0 = mean(value), y1 = HDI[1], angle = 90, length = 0.1)
}
BFtHDIup <- function(value, xpos = 1, postit = 100000){
  post <- BayesFactor::posterior(BayesFactor::ttestBF(value, mu = 0), iterations = postit)
  HDI <- coda::HPDinterval(post[, "mu"])
  arrows(x0 = xpos, y0 = mean(value), y1 = HDI[2], angle = 90, length = 0.1)
}

BFtMean <- function(value, postit = 100000){
  mean(BayesFactor::posterior(BayesFactor::ttestBF(value, mu = 0), iterations = 10000)[, "mu"])
}