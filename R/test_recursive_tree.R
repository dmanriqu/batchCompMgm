gr <- list(
  A = c('x'),
  B = c('x', 'y', 'z'),
  x = c('C'),
  D = c('C'),
  C = c()
)

desc <- function(l, node, check = node){
  a <- l[[node]]
  if (check %in% a){
    stop('circular reference!')
  }
  li <- c()
  if (is.null(a)) return(NULL)
  for (n in a){
    r <- desc(l, n, node)
    li <- append(li,r)
  }
  return(unique(append(li, a)))
}
desc(gr, 'C')

asc1(gr, 'C')
downtheline <- function(l, node, check = node){
  asc1 <- function(lst, node){
    #WHich nodes list 'node' as a requisite
    res <- as.character()
    for (i in seq_along(gr)){
      if (node %in% gr[[i]]) {
        res <- append(names(gr[i]), res)
      }
    }
    return(res)
  }
  a <- asc1(gr, node)
  if (check %in% a){
    stop('circular reference!')
  }
  li <- c()
  if (is.null(a)) return(NULL)
  for (n in a){
    r <- downtheline(l, n, node)
    li <- append(li,r)
  }
  return(unique(append(li, a)))
}
downtheline(gr, node = 'C')
