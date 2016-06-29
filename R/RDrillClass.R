#' Class \code{drill} defines a Apache Drill driver class.
#'
#' @slot host A string denoting the Apache Drill cluster/node host
#' @slot port A numeric denoting the Apache Drill cluster/node port
setClass("drill",slots = list(host="character", port="numeric"))


#' Get the Apache Drill Connection, return the RDrill object.
#'
#' @param host Apache Drill cluster/node host
#' @param port Apache Drill cluster/node port
#' @keywords RDrillClass
#' @import httr
#' @import methods
#' @name rdrill
#' @rdname rdrill
#' @exportMethod rdrill
#' @examples
#' rdrill("127.0.0.1",8047)
setGeneric("rdrill",function(host, port) standardGeneric("rdrill"))

#' @rdname rdrill
#' @aliases rdrill
setMethod("rdrill", signature=c(host="character", port="numeric"), 
          function(host, port){
            if(missing(host)){
              host = '127.0.0.1'
            }
            if(missing(port)){
              port = 8047
            }
            new("drill", host=host, port=port)
          })