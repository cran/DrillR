#' To show whether the connection is active.
#'
#' @param object the RDrill object
#' @keywords RDrillGeneric 
#' @docType methods
#' @import httr
#' @name rd_active
#' @rdname rd_active
#' @exportMethod rd_active
#' @examples
#' #rd_active(R_DRILL_OBJECT) #please replace 'R_DRILL_OBJECT' with your RDrill object.
setGeneric("rd_active",function(object) standardGeneric("rd_active"))

#' @rdname rd_active
#' @aliases rd_active
setMethod("rd_active", 
          signature(object="drill"), 
          function(object) {
            tryCatch({
              data <- HEAD(paste("http://",object@host,":",object@port,"/",sep=""), verbose(), timeout(2))
              if(status_code(data) == 200)
              {
                return(TRUE)
              }else{
                return(FALSE)
              }
              
            },warning=function(w){
              print(w)
              return(FALSE)
            },error=function(e){
              print(e)
              return(FALSE)
            })
        })


#' Submit a SQL query and return results as a data frame.
#'
#' @param object the RDrill object
#' @param sql the query submitted
#' @keywords RDrillGeneric 
#' @import httr
#' @name rd_query
#' @rdname rd_query
#' @exportMethod rd_query
#' @examples
#' #rd_query(R_DRILL_OBJECT,SQL_QUERY) #please replace 'R_DRILL_OBJECT' with your RDrill object.
setGeneric("rd_query",function(object,sql) standardGeneric("rd_query"))

#' @rdname rd_query
#' @aliases rd_query
setMethod("rd_query", 
          signature(object="drill"), 
          function(object,sql) {
            tryCatch({
              data <- POST(paste("http://",object@host,":",object@port,"/query.json",sep=""), encode = "json", body = list(queryType='SQL',query=sql),verbose())
              data_df <- as.data.frame(do.call(rbind,content(data)$rows))
              if(status_code(data) == 200)
              {
                return(data_df)
              }else{
                return(data.frame())
              }
              
            },warning=function(w){
              print(w)
              return(data.frame())
            },error=function(e){
              print(e)
              return(data.frame())
            })
          })


#' Show the information of Drillbits, their ports and hosts.
#'
#' @param object the RDrill object
#' @keywords RDrillGeneric 
#' @import httr
#' @name rd_stats
#' @rdname rd_stats
#' @exportMethod rd_stats
#' @examples
#' #rd_stats(R_DRILL_OBJECT) #please replace 'R_DRILL_OBJECT' with your RDrill object.
setGeneric("rd_stats",function(object) standardGeneric("rd_stats"))

#' @rdname rd_stats
#' @aliases rd_stats
setMethod("rd_stats", 
          signature(object="drill"), 
          function(object) {
            tryCatch({
              data <- GET(paste("http://",object@host,":",object@port,"/stats.json",sep=""), encode = "json", verbose(), timeout(2))
              df_data <- as.data.frame(do.call(rbind,content(data)))
              colnames(df_data) = c('name','value')
              if(status_code(data) == 200)
              {
                return(df_data)
              }else{
                return(data.frame())
              }
              
            },warning=function(w){
              print(w)
              return(data.frame())
            },error=function(e){
              print(e)
              return(data.frame())
            })
          })


#' Show the information of the memory metrics.
#'
#' @param object the RDrill object
#' @keywords RDrillGeneric 
#' @import httr
#' @name rd_metrics
#' @rdname rd_metrics
#' @exportMethod rd_metrics
#' @examples
#' #rd_metrics(R_DRILL_OBJECT) #please replace 'R_DRILL_OBJECT' with your RDrill object.
setGeneric("rd_metrics",function(object) standardGeneric("rd_metrics"))

#' @rdname rd_metrics
#' @aliases rd_metrics
setMethod("rd_metrics", 
          signature(object="drill"), 
          function(object) {
            tryCatch({
              data <- GET(paste("http://",object@host,":",object@port,"/status/metrics",sep=""), encode = "json", verbose(), timeout(2))
              df_data <- as.data.frame(do.call(rbind,content(data)))
              if(status_code(data) == 200)
              {
                return(df_data)
              }else{
                return(data.frame())
              }
              
            },warning=function(w){
              print(w)
              return(data.frame())
            },error=function(e){
              print(e)
              return(data.frame())
            })
          })


#' Show the information of the threads status.
#'
#' @param object the RDrill object
#' @keywords RDrillGeneric 
#' @import httr
#' @name rd_threads
#' @rdname rd_threads
#' @exportMethod rd_threads
#' @examples
#' #rd_threads(R_DRILL_OBJECT) #please replace 'R_DRILL_OBJECT' with your RDrill object.
setGeneric("rd_threads",function(object) standardGeneric("rd_threads"))

#' @rdname rd_threads
#' @aliases rd_threads
setMethod("rd_threads", 
          signature(object="drill"), 
          function(object) {
            tryCatch({
              data <- GET(paste("http://",object@host,":",object@port,"/status/threads",sep=""), encode = "json", verbose(), timeout(2))
              df_data <- content(data)
              if(status_code(data) == 200)
              {
                return(df_data)
              }else{
                return(data.frame())
              }
              
            },warning=function(w){
              print(w)
              return(data.frame())
            },error=function(e){
              print(e)
              return(data.frame())
            })
          })


#' Show the information of the system and session options such as name and data type and so forth.
#'
#' @param object the RDrill object
#' @keywords RDrillGeneric 
#' @import httr
#' @name rd_options
#' @rdname rd_options
#' @exportMethod rd_options
#' @examples
#' #rd_options(R_DRILL_OBJECT) #please replace 'R_DRILL_OBJECT' with your RDrill object.
setGeneric("rd_options",function(object) standardGeneric("rd_options"))

#' @rdname rd_options
#' @aliases rd_options
setMethod("rd_options", 
          signature(object="drill"), 
          function(object) {
            tryCatch({
              data <- GET(paste("http://",object@host,":",object@port,"/options.json",sep=""), encode = "json", verbose(), timeout(2))
              df_data <- as.data.frame(do.call(rbind,content(data)))
              if(status_code(data) == 200)
              {
                return(df_data)
              }else{
                return(data.frame())
              }
              
            },warning=function(w){
              print(w)
              return(data.frame())
            },error=function(e){
              print(e)
              return(data.frame())
            })
          })


#' Show the configurations of the storage plugins.
#'
#' @param object the RDrill object
#' @keywords RDrillGeneric 
#' @import httr
#' @name rd_storages
#' @rdname rd_storages
#' @exportMethod rd_storages
#' @examples
#' #rd_storages(R_DRILL_OBJECT) #please replace 'R_DRILL_OBJECT' with your RDrill object.
setGeneric("rd_storages",function(object) standardGeneric("rd_storages"))

#' @rdname rd_storages
#' @aliases rd_storages
setMethod("rd_storages", 
          signature(object="drill"), 
          function(object) {
            tryCatch({
              data <- GET(paste("http://",object@host,":",object@port,"/storage.json",sep=""), encode = "json", verbose(), timeout(2))
              df_data <- as.data.frame(do.call(rbind,content(data)))
              if(status_code(data) == 200)
              {
                return(df_data)
              }else{
                return(data.frame())
              }
              
            },warning=function(w){
              print(w)
              return(data.frame())
            },error=function(e){
              print(e)
              return(data.frame())
            })
          })


#' Show the information about the running and completed queries.
#'
#' @param object the RDrill object
#' @keywords RDrillGeneric 
#' @import httr
#' @name rd_profiles
#' @rdname rd_profiles
#' @exportMethod rd_profiles
#' @examples
#' #rd_profiles(R_DRILL_OBJECT) #please replace 'R_DRILL_OBJECT' with your RDrill object.
setGeneric("rd_profiles",function(object) standardGeneric("rd_profiles"))

#' @rdname rd_profiles
#' @aliases rd_profiles
setMethod("rd_profiles", 
          signature(object="drill"), 
          function(object) {
            tryCatch({
              data <- GET(paste("http://",object@host,":",object@port,"/profiles.json",sep=""), encode = "json", verbose(), timeout(2))
              df_data <- df_res <- data.frame(matrix(unlist(content(data)), ncol=7,byrow = T))
              colnames(df_data) = c('query_id','time','location','foreman','query','state','user')
              if(status_code(data) == 200)
              {
                return(df_data)
              }else{
                return(data.frame())
              }
              
            },warning=function(w){
              print(w)
              return(data.frame())
            },error=function(e){
              print(e)
              return(data.frame())
            })
          })