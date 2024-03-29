#' @title Returns a variance-covariance matrix for a Cox proportional hazards model
#' @description This function returns a variance-covariance matrix  
#'	for a Cox proportional hazards model.
#' @details This is a function that returns a variance-covariance matrix for a fitted Cox 
#' 	proportional hazards model that has previously been generated on the server side.
#' 
#' Server function called: \code{vcovDS.coxph}. 
#' 
#' @param object character string (potentially including \code{*} symbol without spaces) 
#' specifying the name of the fitted server-side Cox proportioanl hazards model
#'	 that has been created using ds.coxphSLMAassign()
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#' For more information see \strong{Details}. 
#' @return \code{coxphSummaryDS} returns to the client-side the variance-covariance matrix of
#' 	the Cox proportional hazards model
#' @author Soumya Banerjee and Tom RP Bishop, 2021
#' @import DSI
#' @examples
#' \dontrun{
#'
#'   ## Version 1.0.0
#'   
#'   # connecting to the Opal servers
#' 
#'   require('DSI')
#'   require('DSOpal')
#'   require('dsBaseClient')
#'
#'   builder <- DSI::newDSLoginBuilder()
#'   builder$append(server = "study1", 
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "SURVIVAL.EXPAND_NO_MISSING1", driver = "OpalDriver")
#'   builder$append(server = "study2", 
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "SURVIVAL.EXPAND_NO_MISSING2", driver = "OpalDriver")
#'   builder$append(server = "study3",
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "SURVIVAL.EXPAND_NO_MISSING3", driver = "OpalDriver")
#'   logindata <- builder$build()
#'   
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
#'   
#'   # make sure that the outcome is numeric 
#'   ds.asNumeric(x.name = "D$cens",
#'             newobj = "EVENT",
#'             datasources = connections)
#'
#'   ds.asNumeric(x.name = "D$survtime",
#'             newobj = "SURVTIME",
#'             datasources = connections)
#'
#'   dsSurvivalClient::ds.Surv(time='SURVTIME', event='EVENT', objectname='surv_object')
#'
#'   dsSurvivalClient::ds.coxphSLMAassign(formula = 'surv_object ~  D$female', 
#'              dataName = 'D', datasources = connections, 
#'		objectname = 'coxph_serverside')
#'   
#'   dsSurvivalClient::ds.vcov.coxph(object = 'coxph_serverside')
#'
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#' }
#'
#' @export
ds.vcov.coxph <- function(object = NULL,
			   datasources = NULL
			   )
{
   
   # look for DS connections
   # if one not provided then get current
   if(is.null(datasources))
   {
      datasources <- DSI::datashield.connections_find()
   }
      
   # verify that 'object' name of Cox model was set
   if(is.null(object))
   {
      stop(" Please provide a valid name for a server-side Cox proportional hazards model that has been fitted to data !", call.=FALSE)
   }
   
   
   calltext <- call("vcovDS.coxph", object)

   # call aggregate function
   output <- DSI::datashield.aggregate(datasources, calltext)
  
   # return vcov matrix
   return(output)
	
}
#ds.vcov.coxph

