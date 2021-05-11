#' @title Performs plotting of privacy preserving survival curves. 
#' @description Performs plotting of privacy preserving survival curves. 
#' @details This is a function that performs plotting of privacy preserving survival curves.
#' 
#' Server function called: \code{plotsurvfitDS}. 
#' 
#' @param formula character string  
#' 	specifying the name of survfit object on the server-side created using ds.survfit().
#' For more information see \strong{Details}. 
#' @param dataName character string of name of data frame
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#' @return privacy preserving survival curve from the server side environment.
#' @author Soumya Banerjee, Tom Bishop, Demetris Avraam, Paul Burton and DataSHIELD technical team (2021).
#' @examples
#' \dontrun{
#'
#'   ## Version 6
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
#'   dsBaseClient::ds.Surv(time='SURVTIME', event='EVENT', objectname='surv_object')
#'
#'   dsBaseClient::ds.coxph.SLMA(formula = 'surv_object ~  D$female', 
#'             dataName = 'D', datasources = connections)
#'   
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#' }
#'
#' @export
ds.plotsurvfit <- function(formula = NULL,
                           dataName = NULL,
                           fun = NULL,
                           datasources = NULL)
{
  
  # look for DS connections
  # if one not provided then get current
  if(is.null(datasources))
  {
    datasources <- datashield.connections_find()
  }
  
  # if the argument 'dataName' is set, check that the data frame is defined (i.e. exists) on the server site
  if(!(is.null(dataName)))
  {
    defined <- dsBase:::isDefined(datasources, dataName)
  }
  
  # verify that 'formula' was set
  if(is.null(formula))
  {
    stop(" Please provide a valid survival formula!", call.=FALSE)
  }
  
  
  calltext <- call("plotsurvfitDS", formula=formula, dataName) #, weights, init, ties, singular.ok, model, x, y, control)
  
  # call aggregate function
  output <- datashield.aggregate(datasources, calltext)
  
  # TODO: other arguments 
  #	https://www.rdocumentation.org/packages/survival/versions/3.2-7/topics/plot.survfit
  
  # Get the required grid according to the number of servers
  nrows_plot <- ceiling(length(output) / 2)
  if(nrows_plot != 1){par(mfrow=c(nrows_plot, 2))}
  # Plot for each server
  Map(function(x, n) {
    funct <- eval("fun")
    if (is.null(fun)){	
      funct <- rlang::missing_arg()
    }
    graphics::plot(x, 
                   main = paste0('Survival curve of anonymized data \n [', n, ']'),
                   fun = funct)
  }, output, names(output))
  # Reset graphic options to not interfere other plots
  par(mfrow=c(1,1))
  
  return(output)
  
}
#ds.plotsurvfit
