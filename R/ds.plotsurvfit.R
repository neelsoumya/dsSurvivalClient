#' @title Performs plotting of privacy preserving survival curves. 
#' @description Performs plotting of privacy preserving survival curves. 
#' @details This is a function that plots privacy preserving survival curves.
#' 
#' Server function called: \code{plotsurvfitDS}. 
#' 
#' @param formula character string  
#' 	specifying the name of survfit object on the server-side created using ds.survfit().
#' For more information see \strong{Details}. 
#' @param dataName character string of name of data frame
#' @param fun optional parameter to have an argument. For example you can pass 'cloglog'
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#' @param method_anonymization an integer. Method of anonymization to be used (1: deterministic, 2: probabilistic). 
#'     Default value is 2.
#' @param noise an integer. fraction of noise (between 0 and 1) to be added to original data. 
#'     Noise is added as a percentage of original value. This is used for probabilistic anonymization.
#'     Default value is 0.03 
#' @param knn an integer. Number of nearest neighbours to be used for k nearest neighbours algoithm (for determinstic anonymization). 
#'     Default value is 20.
#' @return privacy preserving survival curve from the server side environment.
#' @author Soumya Banerjee, Demetris Avraam, Paul Burton, Xavier Escriba-Montagut, Juan Gonzalez and Tom RP Bishop (2021).
#' @examples
#' \dontrun{
#'
#'   ## Version 2.0
#'   
#'   # connecting to the Opal servers
#' 
#'   require('DSI')
#'   require('DSOpal')
#'   require('dsBaseClient')
#'   library(dsSurvivalClient)
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
#'   dsSurvivalClient::ds.coxph.SLMA(formula = 'surv_object ~  D$female', 
#'             dataName = 'D', datasources = connections)
#'
#'   dsSurvivalClient::ds.survfit(formula='surv_object~1', objectname='survfit_object')
#'
#'   dsSurvivalClient::ds.plotsurvfit(formula = 'survfit_object')
#'   
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#' }
#'
#' @export
ds.plotsurvfit <- function(formula = NULL,
                           dataName = NULL,
                           fun = NULL,
                           datasources = NULL,
                           method_anonymization = 2,
                           noise = 0.03,
                           knn = 20
			  )
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
  
  # call the server side function
  calltext <- call("plotsurvfitDS", formula=formula, dataName, method_anonymization, noise, knn)
  # calltext <- call("plotsurvfitDS", formula=formula, dataName) #, method_anonymization, noise, knn)	
  
  # call aggregate function
  output <- datashield.aggregate(datasources, calltext)
  
  # TODO: implement other arguments 
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
