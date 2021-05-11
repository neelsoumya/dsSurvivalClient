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
#' @param weights vector of case weights
#' @param init vector of initial values of the iteration.
#' @param ties character string specifying the method for tie handling. The Efron approximation is
#'	used as the default. Other options are 'breslow' and 'exact'.
#' @param singular.ok logical value indicating how to handle collinearity in the model matrix.
#'	Default is TRUE. If TRUE, the program will automatically skip over columns of the X matrix
#'	that are linear combinations of earlier columns. In this case the coefficients of such
#'	columns will be NA and the variance matrix will contain zeros. 
#' @param model logical value. If TRUE, the model frame is returned in component model.
#' @param x logical value. If TRUE, the x matrix is returned in component x.
#' @param y logical value. If TRUE, the response vector is returned in component y.
#' @param control object of class survival::coxph.control() specifying iteration limit and 
#'		other control options. Default is survival::coxph.control()
#' @param combine_with_metafor logical If TRUE the
#' 	estimates and standard errors for each regression coefficient are pooled across
#' 	studies using random-effects meta-analysis under maximum likelihood (ML),
#' 	restricted maximum likelihood (REML) or fixed-effects meta-analysis (FE). Default is FALSE. 
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
                           weights = NULL,
                           init = NULL,
                           ties = 'efron',
                           singular.ok = TRUE,
                           model = FALSE,
                           x = FALSE,
                           y = TRUE,
                           control = NULL,
                           combine_with_metafor = FALSE,
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
    # TODO: cannot find function isDefined but is is inds.glmerSLMA
    # defined <- isDefined(datasources, dataName)
  }
  
  # verify that 'formula' was set
  if(is.null(formula))
  {
    stop(" Please provide a valid survival formula!", call.=FALSE)
  }
  
  
  calltext <- call("plotsurvfitDS", formula=formula, dataName) #, weights, init, ties, singular.ok, model, x, y, control)
  
  # call aggregate function
  output <- datashield.aggregate(datasources, calltext)
  
  # TODO: do for each study by using colnames which will have study1 etc	
  # TODO: other arguments like fun
  #	https://www.rdocumentation.org/packages/survival/versions/3.2-7/topics/plot.survfit
  fun = NULL
  
  if (is.null(fun))
  {	   
    graphics::plot(output$study1, main = 'Survival curve of anonymized data')	
  }
  else
  {
    graphics::plot(output$study1, main = 'Survival curve of anonymized data', fun = fun)	   
  }	   
  
  return(output)
  
}
#ds.plotsurvfit
