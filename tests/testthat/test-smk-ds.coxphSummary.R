#-------------------------------------------------------------------------------
# Copyright (c) 2019-2020 University of Newcastle upon Tyne. All rights reserved.
#
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#
# Set up
#

context("ds.coxphSummary::smk::setup")

# connect.studies.dataset.cnsim(list("LAB_TSC"))
# load survival expand no missing data
connect.studies.dataset.survival_nomissing(list("cens","survtime","time.id","female","age.60","starttime","endtime"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#connect.studies.dataset.survival(list("D"))
##init.studies.dataset.survival(list("D"))
#init.studies.dataset.survival_nomissing(list("D"))
##connect.studies.dataset.dasim(c("SURVTIME"))

######################################
# add server side survival variables
######################################
# add survival related server side variables like SURVTIME, etc.
#   need to convert these to numeric and create server side
#   variables
ls_object <- add_server_side_var_survival()
# snure that objects have been added
print(ls_object)

#
# Tests
#

              
context("ds.coxphSummary::smk")
test_that("simple error,wrong formula", {
    
    #try( cox_object <- ds.coxph.SLMA(formula = 'survival::Surv(time=SURVTIME,event=EVENT)~D$age.60')#, dataName = 'D') 
    #, silent = FALSE)
    
    dsBaseClient::ds.Surv(time='STARTTIME', time2='ENDTIME', event = 'EVENT', objectname='surv_object', type='counting')
    #try(
    # cox_object <- ds.coxph.SLMA(formula = 'surv_object~AGE')#, dataName = 'D')
    #, silent=FALSE)
    # print(cox_object$study1$call)
    # print("coeff from simple model")
    # print(cox_object$study1$coefficients[1,1])
    
    # print( datashield.errors() )
    
    # summary(cox_object)
    
    # expect_error( as.character(  ds.coxph.SLMA(formula = 'survival::Surv(time=SURVTIME,event=EVENT)~D$age', dataName = 'D')   ) )
    
    # wrong formula
    expect_error( as.character(  ds.coxphSLMAassign(formula = 'survival::Surv(time=SURVTIME,event=EVENT)=D$age', dataName = 'D', objectname = 'surv_server')   ) )
    
})



context("ds.coxphSummary::smk")
test_that("simple test, checking coefficients of diagnostics", {
        
    dsBaseClient::ds.Surv(time='STARTTIME', time2='ENDTIME', event = 'EVENT', objectname='surv_object', type='counting')
    
    dsBaseClient::ds.coxphSLMAassign(formula = 'surv_object~AGE', objectname = 'cox_object_serverside')
    
    dsBaseClient::ds.cox.zphSLMA(fit = 'cox_object_serverside')
    
    dsBaseClient::ds.coxphSummary(x = 'cox_object_serverside')
    
    # expect_equal(coxph_model_full$survival1$coefficients[1], 0.0387, tolerance = 0.0001)
    
    
})



#
# Done
#

context("ds.coxphSummary::smk::shutdown")

#test_that("shutdown", {
#    ds_expect_variables(c("D"))
#})

disconnect.studies.dataset.cnsim()
disconnect.studies.dataset.survival()

context("ds.coxphSummary::smk::done")
