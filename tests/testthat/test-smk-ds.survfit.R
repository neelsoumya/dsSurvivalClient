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

context("ds.survfit::smk::setup")

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


context("ds.survfit::smk")
test_that("simple error, setting up survfit but no summary allowed", {
    
    #try( cox_object <- ds.coxph.SLMA(formula = 'survival::Surv(time=SURVTIME,event=EVENT)~D$age.60')#, dataName = 'D') 
    #, silent = FALSE)
    
    dsBaseClient::ds.Surv(time='STARTTIME', time2='ENDTIME', event = 'EVENT', objectname='surv_object', type='counting')
    #try(
    
    cox_object <- ds.coxph.SLMA(formula = 'surv_object~AGE')#, dataName = 'D')
    
    #, silent=FALSE)
    # print(cox_object$study1$call)
    # print("coeff from simple model")
    # print(cox_object$study1$coefficients[1,1])
    
    # print( datashield.errors() )
    
    # summary(cox_object)
    
    # expect_error( as.character(  ds.coxph.SLMA(formula = 'survival::Surv(time=SURVTIME,event=EVENT)~D$age', dataName = 'D')   ) )
    
    # create survfit object
    dsBaseClient::ds.survfit(formula = 'surv_object~1', objectname = 'survfit_object')
    
    # no summary of survfit object allowed
    expect_error( as.character(  ds.summary(x = 'survfit_object')   ) )
    
})


#
# Done
#

context("ds.survfit::smk::shutdown")

#test_that("shutdown", {
#    ds_expect_variables(c("D"))
#})

disconnect.studies.dataset.cnsim()
disconnect.studies.dataset.survival()

context("ds.survfit::smk::done")
