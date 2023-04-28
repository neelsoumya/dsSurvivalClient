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

context("ds.vcov.coxph::smk::setup")

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



context("ds.vcov.coxph::smk")
test_that("simple test, checking results", {
    
    dsSurvivalClient::ds.Surv(time='STARTTIME', time2='ENDTIME', event = 'EVENT', objectname='surv_object', type='counting')
    
    dsSurvivalClient::ds.coxphSLMAassign(formula = 'surv_object~AGE', objectname = 'cox_object_serverside')
    
    vcov_res = dsSurvivalClient::ds.vcov.coxph(object = 'cox_object_serverside')
    expect_equal(vcov_res$survival1[1], 1.238578e-05, tolerance = 0.0001)
    
    
})



#
# Done
#

context("ds.coxphSummary::smk::shutdown")

#test_that("shutdown", {
#    ds_expect_variables(c("D"))
#})

disconnect.studies.dataset.survival()

context("ds.coxphSummary::smk::done")
