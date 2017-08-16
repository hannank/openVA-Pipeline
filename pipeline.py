#------------------------------------------------------------------------------------------------------------------------------------------#
#
# pipeline.py
# 
# Purpose: 
#
# Notes:
#
# (--) To set values for an openVA algorithm navigate to "OPENVA ALGORITHM DEFAULTS" and change to desired values.
#      --> an alternative is to specify default values in the database
#
#
#
#------------------------------------------------------------------------------------------------------------------------------------------#

import MySQLdb
import sys
import csv
import datetime
import os
import subprocess
import shutil

#this function is used to cleanup if the script fails and will mark the wp_SVA_Conf table item LastRunResult as 0, close db and exit script
def cleanup():
    if connectionError == "1":
        errorMsg = [time, e]
        f = open(connectionErrorFile, 'wb')
        writer = csv.writer(f)
        writer.writerow([time] + ["Unable to Connect to MySQL Database, see dbErrorLog.csv for details"])
        f.close
        sys.exit(1)
    else:
        #Update Config table with LastRunResult = 0
        try:
            cursor.execute("UPDATE wp_SVA_Conf SET LastRunResult=%s WHERE Conf_ID=%s", ("0","1"))
            db.commit()
            if os.path.isfile(connectionErrorFile) == True:
                try:
                    os.remove(connectionErrorFile)
                except:
                    sys.exit(1)
        except:
            errorMsg = [time, e]
            f = open(errorFile, 'wb')
            writer = csv.writer(f)
            writer.writerow(errorMsg)
            f.close
        db.close()
        sys.exit(1)

errorFile = './dbErrorLog.csv'
time = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
connectionError = "0"
connectionErrorFile = "./MySQLConnect.csv"
    
if os.path.isfile(errorFile) == False:
    f = open(errorFile, 'wb')
    writer = csv.writer(f)
    writer.writerow(['Date'] + ['Description'] + ['Additional Information'])
    f.close


# Open database connection
try:
    db = MySQLdb.connect("localhost", "d4hsva", "d4hsva", "wordpress")

except (MySQLdb.Error, MySQLdb.Warning) as e:
    errorMsg = [time,e]
    f = open(errorFile,'ab')
    writer = csv.writer(f)
    writer.writerow(errorMsg)
    f.close
    connectionError = "1"
    cleanup()

#Read Configuration from Database and store values
sql = "SELECT * FROM wp_SVA_Conf"
try:
    # prepare a cursor object using cursor() method
    cursor = db.cursor()
    # Execute the SQL command
    cursor.execute(sql)
    # Fetch all the rows in a list of lists.
    results = cursor.fetchall()
    for row in results:
        Cod_TableName = row[0]
        AggURL = row[1]
        AggUser = row[2]
        AggPass = row[3]
        FormID = row[4]
        Country = row[5]
        HIV = row[6]
        Malaria = row[7]
        Hce = row[8]
        Freetext = row[9]
        Figures = row[10]
        ProcessDir = row[11]
        LastRun = row[12]
        LastRunDate = LastRun.strftime("%Y/%m/%d")
        LastRunResult = row[13]
        Algorithm = row[15]

except (MySQLdb.Error, MySQLdb.Warning) as e:
    errorMsg = [time,e]
    f = open(errorFile,'ab')
    writer = csv.writer(f)
    writer.writerow(errorMsg)
    f.close
    cleanup()

#Set variables for use with Briefcase and OpenVA in R
odkBCExportDir = ProcessDir + "/ODKExport"
odkBCExportFilename = "ODKExportNew.csv"
odkBCExportNewFile = odkBCExportDir + "/" + odkBCExportFilename
odkBCExportPrevious = odkBCExportDir + "/ODKExportPrevious.csv"
# odkBCArgumentList = "java -jar ./briefcase.jar -oc -em -id '" + FormID + "' -sd '" + ProcessDir + "' -ed '" + odkBCExportDir + "' -f '" + odkBCExportFilename + "' -url '" + AggURL + "' -u '" + AggUser + "' -p '" + AggPass + "' -start " + str(LastRunDate)
odkBCArgumentList = "java -jar ./briefcase.jar -oc -em -id '" + FormID + "' -sd '" + odkBCExportDir + "' -ed '" + odkBCExportDir + "' -f '" + odkBCExportFilename + "' -url '" + AggURL + "' -u '" + AggUser + "' -p '" + AggPass + "' -start " + str(LastRunDate)
openVAFilesDir = ProcessDir + "/OpenVAFiles"              
openVAReadyFile = odkBCExportDir + "/OpenVAReadyFile.csv"
rScriptIn  = openVAFilesDir + "/RScript.R"
rScriptOut = openVAFilesDir + "/RScript.Rout"

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------# 
# OPENVA ALGORITHM DEFAULTS
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------# 
#### HERE -- how do you want to read in the primary inputs
DataType = '"WHO"'
# Model = '"InterVA"'
# DataTrain = '"train"'
# DataTrain = "train"
# CausesTrain = '"cause"'
##### HERE -- put these into an array and loop over them when writing the R script?
##### HERE -- which of these does codeVA() refer to?

## InSilicoVA
     # insilico(data, isNumeric = FALSE, updateCondProb = TRUE,
     #   keepProbbase.level = TRUE, CondProb = NULL, CondProbNum = NULL,
     #   datacheck = TRUE, datacheck.missing = TRUE, warning.write = FALSE,
     #   external.sep = TRUE, Nsim = 4000, thin = 10, burnin = 2000,
     #   auto.length = TRUE, conv.csmf = 0.02, jump.scale = 0.1,
     #   levels.prior = NULL, levels.strength = 1, trunc.min = 1e-04,
     #   trunc.max = 0.9999, subpop = NULL, java_option = "-Xmx1g", seed = 1,
     #   phy.code = NULL, phy.cat = NULL, phy.unknown = NULL,
     #   phy.external = NULL, phy.debias = NULL, exclude.impossible.cause = TRUE,
     #   indiv.CI = NULL)
#### Required Args: Input, Nsim 
insilico_Nsim = "10000"
insilico_isNumeric = "FALSE"
insilico_updateCondProb = "TRUE"
insilico_keepProbbase_level = "TRUE"
insilico_CondProb = "NULL"
insilico_CondProbNum = "NULL"
insilico_datacheck = "TRUE"
insilico_datacheck_missing = "TRUE"
insilico_warning_write = "FALSE"
insilico_external_sep = "TRUE"
insilico_thin = "10"
insilico_burnin = "2000"
insilico_auto_length = "FALSE"
insilico_conv_csmf = "0.02"
insilico_jump_scale = "0.1"
insilico_levels_prior = "NULL"
insilico_levels_strength = "1"
insilico_trunc_min = "1e-04"
insilico_trunc_max = "0.9999"
insilico_subpop = "NULL"
insilico_java_option = '"-Xmx1g"'
insilico_seed = "1"
insilico_phy_code = "NULL"
insilico_phy_cat = "NULL"
insilico_phy_unknown = "NULL"
insilico_phy_external = "NULL"
insilico_phy_debias = "NULL"
insilico_exclude_impossible_cause = "TRUE"
insilico_indiv_CI = "NULL"

## InterVA
     # InterVA(Input, HIV, Malaria, directory = NULL, filename = "VA_result",
     #   output = "classic", append = FALSE, groupcode = FALSE,
     #   replicate = FALSE, replicate.bug1 = FALSE, replicate.bug2 = FALSE,
     #   write = TRUE)
#### Required Args: Input, HIV, Malaria
interVA_filename       = '"VA_result"'
interVA_output         = '"classic"'
interVA_append         = "FALSE"
interVA_groupcode      = "FALSE"
interVA_version        = '"4.03"'
# interVA_replicate      = "FALSE"
# interVA_replicate_bug1 = "FALSE"
# interVA_replicate_bug2 = "FALSE"
# interVA_write          = "TRUE"

## Naive Bayes Classifier
     # nbc(train, test, known = TRUE)
#### Required Args: train, test
nbc_known = "TRUE"

## Tariff
     # tariff(causes.train, symps.train, symps.test, causes.table = NULL,
     #   use.rank = TRUE, nboot.rank = 1, use.sig = TRUE, nboot.sig = 500,
     #   use.top = FALSE, ntop = 40)
#### Required Args: causes.train, symps.train, symps.test
tariff_causes_table = "NULL"
tariff_use_rank = "TRUE"
tariff_nboot_rank = "1"
tariff_use_sig = "TRUE"
tariff_nboot_sig = "500"
tariff_use_top = "FALSE"
tariff_ntop = "40"


#Check if Processing Directory exists and create if necessary
if not os.path.exists(ProcessDir):
    try:
        os.makedirs(ProcessDir)
    except:
        sql = """INSERT INTO wp_SVA_EventLog(EventDesc,EventType) VALUES ('Could not create Processing Directory','Error')"""
        try:
            cursor.execute(sql)
            db.commit()
        except:
            db.rollback()
        cleanup()

#Clear OpenVAFolder ready for processing (HERE -- do we want to save the R BATCH files?)
if os.path.exists(openVAFilesDir):
    try:
        shutil.rmtree(openVAFilesDir)
    except (IOError, os.error, shutil.Error) as e:
        try:
            cursor.execute("INSERT INTO wp_SVA_EventLog(EventDesc,EventType) VALUES (%s,%s)", (e,"Error"))
            db.commit()
        except:
            db.rollback()
        cleanup()

if not os.path.exists(openVAFilesDir):
    try:
        os.makedirs(openVAFilesDir)
    except:
        sql = """INSERT INTO wp_SVA_EventLog(EventDesc,EventType) VALUES ('Could not create OpenVA Directory','Error')"""
        try:
            cursor.execute(sql)
            db.commit()
        except:
            db.rollback()
        cleanup()

#Make a Copy of last ODK Briefcase Export file, to compare with new file once exported, unless last doesn't exist, then copy and rename file to OpenVAReadyFile.csv
if os.path.isfile(odkBCExportNewFile) == True and LastRunResult == 1 and not os.path.isfile(connectionErrorFile):
    try:
        shutil.copy(odkBCExportNewFile, odkBCExportPrevious)
    except (IOError, os.error, shutil.Error) as e:
        try:
            cursor.execute("INSERT INTO wp_SVA_EventLog(EventDesc,EventType) VALUES (%s,%s)", (e,"Error"))
            db.commit()
        except:
            db.rollback()
        cleanup()
    try:
        os.remove(openVAReadyFile)
    except (IOError, os.error):
        print IOError
        print os.error
        sql = """INSERT INTO wp_SVA_EventLog(EventDesc,EventType) VALUES ('Could not remove OpenVAReadyFile.csv','Error')"""
        try:
            cursor.execute(sql)
            db.commit()
        except:
            db.rollback()
        cleanup()

#Lauch ODK Briefcase CLI to collect Aggregate data and export to file for further processing
try:
    process = subprocess.Popen(odkBCArgumentList, stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
    stdout, stderr = process.communicate()
    rc = process.returncode

#Catch Errors with calling Briefcase and log to MySQL Event Log table
except:
    sql = """INSERT INTO wp_SVA_EventLog(EventDesc,EventType) VALUES ('Could not launch Briefcase Java Application','Error')"""
    try:
        cursor.execute(sql)
        db.commit()
    except:
        db.rollback()
    cleanup()

#Catch Application Errors from ODK Briefcase CLI and log into MySQL Event Log table
if rc != 0:
    try:
        cursor.execute("INSERT INTO wp_SVA_EventLog(EventDesc,EventType) VALUES (%s,%s)", (stderr, "Error"))
        db.commit()
    except:
        db.rollback()
    cleanup()

if "SEVERE" in stderr:
    try:
        cursor.execute("INSERT INTO wp_SVA_EventLog(EventDesc,EventType) VALUES (%s,%s)", (stderr,"Error"))
        db.commit()
    except:
        db.rollback()
    cleanup()
else:
    sql = """INSERT INTO wp_SVA_EventLog(EventDesc,EventType) VALUES ('Briefcase Export Completed Successfully','Information')"""
    try:
        cursor.execute(sql)
        db.commit()
    except:
        db.rollback()
    #Check if previous file exists from above operations and create delta file of new entries
    if os.path.isfile(odkBCExportPrevious) == True:
        try:
            with open(odkBCExportPrevious, 'rb') as t1, open(odkBCExportNewFile, 'rb') as t2:
                fileone = t1.readlines()
                filetwo = t2.readlines()
                header = filetwo[0]

            with open(openVAReadyFile, 'wb') as outFile:
                outFile.write(header)
                for line in filetwo:
                    if line not in fileone:
                        outFile.write(line) 
        except:
            sql = """INSERT INTO wp_SVA_EventLog(EventDesc,EventType) VALUES ('Could not create OpenVAReadyFile.csv','Error')"""
            try:
                cursor.execute(sql)
                db.commit()
            except:
                db.rollback()
            cleanup()
    else:
        #If there is no pre-existing ODK Briefcase Export file, then copy and rename to OpenVAReadyFile.csv, ready for openVA analysis.
        try:
            shutil.copy(odkBCExportNewFile, openVAReadyFile)
        except (IOError, os.error, shutil.Error) as e:
            try:
                cursor.execute("INSERT INTO wp_SVA_EventLog(EventDesc,EventType) VALUES (%s,%s)", (e,"Error"))
                db.commit()
            except:
                db.rollback()
            cleanup()

    #Create R script for running openVA (HERE -- if this file already exists, remove it?)
    try:
        f = open(rScriptIn, "wb")
        f.write("library(openVA); library(CrossVA) \n")
        f.write("getwd() \n")
        f.write("records <- read.csv(\"" + openVAReadyFile + "\") \n")
        f.write("data <- map_records_interva4(records) \n")

        ## insilico
        if Algorithm == "InSilicoVA":
            f.write("out <- codeVA(data=data, " + "data.type=" + DataType + ", model='" + Alogrithm + "', \n")
            # f.write("\t data.train=" + DataTrain + ", causes.train=" + CausesTrain + ", \n")
            f.write("\t Nsim=" + insilico_Nsim + ", \n")        
            f.write("\t isNumeric=" + insilico_isNumeric + ", \n")
            f.write("\t updateCondProb=" + insilico_updateCondProb + ", \n")
            f.write("\t keepProbbase.level=" + insilico_keepProbbase_level + ", \n")
            f.write("\t CondProb=" + insilico_CondProb + ", \n")
            f.write("\t CondProbNum=" + insilico_CondProbNum + ", \n")
            f.write("\t datacheck=" + insilico_datacheck + ", \n")
            f.write("\t datacheck.missing=" + insilico_datacheck_missing + ", \n")
            f.write("\t warning.write=" + insilico_warning_write + ", \n")
            f.write("\t external.sep=" + insilico_external_sep + ", \n")
            f.write("\t thin=" + insilico_thin + ", \n")
            f.write("\t burnin=" + insilico_burnin + ", \n")
            f.write("\t auto.length=" + insilico_auto_length + ", \n")
            f.write("\t conv.csmf=" + insilico_conv_csmf + ", \n")
            f.write("\t jump.scale=" + insilico_jump_scale + ", \n")
            f.write("\t levels.prior=" + insilico_levels_prior + ", \n")
            f.write("\t levels.strength=" + insilico_levels_strength + ", \n")
            f.write("\t trunc.min=" + insilico_trunc_min + ", \n")
            f.write("\t trunc.max=" + insilico_trunc_max + ", \n")
            f.write("\t subpop=" + insilico_subpop + ", \n")
            f.write("\t java.option=" + insilico_java_option + ", \n")
            f.write("\t seed=" + insilico_seed + ", \n")
            f.write("\t phy.code=" + insilico_phy_code + ", \n")
            f.write("\t phy.cat=" + insilico_phy_cat + ", \n")
            f.write("\t phy.unknown=" + insilico_phy_unknown + ", \n")
            f.write("\t phy.external=" + insilico_phy_external + ", \n")
            f.write("\t phy.debias=" + insilico_phy_debias + ", \n")
            f.write("\t exclude.impossible.cause=" + insilico_exclude_impossible_cause + ", \n")
            f.write("\t indiv.CI=" + insilico_indiv_CI + ") \n")

        ## interVA
        elif Algorithm == "InterVA":
            f.write("out <- codeVA(data=data, " + "data.type=" + DataType + ", model='" + Algorithm + "', \n")
            f.write("\t HIV= \"" + HIV + "\", \n")                ## can't have spaces around value ('v', 'l', or, 'h')
            f.write("\t Malaria = \"" + Malaria + "\", \n")       ## can't have spaces around value ('v', 'l', or, 'h')
            f.write("\t filename=" + interVA_filename + ", \n")
            f.write("\t output=" + interVA_output + ", \n")
            f.write("\t append=" + interVA_append + ", \n")
            f.write("\t groupcode=" + interVA_groupcode + ", \n")
            f.write("\t version=" + interVA_version + ") \n")
            # f.write("\t replicate=" + interVA_replicate + ", \n")                 
            # f.write("\t replicate.bug1=" + interVA_replicate_bug1 + ", \n")
            # f.write("\t replicate.bug2=" + interVA_replicate_bug2 + ", \n")
            # f.write("\t write=" + interVA_write + ") \n")

        ## nbc
        elif Algorithm=='"NBC"':
            f.write("out <- codeVA(data=data, " + "data.type=" + DataType + ", model='" + Algorithm + "', \n")
            f.write("\t known=" + nbc_known + ") \n")

        ## tariff
        else:
            f.write("out <- codeVA(data=data, " + "data.type=" + DataType + ", model='" + Algorithm + "', \n")
            f.write("\t causes.table=" + tariff_causes_table + ", \n")
            f.write("\t use.rank=" + tariff_use_rank + ", \n")
            f.write("\t nboot.rank=" + tariff_nboot_rank + ", \n")
            f.write("\t use.sig=" + tariff_use_sig + ", \n")
            f.write("\t nboot.sig=" + tariff_nboot_sig + ", \n")
            f.write("\t use.top=" + tariff_use_top + ", \n")
            f.write("\t ntop=" + tariff_ntop + ") \n")

        ## write results
        f.write("cod <- getTopCOD(out) \n")
        f.write("age <- records$consented.deceased_CRVS.info_on_deceased.ageInYears \n")
        f.write("sex <- ifelse(tolower(data$MALE)=='y', 'Male', 'Female') \n")
        f.write("out2 <- cbind(cod, age, sex) \n")
        #### HERE -- might want to set file name as an input parameter/value in config table
        f.write("write.csv(out2, file='" + openVAFilesDir + "/predictions.csv', row.names=FALSE)")
        f.close()        

    except:
        sql = """INSERT INTO wp_SVA_EventLog(EventDesc,EventType) VALUES ('Could not create R Script File','Error')"""
        try:
            cursor.execute(sql)
            db.commit()
        except:
            db.rollback()
        cleanup()

    #Run RScript
    rBatch = "R CMD BATCH --vanilla " + rScriptIn + " " + rScriptOut
    rprocess = subprocess.Popen(rBatch, stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
    stdout, stderr = rprocess.communicate()
    rrc = rprocess.returncode
    if rrc != 0: 
        sql = """INSERT INTO wp_SVA_EventLog(EventDesc,EventType) VALUES ('Could not run R Script','Error')"""
        try:
            cursor.execute(sql)
            db.commit()
        except:
            db.rollback()
            cleanup()
            
        ## HERE -- you could search output file for "Error" and "Execution halted" (last two lines) and copy Error to log
        ##         (use regulare expression -- import re and re.findall()
            
        else:
            sql = """INSERT INTO wp_SVA_EventLog(EventDesc,EventType) VALUES ('OpenVA Analysis Completed Successfully','Information')"""
            try:
                cursor.execute(sql)
                db.commit()
            except:
                db.rollback()
                cleanup()

    ## HERE -- at this point, you may just want to go to DHIS2
    #Read OpenVA Output Files and update MySQL Table
    if rrc == 0:
        #Get list of all predictions files
        files = [f for f in os.listdir(openVAFilesDir) if f.endswith('predictions.csv')]

        for file in files:
            path = openVAFilesDir + '/' + file
        
            # open file and create reader
            tcsv = open(path, 'rb')
            reader = csv.reader(tcsv, delimiter=',', quotechar='"', skipinitialspace=True)
            
            # read header
            header = reader.next()
            
            # read rows, append values to lists
            for row in reader:
                cell1 = row[0]
                cell2 = row[1]
                cell3 = row[2]
                cell4 = row[3]

                #Add Entry to MySQL Database
                try:
                    cursor.execute("INSERT INTO wp_SVA_CoD(VA_ID,CoD,Age,Sex) VALUES (%s,%s,%s,%s)", (cell1,cell2,cell3,cell4))

                #If duplicate VA_ID perform UPDATE to the record
                except (MySQLdb.IntegrityError) as e:
                    errorMsg = [time,e,'Check Date_Updated in database correlates to this entry.  If so, update to entry was successful']
                    f = open(errorFile,'ab')
                    writer = csv.writer(f)
                    writer.writerow(errorMsg)
                    f.close
                    try:
                        cursor.execute("UPDATE wp_SVA_CoD SET VA_ID=%s,CoD=%s,Age=%s,Sex=%s,Date_Updated=%s WHERE VA_ID=%s", (cell1,cell2,cell3,cell4,time,cell1))
                        db.commit()
                    except:
                        db.rollback()
                        cleanup()
                
                #If error in mysql insert query, write message to dbErroLog file
                except (MySQLdb.Error, MySQLdb.Warning) as e:
                    errorMsg = [time,e]
                    f = open(errorFile,'ab')
                    writer = csv.writer(f)
                    writer.writerow(errorMsg)
                    f.close
                    db.rollback()
                    cleanup()

        #Update Config table with LastRun date if successfull
        try:
            cursor.execute("UPDATE wp_SVA_Conf SET LastRun=%s, LastRunResult=%s WHERE Conf_ID=%s", (time,"1", "1"))
            db.commit()
        except (MySQLdb.Error, MySQLdb.Warning) as e:
            db.rollback()
            errorMsg = [time,e]
            f = open(errorFile,'ab')
            writer = csv.writer(f)
            writer.writerow(errorMsg)
            f.close
            db.close()
            sys.exit()
        if os.path.isfile(connectionErrorFile) == True:
            try:
                os.remove(connectionErrorFile)
            except:
                sys.exit(1)
    else:
        sql = """INSERT INTO wp_SVA_EventLog(EventDesc,EventType) VALUES ('OpenVA exited with non-zero code','Error')"""
        try:
            cursor.execute(sql)
            db.commit()
        except:
            db.rollback()
            cleanup()
