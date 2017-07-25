#------------------------------------------------------------------------------------------------------------------------------------------#
#
# OpenVAprocessor.py
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
        Hiv = row[6]
        Malaria = row[7]
        Hce = row[8]
        Freetext = row[9]
        Figures = row[10]
        ProcessDir = row[11]
        LastRun = row[12]
        LastRunDate = LastRun.strftime("%Y/%m/%d")
        LastRunResult = row[13]
        
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
odkBCArgumentList = "java -jar ./briefcase.jar -oc -em -id " + FormID + " -sd " + ProcessDir + " -ed '" + odkBCExportDir + "' -f " + odkBCExportFilename + " -url " + AggURL + " -u " + AggUser + " -p " + AggPass + " -start " + str(LastRunDate)
openVAFilesDir = ProcessDir + "/OpenVAFiles"              
openVAReadyFile = odkBCExportDir + "/OpenVAReadyFile.csv"
rScriptIn  = openVAFilesDir + "/RScript.R"
rScriptOut = openVAFilesDir + "/RScript.Rout"

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------# 
# OPENVA ALGORITHM DEFAULTS
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------# 
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
inSilicoVA_isNumeric = "FALSE"
inSilicoVA_updateCondProb = "TRUE"
inSilicoVA_keepProbbase_level = "TRUE"
inSilicoVA_CondProb = "NULL"
inSilicoVA_CondProbNum = "NULL"
inSilicoVA_datacheck = "TRUE"
inSilicoVA_datacheck_missing = "TRUE"
inSilicoVA_warning_write = "FALSE"
inSilicoVA_external_sep = "TRUE"
# inSilicoVA_Nsim = 4000
inSilicoVA_thin = 10
inSilicoVA_burnin = 2000
inSilicoVA_auto_length = "TRUE"
inSilicoVA_conv_csmf = 0.02
inSilicoVA_jump_scale = 0.1
inSilicoVA_levels_prior = "NULL"
inSilicoVA_levels_strength = 1
inSilicoVA_trunc_min = 1e-04
inSilicoVA_trunc_max = 0.9999
inSilicoVA_subpop = "NULL"
inSilicoVA_java_option = "-Xmx1g"
inSilicoVA_seed = 1
inSilicoVA_phy_code = "NULL"
inSilicoVA_phy_cat = "NULL"
inSilicoVA_phy_unknown = "NULL"
inSilicoVA_phy_external = "NULL"
inSilicoVA_phy_debias = "NULL"
inSilicoVA_exclude_impossible_cause = "TRUE"
inSilicoVA_indiv_CI = "NULL"

## InterVA
     # InterVA(Input, HIV, Malaria, directory = NULL, filename = "VA_result",
     #   output = "classic", append = FALSE, groupcode = FALSE,
     #   replicate = FALSE, replicate.bug1 = FALSE, replicate.bug2 = FALSE,
     #   write = TRUE)
#### Required Args: Input, HIV, Malaria
InterVA_directory      = "NULL"
InterVA_filename       = "VA_result"
InterVA_output         = "classic"
InterVA_append         = "FALSE"
InterVA_groupcode      = "FALSE"
InterVA_Replicate      = "FALSE"
InterVA_Replicate.bug1 = "FALSE"
InterVA_Replicate.bug2 = "FALSE"
InterVA_Write          = "TRUE"

## Naive Bayes Classifier
     # nbc(train, test, known = TRUE)
#### Required Args: train, test
NBC_KNOWN = "TRUE"

## Tariff
     # tariff(causes.train, symps.train, symps.test, causes.table = NULL,
     #   use.rank = TRUE, nboot.rank = 1, use.sig = TRUE, nboot.sig = 500,
     #   use.top = FALSE, ntop = 40)
#### Required Args: causes.train, symps.train, symps.test
Tariff_causes_table = "NULL"
Tariff_use_rank = "TRUE"
Tariff_nboot_rank = 1
Tariff_use_sig = "TRUE"
Tariff_nboot_sig = 500
Tariff_use_top = "FALSE"
Tariff_ntop = 40


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

    #------------------------------------------------------------------------------------------------------------------------------------------#
    # START NEW
    #------------------------------------------------------------------------------------------------------------------------------------------#
    #Create R script for running openVA (HERE -- if this file already exists, remove it?)
    try:
        f = open(rScriptIn, "wb")
        f.write("library(openVA); library(CrossVA) \n")
        f.write("getwd() \n")
        f.write("data <- read.csv(\"" + openVAReadyFile + "\") \n")
        # f.write("data")
        f.write("codeVA(data=data," + "data.type=" + DataType + ", model=" + Model + ", \n")
        f.write("\t data.train=" + DataTrain + ", causes.train=" + CausesTrain + ", \n")
        f.write("\t Nsim=" + NSim + "auto.length" auto.length + ", \n")        
        f.close()
    except:
        sql = """INSERT INTO wp_SVA_EventLog(EventDesc,EventType) VALUES ('Could not create R Script File','Error')"""
        try:
            cursor.execute(sql)
            db.commit()
        except:
            db.rollback()
        cleanup()

    #Run RScript -- STOPPED HERE
    rBatch = "R CMD BATCH --vanilla " + rScriptIn + " " + rScriptOut
    #rBatch = "R CMD BATCH --vanilla" + rScriptIn + " " + rScriptOut + " --args datafile=" + openVAReadyFile

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

    #------------------------------------------------------------------------------------------------------------------------------------------#
    # END NEW -- STOPPED HERE
    #------------------------------------------------------------------------------------------------------------------------------------------#

    #Read OpenVA Output Files and update MySQL Table
    if trc == 0:
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
                cell2 = row[2]
                cell3 = row[3]
                if row[4] == "1":
                    cell4 = "Male"
                elif row[4] == "2":
                    cell4 = "Female"
                elif row[4] == "8":
                    cell4 = "Refused to answer"
                elif row[4] == "9":
                    cell4 = "Don't Know"
                else:
                    cell4 = row[4]

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
