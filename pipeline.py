#------------------------------------------------------------------------------------------------------------------------------------------#
#
# pipeline.py
# 
# Notes:
#
# (1) Configuration for ODK Aggregate, openVA, and DHIS2 are stored in MySQL tables in the Pipeline database
#     (ODK_Conf, openVAConf, and DHIS_Conf, respectively).
#
# (2) Log files:
#     -- Errors associated with MySQL Pipeline database 
#        ++ connectionErrorFile = "./MySQLConnect.csv"
#           (unable to connect to DB; cleanup() attempt to remove this file on exit if able to log error in database)
#        ++ errorFile = "./dbErrorLog.csv"
#           (contains time and error message generated when trying to connect to db)
#     -- ODK Aggregate errors are stored in the Pipeline database (table name is ODK_Eventlog)
#
#------------------------------------------------------------------------------------------------------------------------------------------#

import MySQLdb
import sys
import csv
import datetime
import os
import subprocess
import shutil
import requests
import json
import sqlite3
import time
import uuid
from dateutil import parser

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------# 
# Define functions and objects needed for functioning of pipeline; then set up log files and configuration of pipeline 
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------# 

# class for connecting to DHIS2 
class Dhis(object):
    def __init__(self, dhisURL, dhisUser, dhisPass):
        if '/api' in dhisURL:
            print('Please do not specify /api/ in the server argument: e.g. --server=play.dhis2.org/demo')
            sys.exit()
        if dhisURL.startswith('localhost') or dhisURL.startswith('127.0.0.1'):
            dhisURL = 'http://{}'.format(dhisURL)
        elif dhisURL.startswith('http://'):
            dhisURL = dhisURL
        elif not dhisURL.startswith('https://'):
            dhisURL = 'https://{}'.format(dhisURL)
        self.auth = (dhisUser, dhisPass)
        self.url = '{}/api'.format(dhisURL)
#
    def get(self, endpoint, params=None):
        url = '{}/{}.json'.format(self.url, endpoint)
        if not params:
            params = {}
        params['paging'] = False
        try:
            r = requests.get(url=url, params=params, auth=self.auth)
            if r.status_code != 200:
                print("HTTP Code: {}".format(r.status_code))
                print(r.text)
            else:
                return r.json()
        except requests.RequestException as e:
            print(e)
#
    def post(self, endpoint, data):
        """
        POST  method for DHIS2 API.
        :rtype: dict
        """
        url = '{}/{}.json'.format(self.url, endpoint)
        try:
            r = requests.post(url=url, json=data, auth=self.auth)
            if r.status_code not in range(200, 206):
                print("HTTP Code: {}".format(r.status_code))
                print(r.text)
            else:
                return r.json()
        except requests.RequestException as e:
            print(e)
#
    def post_blob(self, f):
        """ Post file to DHIS2 and return created UID for that file
        :rtype: str
        """
        url = '{}/fileResources'.format(self.url)
        files = {'file': (f, open(f, 'rb'), 'application/x-sqlite3', {'Expires': '0'})}
        try:
            r = requests.post(url, files=files, auth=self.auth)
            if r.status_code not in (200, 202):
                print("HTTP Code: {}".format(r.status_code))
                print(r.text)
            else:
                response = r.json()
                file_id = response['response']['fileResource']['id']
                print("\tPosted BLOB for VA ID {} - returned fileResource ID: {}".format(va_id, file_id))
                return file_id
#
        except requests.RequestException as e:
            print(e)

# class for creating VA event & BLOB file
class VerbalAutopsyEvent(object):
    """ DHIS2 event + a BLOB file resource"""
#
    def __init__(self, va_id, program, orgunit, event_date, sex, age, icd10, algorithm_metadata, file_id):
        self.va_id = va_id
        self.program = program
        self.dhisOrgUnit = dhisOrgUnit
        self.event_date = event_date
        self.sex = sex
        self.age = age
        self.icd10 = icd10
        self.algorithm_metadata = algorithm_metadata
        self.file_id = file_id
        self.datavalues = [
            {"dataElement": "htm6PixLJNy", "value": self.va_id},
            {"dataElement": "hi7qRC4SMMk", "value": self.sex},
            {"dataElement": "F4XGdOBvWww", "value": self.icd10},
            {"dataElement": "wiJviUqN1io", "value": self.algorithm_metadata},
            {"dataElement": "XLHIBoLtjGt", "value": file_id}
        ]
        self.datavalues.append({"dataElement": "oPAg4MA0880", "value": self.age})
#
    def format_to_dhis2(self, dhisUser):
        """
        Format object to DHIS2 compatible event for DHIS2 API
        :rtype: dict
        """
        event = {
            "program": self.program,
            "orgUnit": self.dhisOrgUnit,
            "eventDate": datetime.datetime.strftime(self.event_date, '%Y-%m-%d'),
            "status": "COMPLETED",
            "storedBy": dhisUser,
            "dataValues": self.datavalues
        }
        return event
#
    def __str__(self):
        return json.dumps(self, default=lambda o: o.__dict__)

# create a dummy SQLite database
def create_db(f):
    """
    Create a dummy SQLite database
    :rtype: None
    """
    conn = sqlite3.connect(f)
    with conn:
        cur = conn.cursor()
        cur.execute("CREATE TABLE Cars(Id INT, Name TEXT, Price INT)")
        cur.execute("INSERT INTO Cars VALUES(1,'Audi',52642)")
        cur.execute("INSERT INTO Cars VALUES(2,'Mercedes',57127)")
        cur.execute("INSERT INTO Cars VALUES(3,'Skoda',9000)")
        cur.execute("INSERT INTO Cars VALUES(4,'Volvo',29000)")
        cur.execute("INSERT INTO Cars VALUES(5,'Bentley',350000)")
        cur.execute("INSERT INTO Cars VALUES(6,'Citroen',21000)")
        cur.execute("INSERT INTO Cars VALUES(7,'Hummer',41400)")
        cur.execute("INSERT INTO Cars VALUES(8,'Volkswagen',21600)")

# ICD10 Codes for openVA output
icd10OpenVA = {"Sepsis (non-obstetric)": "A40", "Acute resp infect incl pneumonia": "J00", "HIV/AIDS related death": "B20",
               "Diarrhoeal diseases": "A00", "Malaria": "B50", "Measles": "B05", "Meningitis and encephalitis": "A39", "Tetanus": "A33",
               "Pulmonary tuberculosis": "A15", "Pertussis": "A37", "Haemorrhagic fever": "A90", "Other and unspecified infect dis": "A17",
               "Oral neoplasms": "C00", "Digestive neoplasms": "C15", "Respiratory neoplasms": "C30", "Breast neoplasms": "C50",
               "Reproductive neoplasms MF": "C51", "Other and unspecified neoplasms": "C07", "Severe anaemia": "D50",
               "Severe malnutrition": "E40", "Diabetes mellitus": "E10", "Acute cardiac disease": "I20", "Sickle cell with crisis": "I60",
               "Stroke": "D57", "Other and unspecified cardiac dis": "I00", "Chronic obstructive pulmonary dis": "J40", "Asthma": "J45",
               "Acute abdomen": "R10", "Liver cirrhosis": "K70", "Renal failure": "N17", "Epilepsy": "G40", "Other and unspecified NCD": "D55",
               "Ectopic pregnancy": "O00", "Abortion-related death": "O03", "Pregnancy-induced hypertension": "O10", "Obstetric haemorrhage": "O46",
               "Obstructed labour": "O63", "Pregnancy-related sepsis": "O85", "Anaemia of pregnancy": "O99", "Ruptured uterus": "O71",
               "Other and unspecified maternal CoD": "O01", "Prematurity": "P05", "Birth asphyxia": "P20", "Neonatal pneumonia": "P23",
               "Neonatal sepsis": "P36", "Congenital malformation": "Q00", "Other and unspecified neonatal CoD": "P00",
               "Fresh stillbirth": "P95", "Macerated stillbirth": "P95", "Road traffic accident": "V01", "Other transport accident": "V90",
               "Accid fall": "W00", "Accid drowning and submersion": "W65", "Accid expos to smoke fire & flame": "X00",
               "Contact with venomous plant/animal": "X20", "Accid poisoning and noxious subs": "X40", "Intentional self-harm": "X60",
               "Assault": "X85", "Exposure to force of nature": "X30", "Other and unspecified external CoD": "S00", "Cause of death unknown": "R95"}

# set the ODK_Conf table item odkLastRunResult as 0, close db and exit script upon error or completion of script
def cleanup():
    if connectionError == "1":
        # errorMsg = [time, e] 
        f = open(connectionErrorFile, "wb")
        writer = csv.writer(f)
        writer.writerow([timeFMT] + ["Unable to Connect to MySQL Database, see dbErrorLog.csv for details"])
        f.close()
        sys.exit(1)
    else:
        try:
            sql = "UPDATE ODK_Conf SET odkLastRunResult = %s"
            par = ("0",)
            # cursor.execute("UPDATE ODK_Conf SET odkLastRunResult=%s WHERE Conf_ID=%s", ("0","1"))
            cursor.execute(sql, par)
            db.commit()
            if os.path.isfile(connectionErrorFile) == True:
                try:
                    os.remove(connectionErrorFile)
                except:
                    sys.exit(1)
        except:
            errorMsg = [timeFMT, e]
            f = open(errorFile, "wb")
            writer = csv.writer(f)
            writer.writerow(errorMsg)
            f.close()
        db.close()
        sys.exit(1)

# log files
WARNING = '\033[91m'
ENDC = '\033[0m'
errorFile = "./dbErrorLog.csv"
timeFMT = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
connectionError = "0"
connectionErrorFile = "./MySQLConnect.csv"
    
if os.path.isfile(errorFile) == False:
    f = open(errorFile, "wb")
    writer = csv.writer(f)
    writer.writerow(["Date"] + ["Description"] + ["Additional Information"])
    f.close()

# configure pipeline
# open database connection for configuration of ODK Aggregate and DHIS2
try:
    db = MySQLdb.connect("localhost", "d4h", "d4h", "Pipeline")
    # or put username and password in /path/.my.cnf and use: db=MySQLdb.connect(host="localhost", db="Pipeline", read_default_file="/path/.my.cnf")
except (MySQLdb.Error, MySQLdb.Warning) as e:
    errorMsg = [timeFMT,e]
    f = open(errorFile,"ab")
    writer = csv.writer(f)
    writer.writerow(errorMsg)
    f.close()
    connectionError = "1"
    cleanup()

sqlODK = "SELECT odkURL, odkUser, odkPass, odkFormID, odkLastRun, odkLastRunResult FROM ODK_Conf"
sqlDHIS = "SELECT dhisURL, dhisUser, dhisPass, dhisOrgUnit FROM DHIS_Conf;"
try:
    cursor = db.cursor()
    # get ODK Aggregate configuration
    cursor.execute(sqlODK)
    resultsODK = cursor.fetchall()
    for row in resultsODK:
        odkURL = row[0]
        odkUser = row[1]
        odkPass = row[2]
        odkFormID = row[3]
        odkLastRun = row[4]
        odkLastRunDate = odkLastRun.strftime("%Y/%m/%d")
        odkLastRunResult = row[5]
    # set up R configuration
    # get DHIS2 configuration
    cursor.execute(sqlDHIS)
    resultsDHIS = cursor.fetchall()
    for row in resultsDHIS:
        dhisURL = row[0]
        dhisUser = row[1]
        dhisPass = row[2]
        dhisOrgUnit = row[3] ## want to grab this info from ODK record
except (MySQLdb.Error, MySQLdb.Warning) as e:
    errorMsg = [timeFMT,e]
    f = open(errorFile,"ab")
    writer = csv.writer(f)
    writer.writerow(errorMsg)
    f.close()
    cleanup()

# set variables for use with Briefcase and OpenVA in R
ProcessDir = os.path.dirname(os.path.abspath("pipeline.py"))
## HERE: (this gets created by briefcase.jar...I think)
odkBCExportDir = ProcessDir + "/ODKExport"
odkBCExportFilename = "ODKExportNew.csv"
odkBCExportNewFile = odkBCExportDir + "/" + odkBCExportFilename
odkBCExportPrevious = odkBCExportDir + "/ODKExportPrevious.csv"
## HERE: you need to specify older verion of java here for windows (Java 9 installed for rJava)
odkBCArgumentList = "java -jar ./briefcase.jar -oc -em -id '" + odkFormID + "' -sd '" + odkBCExportDir + "' -ed '" + odkBCExportDir + "' -f '" + odkBCExportFilename + "' -url '" + odkURL + "' -u '" + odkUser + "' -p '" + odkPass + "' -start " + str(odkLastRunDate)
openVAFilesDir = ProcessDir + "/OpenVAFiles"
openVAReadyFile = odkBCExportDir + "/OpenVAReadyFile.csv"
rScriptIn  = openVAFilesDir + "/RScript.R"
rScriptOut = openVAFilesDir + "/RScript.Rout"
dhisDir = ProcessDir + "/DHIS2"

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------# 
# OPENVA ALGORITHM DEFAULTS -- move these to MySQL Pipeline database
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------# 
#### HERE -- how do you want to read in the primary inputs
vaAlgorithm = "InterVA"
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
interVA_HIV = "l"
interVA_Malaria = "l"
interVA_filename       = "VA_result"
interVA_output         = "classic"
# interVA_filename       = " 'VA_result' "
# interVA_output         = " 'classic' "
interVA_append         = "FALSE"
interVA_groupcode      = "FALSE"
interVA_version        = "4.03"
# interVA_version        = " '4.03' "
interVA_replicate      = "FALSE"
interVA_replicate_bug1 = "FALSE"
interVA_replicate_bug2 = "FALSE"
interVA_write          = "FALSE"

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

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------# 
# Remove old files, merge unprocessed Briefcase export (if exists), and then start flow of records through pipeline 
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------# 
# check if processing directory exists and create if necessary (the processing dir is currently set as the working directory where pipeline.py resides so might not want this)
if not os.path.exists(ProcessDir):
    try:
        os.makedirs(ProcessDir)
    except:
        sql = "INSERT INTO ODK_EventLog(odkEventDesc, odkEventType, odkEventTime) VALUES (%s, %s, %s)"
        par = ("Could not create processing directory (ProcessDir)", "Error", timeFMT)
        try:
            cursor.execute(sql, par)
            db.commit()
        except (MySQLdb.Error, MySQLdb.Warning) as e:
            db.rollback()
        cleanup()

# clear openVAFilesDir (if exists) for processing 
if os.path.exists(openVAFilesDir):
    try:
        shutil.rmtree(openVAFilesDir)
    except (IOError, OSError, shutil.Error) as e:
        try:
            sql = "INSERT INTO ODK_EventLog(odkEventDesc, odkEventType, odkEventTime) VALUES (%s, %s, %s)"
            par = (e, "Error", timeFMT)
            cursor.execute(sql, par)
            db.commit()
        except (MySQLdb.Error, MySQLdb.Warning) as e:
            db.rollback()
        cleanup()

# create openVAFilesDir (if does not exist)
if not os.path.exists(openVAFilesDir):
    try:
        os.makedirs(openVAFilesDir)
    except:
        try:
            sql = "INSERT INTO ODK_EventLog(odkEventDesc, odkEventType, odkEventTime) VALUES (%s, %s, %s)"
            par = ("Could not create OpenVA Directory","Error", timeFMT)
            cursor.execute(sql, par)
            db.commit()
        except (MySQLdb.Error, MySQLdb.Warning) as e:
            db.rollback()
        cleanup()

# make a copy of current ODK Briefcase Export file, to compare with new file once exported,
# unless current doesn't exist, then copy and rename file to OpenVAReadyFile.csv
if os.path.isfile(odkBCExportNewFile) == True and odkLastRunResult == 1 and not os.path.isfile(connectionErrorFile):
    try:
        shutil.copy(odkBCExportNewFile, odkBCExportPrevious)
    except (IOError, OSError, shutil.Error) as e:
        try:
            sql = "INSERT INTO ODK_EventLog(odkEventDesc, odkEventType, odkEventTime) VALUES (%s, %s, %s)"
            par = (e, "Error", timeFMT)
            cursor.execute(sql, par)
            db.commit()
        except (MySQLdb.Error, MySQLdb.Warning) as e:
            db.rollback()
        cleanup()
    try:
        os.remove(openVAReadyFile)
    except (IOError, OSError):
        print IOError
        print OSError
        try:
            sql = "INSERT INTO ODK_EventLog(odkEventDesc, odkEventType, odkEventTime)" 
            par = ("Could not remove OpenVAReadyFile.csv", "Error", timeFMT)
            cursor.execute(sql, par)
            db.commit()
        except (MySQLdb.Error, MySQLdb.Warning) as e:
            db.rollback()
        cleanup()

# launch ODK Briefcase to collect Aggregate data and export to file for further processing
try:
    process = subprocess.Popen(odkBCArgumentList, stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
    stdout, stderr = process.communicate()
    rc = process.returncode
# catch Errors with calling Briefcase and log to MySQL Event Log table
except:
    try:
        sql = "INSERT INTO ODK_EventLog(odkEventDesc, odkEventType, odkEventTime) VALUES (%s, %s, %s)"
        par = ("Could not launch ODK Briefcase Java Application", "Error", timeFMT)
        cursor.execute(sql, par)
        db.commit()
    except (MySQLdb.Error, MySQLdb.Warning) as e:
        db.rollback()
    cleanup()

# catch Application Errors from ODK Briefcase CLI and log into MySQL Event Log table
if rc != 0:
    try:
        sql = "INSERT INTO ODK_EventLog(odkEventDesc, odkEventType, odkEventTime) VALUES (%s, %s, %s)"
        par = (stderr, "Error", timeFMT)
        cursor.execute(sql, par)
        db.commit()
    except (MySQLdb.Error, MySQLdb.Warning) as e: 
        db.rollback()
    cleanup()
if "SEVERE" in stderr:
    try:
        sql = "INSERT INTO ODK_EventLog(odkEventDesc, odkEventType, odkEventTime) VALUES (%s, %s, %s)"
        par = (stderr,"Error", timeFMT)
        cursor.execute(sql, par)
        db.commit()
    except (MySQLdb.Error, MySQLdb.Warning) as e:
        db.rollback()
    cleanup()
else:
    try:
        sql = "INSERT INTO ODK_EventLog(odkEventDesc, odkEventType, odkEventTime) VALUES (%s, %s, %s)"
        par = ("Briefcase Export Completed Successfully", "Information", timeFMT)
        cursor.execute(sql, par)
        db.commit()
    except (MySQLdb.Error, MySQLdb.Warning) as e:
        db.rollback()
    # check if previous file exists from above operations and create delta file of new entries
    if os.path.isfile(odkBCExportPrevious) == True:
        try:
            with open(odkBCExportPrevious, "rb") as t1, open(odkBCExportNewFile, "rb") as t2:
                fileone = t1.readlines()
                filetwo = t2.readlines()
                header = filetwo[0]
            with open(openVAReadyFile, "wb") as outFile:
                outFile.write(header)
                for line in filetwo:
                    if line not in fileone:
                        outFile.write(line) 
        except:
            try:
                sql = "INSERT INTO ODK_EventLog(odkEventDesc, odkEventType, odkEventTime) VALUES"
                par = ("Could not create OpenVAReadyFile.csv", "Error", timeFMT)
                cursor.execute(sql, par)
                db.commit()
            except (MySQLdb.Error, MySQLdb.Warning) as e:
                db.rollback()
            cleanup()
    else:
        # if there is no pre-existing ODK Briefcase Export file, then copy and rename to OpenVAReadyFile.csv
        try:
            shutil.copy(odkBCExportNewFile, openVAReadyFile)
        except (IOError, OSError, shutil.Error) as e:
            try:
                sql = "INSERT INTO ODK_EventLog(odkEventDesc, odkEventType, odkEventTime) VALUES (%s, %s, %s)"
                par = (e, "Error", timeFMT)
                cursor.execute(sql, par)
                db.commit()
            except (MySQLdb.Error, MySQLdb.Warning) as e:
                db.rollback()
            cleanup()

    # if no records retrieved, then close up shop; otherwise, create R script for running openVA
    with open(openVAReadyFile, "rb") as outFile:
        nRecords = len(list(outFile))

    if nRecords == 1:
        try:
            sql = "INSERT INTO ODK_EventLog(odkEventDesc, odkEventType, odkEventTime) VALUES (%s, %s, %s)"
            par = ("No Records From Briefcase (nothing more to do)", "Information", timeFMT)
            cursor.execute(sql, par)
            db.commit()
        except (MySQLdb.Error, MySQLdb.Warning) as e:
            db.rollback()
        cleanup()
        try:
            sql = "UPDATE ODK_Conf SET odkLastRun=%s, odkLastRunResult=%s"
            par = (timeFMT,"1")
            cursor.execute(sql, par)
            db.commit()
            db.close()
            sys.exit()
        except (MySQLdb.Error, MySQLdb.Warning) as e:
            db.rollback()
            cleanup()
    try:
        f = open(rScriptIn, "wb")
        f.write("library(openVA); library(CrossVA) \n")
        f.write("getwd() \n")
        f.write("records <- read.csv('" + openVAReadyFile + "') \n")

        # InSilicoVA ## HERE -- I think you need to something like: names(data) <- tolower(data)
        if vaAlgorithm == "InSilicoVA":
            f.write("data <- map_records_insilicova(records) \n")
            f.write("results <- insilico(data=data, " + ", \n")
            f.write("\t isNumeric=" + insilico_isNumeric + ", \n")
            f.write("\t updateCondProb=" + insilico_updateCondProb + ", \n")
            f.write("\t keepProbbase.level=" + insilico_keepProbbase_level + ", \n")
            f.write("\t CondProb=" + insilico_CondProb + ", \n")
            f.write("\t CondProbNum=" + insilico_CondProbNum + ", \n")
            f.write("\t datacheck=" + insilico_datacheck + ", \n")
            f.write("\t datacheck.missing=" + insilico_datacheck_missing + ", \n")
            f.write("\t warning.write=" + insilico_warning_write + ", \n")
            f.write("\t external.sep=" + insilico_external_sep + ", \n")
            f.write("\t Nsim=" + insilico_Nsim + ", \n")
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
            f.write("sex <- ifelse(tolower(data$male)=='y', 'Male', 'Female') \n")

        # InterVA
        if vaAlgorithm == "InterVA":
           f.write("data <- map_records_interva4(records) \n")
           f.write("results <- InterVA(Input=data, \n")
           f.write("\t HIV= '" + interVA_HIV + "', \n")
           f.write("\t Malaria = '" + interVA_Malaria + "', \n")
           f.write("\t output='" + interVA_output + "', \n")
           f.write("\t groupcode=" + interVA_groupcode + ", \n")
           f.write("\t replicate=" + interVA_replicate + ", \n")
           f.write("\t replicate.bug1=" + interVA_replicate_bug1 + ", \n")
           f.write("\t replicate.bug2=" + interVA_replicate_bug2 + ", \n")
           f.write("\t write=FALSE) \n")
           f.write("sex <- ifelse(tolower(data$MALE)=='y', 'Male', 'Female') \n")

        # write results
        f.write("cod <- getTopCOD(results) \n")
        f.write("hasCOD <- data$ID %in% as.numeric(cod$ID) \n")
        f.write("dob <- as.Date(as.character(records$consented.deceased_CRVS.info_on_deceased.Id10021), '%b %d, %Y') \n")
        f.write("dod <- as.Date(as.character(records$consented.deceased_CRVS.info_on_deceased.Id10023), '%b %d, %Y') \n")
        f.write("age <- floor(records$consented.deceased_CRVS.info_on_deceased.ageInDays/365.25) \n")
        #f.write("age[is.na(age)] <- '' \n")
        f.write("outCOD <- cbind(cod, dob[hasCOD], dod[hasCOD], age[hasCOD], sex[hasCOD]) \n")
        f.write("outNoCOD <- data[!hasCOD,] \n")
        f.write("write.csv(outCOD, file='" + openVAFilesDir + "/codAssigned.csv', row.names=FALSE, na='') \n")
        f.write("write.csv(outNoCOD, file='" + openVAFilesDir + "/codNotAssigned.csv', row.names=FALSE, na='')")
        f.close()
    except:
        try:
            sql = "INSERT INTO ODK_EventLog(odkEventDesc, odkEventType, odkEventTime) VALUES (%s, %s, %s)"
            par = ("Could not create R Script File","Error", timeFMT)
            cursor.execute(sql, par)
            db.commit()
        except (MySQLdb.Error, MySQLdb.Warning) as e:
            db.rollback()
        cleanup()

    # run R script
    rBatch = "R CMD BATCH --vanilla " + rScriptIn + " " + rScriptOut
    rprocess = subprocess.Popen(rBatch, stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
    stdout, stderr = rprocess.communicate()
    rrc = rprocess.returncode
    if rrc != 0: 
        try:
            sql = "INSERT INTO ODK_EventLog(odkEventDesc, odkEventType, odkEventTime) VALUES (%s, %s, %s)"
            par = ("Could not run R Script","Error", timeFMT)
            cursor.execute(sql)
            db.commit()
        except:
            db.rollback()
            cleanup()
            
        ## HERE -- you could search output file for "Error" and "Execution halted" (last two lines) and copy Error to log
        ##         (use regulare expression -- import re and re.findall()
        else:
            try:
                sql = "INSERT INTO ODK_EventLog(odkEventDesc, odkEventType, odkEventTime) VALUES (%s, %s, %s)"
                par = ("OpenVA Analysis Completed Successfully","Information", timeFMT)
                cursor.execute(sql)
                db.commit()
            except (MySQLdb.Error, MySQLdb.Warning) as e:
                db.rollback()
                cleanup()

    # read in results
    api = Dhis(dhisURL, dhisUser, dhisPass)
    vaPrograms = api.get('programs', params={'filter': 'name:like:Verbal Autopsy'}).get('programs')
    orgUnitValid = len(api.get('organisationUnits', params={'filter': 'id:eq:{}'.format(dhisOrgUnit)})['organisationUnits']) == 1
    if not orgUnitValid:
        print('Organisation Unit UID could not be found. Exiting...')
    if not vaPrograms:
        print("'Verbal Autopsy' program not found. Exiting...")
    elif len(vaPrograms) > 1:
        print("More than one 'Verbal Autopsy' found. Exiting...")

    va_program_uid = vaPrograms[0]['id']
    icd10_options = [o['code'] for o in api.get('options', params={'filter': 'optionSet.id:eq:LAWwdYur1ds', 'fields': 'code'}).get('options')]
    algorithm_metadata_options = [o['code'] for o in api.get('options', params={'filter': 'optionSet.id:eq:Joti2JHU4i6', 'fields': 'code'}).get('options')]

    blobPath = os.path.join(dhisDir, 'blobs')
    if not os.path.isdir(blobPath):
        os.makedirs(blobPath)

    events = []
    export = {}
    resultsPath = openVAFilesDir + "/codAssigned.csv"

    with open(resultsPath, "rb") as f:
        reader = csv.reader(f)
        reader.next()
        for row in reader:
            if row[1]!="Undetermined":
                va_id = str(uuid.uuid4())
                blob_file = "{}.db".format(os.path.join(dhisDir, 'blobs', va_id))
                create_db(blob_file)
                file_id = api.post_blob(blob_file)
                icd10 = icd10OpenVA[row[1]]
                age = row[4]
                if row3 =="":
                    event_date = datetime.date(9999,9,9)
                else:
                    event_date = parser.parse(row[3])
                sex = row[5].lower()
                e = VerbalAutopsyEvent(va_id, va_program_uid, dhisOrgUnit, event_date, sex, age, icd10, algorithm_metadata_options[0], file_id)
                events.append(e.format_to_dhis2(dhisUser))
                ## HERE -- if row[1]=="Undetermined" then store these records in database for futher (manual?) processing

    export['events'] = events

    with open('events.json', 'w') as json_file:
        json.dump(export, json_file, indent=4)

    print("Import file stored at events.json - {} Proceeding to POST that import file NOW...{} Abort with CTRL+C.".format(WARNING, ENDC))
    try:
        time.sleep(5)
    except KeyboardInterrupt:
        print("POSTing events aborted. Nothing was imported to {}".format(dhisURL))

    log = api.post('events', data=export)
    # errors = [x.get('description') for x in log['response']['importSummaries'] if x.get('status', None) in {'ERROR', 'WARNING'}]
    # with open('log.json',  'w') as json_file:
    #     json.dump(log, json_file, indent=4)
    # print("Import summary: {} errors".format(len(errors)))
    # print("Log file of import stored at log.json")
    # print("\n".join(errors))
