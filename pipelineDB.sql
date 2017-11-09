--------------------------------------------------------------------------------------------------
-- Set up Pipeline database for ODK & DHIS2 configuration and event logs.
--------------------------------------------------------------------------------------------------

-- ODK configuration
CREATE TABLE ODK_Conf
(
  odkURL           char(50),
  odkUser          char(50),
  odkPass          char(50),
  odkformID        char(50),
  odkLastRun       date,
  odkLastRunResult int
);

INSERT INTO ODK_Conf
  (aggURL, aggUser, aggPass, formID, lastRun, lastRunResult)
  VALUES('http://192.168.56.101', 'aggregate', 'aggregate', 'va_who_2016_11_03_v1_4_1', '2016-04-12', '0');

-- ODK event log
CREATE TABLE ODK_EventLog
(
  odkEventDesc char(255),
  odkEventType char(255)
);

--  DHIS2 Configuration
CREATE TABLE DHIS_Conf
(
  dhisURL     char(50),
  dhisUser    char(50),
  dhisPass    char(50),
  dhisOrgUnit char(20)
);

INSERT INTO DHIS_Conf
  (dhisURL, dhisUser, dhisPass, dhisOrgUnit)
  VALUES("http://192.168.56.102:8080", "va-demo", "VerbalAutopsy99", "SCVeBskgiK6");

-- DHIS2 event log
CREATE TABLE DHIS_EventLog
(
  dhisEventDesc char(255),
  dhisEventType char(255)
);
