from datetime import timedelta, datetime
import json
import os
import xml.etree.ElementTree as etree

import pandas as pd
import pyodbc as db
import requests

today = datetime.today()
td = timedelta(-1)
lastweek = today + td

today = today.strftime("%m/%d/%Y")
lastweek = lastweek.strftime("%m/%d/%Y")

oathurl = "https://api3.ibmmarketingcloud.com/oauth/token"
oauth_dict = {
    "grant_type": "refresh_token",
    "client_id": "1122682c-6077-49fe-aec6-df558bc1a873",
    "client_secret": "8bbe9794-22ae-4d2e-80ad-364bee540c84",
    "refresh_token": "rkGGWusB9L3_s-uKb36DFJxDelVwUIVIRPTep-EHeTfES1",
}

response = requests.post(oathurl, data=oauth_dict)
json_data = json.loads(response.text)

access_token = json_data.get("access_token")


def parse_tracking_XML(xml_file_root, tracking_cols, click_cols):
    """Parse the input XML file and store the result in a pandas
    DataFrame with the given columns.
    The first element of df_cols is supposed to be the identifier
    variable, which is an attribute of each node element in the
    XML data; other features will be parsed from the text content
    of each sub-element.
    """
    # First for loop goes through mailing
    rows = []
    for i in range(0, len(df_cols)):
        res = []
        for child in xml_file_root.findall(".//Mailing/" + tracking_cols[i]):
            res.append(child.text)
        rows.append({tracking_cols[i]: res})

    agg_mailing_df = pd.concat((pd.DataFrame(row) for row in rows), axis=1)

    # Second for loop goes through clicks to other links
    rows = []
    for i in range(0, len(click_cols)):
        res = []
        for child in xml_file_root.findall(".//Click/" + click_cols[i]):
            res.append(child.text)
        rows.append({click_cols[i]: res})

    clicks_df = pd.concat((pd.DataFrame(row) for row in rows), axis=1)

    return [agg_mailing_df, clicks_df]


api_url = "https://api-campaign-us-3.goacoustic.com/XMLAPI"
# "https://api3.silverpop.com/XMLAPI"

header = {
    "Authorization": "Bearer {}".format(access_token),
    "Content-Type": "text/xml;charset=utf-8",
}

# body = """<Envelope>
#   <Body>
#     <GetAggregateTrackingForOrg>
#       <SENT />
#       <EXCLUDE_TEST_MAILINGS />
#       <PER_CLICK />
#       <CAMPAIGN_ACTIVE />
#       <SHARED />
#       <DATE_START>{} 00:00:00</DATE_START>
#       <DATE_END>{} 23:59:59</DATE_END>
#     </GetAggregateTrackingForOrg>
#   </Body>
# </Envelope>""".format(
#     lastweek, today
# )

body = """<Envelope>
  <Body>
    <GetAggregateTrackingForUser>
      <SENT />
      <EXCLUDE_TEST_MAILINGS />
      <PER_CLICK />
      <CAMPAIGN_ACTIVE />
      <SHARED />
      <DATE_START>{} 00:00:00</DATE_START>
      <DATE_END>{} 23:59:59</DATE_END>
    </GetAggregateTrackingForUser>
  </Body>
</Envelope>""".format(
    lastweek, today
)

# body = """<Envelope><Body>
# <RawRecipientDataExport>
# <EVENT_DATE_START>12/01/2020 00:00:00</EVENT_DATE_START>
# <EVENT_DATE_END>12/02/2020 23:59:00</EVENT_DATE_END>
# <!--<MOVE_TO_FTP/>-->
# <EXPORT_FORMAT>0</EXPORT_FORMAT>
# <EMAIL>misherman@hy-vee.com</EMAIL>
# <ALL_EVENT_TYPES/>
# <!--<EVENT_TYPES>0,1,5,6</EVENT_TYPES>-->
# <EXCLUDE_DELETED/>
# <COLUMNS>
# <COLUMN>
# <NAME>First</NAME>
# </COLUMN>
# <COLUMN>
# <NAME>Last</NAME>
# </COLUMN>
# </COLUMNS>
# </RawRecipientDataExport>
# </Body></Envelope>""".format(
#     lastweek, today
# )

body = """<Envelope>
  <Body>
    <RawRecipientDataExport>
      <EVENT_DATE_START>12/01/2020 00:00:00</EVENT_DATE_START>
      <EVENT_DATE_END>12/02/2020 23:59:00</EVENT_DATE_END>
      <!--<MOVE_TO_FTP/>-->
      <EXPORT_FORMAT>0</EXPORT_FORMAT>
      <EMAIL>misherman@hy-vee.com</EMAIL>
      <ALL_EVENT_TYPES/>
      <INCLUDE_INBOX_MONITORING/>
      <COLUMNS>
        <COLUMN>
          <NAME>CustomerID</NAME>
        </COLUMN>
        <COLUMN>
          <NAME>Address</NAME>
        </COLUMN>
      </COLUMNS>
    </RawRecipientDataExport>
  </Body>
</Envelope>""".format(
    lastweek, today
)


return_xml = requests.post(api_url, headers=header, data=body)
print(return_xml.text)
# root = etree.fromstring(return_xml.text)
# print(root)
#
# df_cols = [
#     "MailingId",
#     "ReportId",
#     "MailingName",
#     "SentDateTime",
#     "NumSent",
#     "NumSupressed",
#     "NumInboxMonitored",
#     "NumBounceHard",
#     "NumBounceSoft",
#     "NumUniqueOpen",
#     "NumGrossOpen",
#     "NumUniqueClick",
#     "NumGrossClick",
#     "NumGrossAbuse",
#     "NumGrossChangeAddress",
#     "NumGrossMailBlock",
#     "NumGrossMailRestriction",
#     "NumUnsubscribes",
# ]
#
# click_cols = [
#     "MailingId",
#     "ReportId",
#     "LinkName",
#     "LinkURL",
#     "TotalHTML",
#     "TotalAOL",
#     "TotalWEB",
#     "TotalTEXT",
# ]

# df_list = parse_tracking_XML(root, tracking_cols=df_cols, click_cols=click_cols)
# tracking_df = df_list[0]
# click_df = df_list[1]

# print(tracking_df)
# print(click_df)
#
# sql_server = "hy-vee-dw-sql-server-p.database.windows.net"
# sql_database = "hy-vee-dw-db"
#
# conn = db.connect(
#     "Driver={{ODBC Driver 17 for SQL Server}};"
#     "Server={server};Database={database};Authentication=ActiveDirectoryIntegrated".format(
#         server=sql_server, database=sql_database
#     ),
#     autocommit=True,
#     timeout=52000,
# )
#
#
# def createDumpDailyQueries(daily_data):
#     """
#     creates queries to add the daily data to azure database
#     inputs: daily_data (df)
#     outputs: all_queries (list)
#     """
#     query_start = """INSERT INTO MARKETING.LOY_EmailMetrics SELECT * FROM ("""
#     all_queries = []
#     for index, row in daily_data.iterrows():
#         values = []
#         values.append(
#             f"""SELECT {row.MailingId} as [MailingId],
#           {row.ReportId} as [ReportId],
#           '{row.MailingName}' as [MailingName],
#           '{row.SentDateTime}' as [SentDateTime],
#           {row.NumSent} as [NumSent],
#           {row.NumInboxMonitored} as [NumInboxMonitored],
#           {row.NumBounceHard} as [NumBounceHard],
#           {row.NumBounceSoft} as [NumBounceSoft],
#           {row.NumUniqueOpen} as [NumUniqueOpen],
#           {row.NumGrossOpen} as [NumGrossOpen],
#           {row.NumUniqueClick} as [NumUniqueClick],
#           {row.NumGrossClick} as [NumGrossClick],
#           {row.NumGrossAbuse} as [NumGrossAbuse],
#           {row.NumGrossChangeAddress} as [NumGrossChangeAddress],
#           {row.NumGrossMailBlock} as [NumGrossMailBlock],
#           {row.NumGrossMailRestriction} as [NumGrossMailRestriction],
#           {row.NumUnsubscribes} as [NumUnsubscribes]"""
#         )
#         query = query_start + " UNION ALL ".join(values) + ") b;"
#         all_queries.append(query)
#     return all_queries
#
#
# query_list = createDumpDailyQueries(tracking_df)
#
# for i in query_list:
#     pd.io.sql.execute(i, conn)
