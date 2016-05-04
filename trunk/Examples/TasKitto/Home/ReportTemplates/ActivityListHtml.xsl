<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="html" version="1.0" encoding="UTF-8" indent="yes"/>
  <xsl:template match="/ACTIVITIES">
<style type="text/css">
.Table_Header {
	font-family: Arial, Helvetica, sans-serif;
	font-size: 12px;
	font-weight: bold;
	color: #FFFFFF;
	background-color: #3300FF;
	padding: 2px;
	border: thin solid #0066FF;
}
.Table_Body {
	font-family: Arial, Helvetica, sans-serif;
	font-size: 12px;
	font-weight: bold;
	color: #333333;
	background-color: #FFFF99;
	padding: 2px;
	border: thin solid #FFCC00;	
}
.ReportTitle {
	font-family: Arial, Helvetica, sans-serif;
	font-size: medium;
	font-weight: bold;
	color: #000033;
}
</style>
<div>
<table width="100%"  cellpadding="0" cellspacing="0" border="0">
<tr>
<td width="33%">
<p align="left" class="ReportTitle">ACTIVITY LIST</p>
</td>
<td width="34%">
<p align="center" class="ReportTitle">Activity list by Date</p>
</td>
<td width="33%">
    <p align="right"><img src="/TasKitto/taskitto_logo_150.png" /></p>
</td>
</tr>
</table>
<table width="100%" cellpadding="0" cellspacing="0" border="0">
  <tr>
    <td width="15%" class="Table_Header">Description</td>
    <td width="15%" class="Table_Header">Phase</td>
    <td width="20%" class="Table_Header">Employee</td>
    <td width="15%" class="Table_Header">Role</td>
    <td width="15%" class="Table_Header">Type</td>
    <td width="10%" class="Table_Header">Activity Date</td>
    <td width="5%" class="Table_Header">Start Time</td>
    <td width="5%" class="Table_Header">End Time </td>
  </tr>
<xsl:for-each select="ACTIVITY">
  <tr>
    <td class="Table_Body"><xsl:value-of select="DESCRIPTION"/></td>
    <td class="Table_Body"><xsl:value-of select="PHASE"/></td>
    <td class="Table_Body"><xsl:value-of select="EMPLOYEE"/></td>
    <td class="Table_Body"><xsl:value-of select="ROLE"/></td>
    <td class="Table_Body"><xsl:value-of select="TYPE"/></td>
    <td class="Table_Body"><xsl:value-of select="ACTIVITY_DATE"/></td>
    <td class="Table_Body"><xsl:value-of select="START_TIME"/></td>
    <td class="Table_Body"><xsl:value-of select="END_TIME"/></td>
  </tr>
</xsl:for-each>
</table>
</div>
  </xsl:template>
</xsl:stylesheet>