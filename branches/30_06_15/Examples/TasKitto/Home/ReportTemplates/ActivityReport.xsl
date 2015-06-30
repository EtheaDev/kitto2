<?xml version="1.0" encoding="utf-8"?>
<?xml-stylesheet type="text/xsl"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0" xmlns:fo="http://www.w3.org/1999/XSL/Format">
  <xsl:output method="xml" version="1.0" encoding="utf-8" indent="yes"/>
  <xsl:decimal-format name="euro" decimal-separator="," grouping-separator="."/>
  <xsl:variable name="linebreak">
    <!--<xsl:text>&#xA;</xsl:text>-->
    <xsl:text>&#13;&#10;</xsl:text>
  </xsl:variable>
  <xsl:variable name="papertype">A4_landscape</xsl:variable>
  <xsl:template match="/ACTIVITIES">
    <fo:root xmlns:fo="http://www.w3.org/1999/XSL/Format">
      <fo:layout-master-set>
        <!-- layout information -->
        <fo:simple-page-master master-name="simple"
          margin-top="1cm"
          margin-bottom="1cm"
          margin-left="1cm"
          margin-right="1cm">
          <xsl:choose>
            <!-- papertype defines page layoput -->
            <xsl:when test="$papertype = 'A4_landscape'">
              <xsl:attribute name="page-height">21cm</xsl:attribute>
              <xsl:attribute name="page-width">29.7cm</xsl:attribute>
            </xsl:when>
            <xsl:when test="$papertype = 'A4_portrait'">
              <xsl:attribute name="page-height">29.7cm</xsl:attribute>
              <xsl:attribute name="page-width">21cm</xsl:attribute>
            </xsl:when>
            <xsl:otherwise>
              <!--default: A4 portrait -->
              <xsl:attribute name="page-height">29.7cm</xsl:attribute>
              <xsl:attribute name="page-width">21cm</xsl:attribute>
            </xsl:otherwise>
          </xsl:choose>
		  <fo:region-body margin-top="1.5cm" margin-bottom="0.5cm"/>
          <fo:region-before extent="1.5cm"/>
		  <fo:region-after extent="0.5cm"/>
        </fo:simple-page-master>
      </fo:layout-master-set>
      <!-- end: layout information -->
      <!-- start page-sequence -->
      <fo:page-sequence master-reference="simple">
        <fo:static-content flow-name="xsl-region-before">
          <!-- table start -->
          <fo:table table-layout="fixed" width="280mm">
            <fo:table-column column-width="95mm"/>
            <fo:table-column column-width="185mm"/>
            <fo:table-body>
              <fo:table-row>
                <fo:table-cell >
                  <!-- title -->
                  <fo:block font-size="14pt"
                font-family="sans-serif"
                line-height="14pt"
                space-after.optimum="10pt"
                color="black"
                text-align="left"
                font-weight="bold">
            ACTIVITY LIST
          </fo:block>
                  <!-- subtitle -->
                  <fo:block font-size="10pt"
                font-family="sans-serif"
                line-height="12pt"
                color="black"
                text-align="left">
            Activity list by date
          </fo:block>
                </fo:table-cell>
                <!-- logo image -->
                <fo:table-cell >
                  <fo:block text-align="end">
                    <fo:external-graphic src="%FILENAME_TO_URL(%APP_PATH%Resources/taskitto_logo_150.png)%" content-height="8mm" height="8mm"/>
                  </fo:block>
                </fo:table-cell>
              </fo:table-row>
            </fo:table-body>
          </fo:table>
          <!-- table end -->
        </fo:static-content>
        <!-- header end -->
        <!-- footer -->
        <fo:static-content flow-name="xsl-region-after">
          <!-- table start -->
        <fo:table table-layout="fixed" width="280mm">
          <fo:table-column column-width="95mm"/>
          <fo:table-column column-width="185mm"/>
            <fo:table-body>
              <fo:table-row>
                <fo:table-cell >
                  <fo:block text-align="left"
            font-size="8pt"
            font-family="sans-serif"
            color="black"
            background-color="white"
            line-height="14pt" >
            Report generated on %DATETIME%
          </fo:block>
                </fo:table-cell>
                <fo:table-cell >
                  <fo:block text-align="end"
            font-size="8pt"
            font-family="sans-serif"
            color="black"
            background-color="white"
            line-height="14pt" >
              Pag.<fo:page-number/> / <fo:page-number-citation ref-id="end-of-document"/>
                  </fo:block>
                </fo:table-cell>
              </fo:table-row>
            </fo:table-body>
          </fo:table>
          <!-- table end -->
        </fo:static-content>
        <!-- footer end -->
        <fo:flow flow-name="xsl-region-body">
          
<fo:table table-layout="fixed" width="190mm" font-size="8pt" font-family="sans-serif">
<fo:table-column column-width="40mm" /> <!--colonna ACTIVITY.DESCRIPTION-->
<fo:table-column column-width="40mm" /> <!--colonna ACTIVITY.PHASE-->
<fo:table-column column-width="60mm" /> <!--colonna ACTIVITY.EMPLOYEE-->
<fo:table-column column-width="40mm" /> <!--colonna ACTIVITY.ROLE-->
<fo:table-column column-width="40mm" /> <!--colonna ACTIVITY.TYPE-->
<fo:table-column column-width="20mm" /> <!--colonna ACTIVITY.ACTIVITY_DATE-->
<fo:table-column column-width="20mm" /> <!--colonna ACTIVITY.START_TIME-->
<fo:table-column column-width="20mm" /> <!--colonna ACTIVITY.END_TIME-->

  <fo:table-header>
<fo:table-row color="white" background-color="blue">
<fo:table-cell border-style="solid" border-color="blue" text-align="left">
                  <fo:block>Description</fo:block>
                </fo:table-cell>
<fo:table-cell border-style="solid" border-color="blue" text-align="left">
                  <fo:block>Phase</fo:block>
                </fo:table-cell>
<fo:table-cell border-style="solid" border-color="blue" text-align="left">
                  <fo:block>Employee</fo:block>
                </fo:table-cell>
<fo:table-cell border-style="solid" border-color="blue" text-align="left">
                  <fo:block>Role</fo:block>
                </fo:table-cell>
<fo:table-cell border-style="solid" border-color="blue" text-align="left">
                  <fo:block>Type</fo:block>
                </fo:table-cell>
<fo:table-cell border-style="solid" border-color="blue" text-align="center">
                  <fo:block>Activity Date</fo:block>
                </fo:table-cell>
<fo:table-cell border-style="solid" border-color="blue" text-align="center">
                  <fo:block>Start Time</fo:block>
                </fo:table-cell>
<fo:table-cell border-style="solid" border-color="blue" text-align="center">
                  <fo:block>End Time</fo:block>
                </fo:table-cell>

</fo:table-row>  </fo:table-header>
  <fo:table-body>
    <xsl:for-each select="ACTIVITY">
                <fo:table-row>
                  <fo:table-cell text-align="left" border-style="solid" border-color="white">
<fo:block>
<xsl:value-of select="DESCRIPTION"/>
</fo:block>
</fo:table-cell>
                  <fo:table-cell text-align="left" border-style="solid" border-color="white">
<fo:block>
<xsl:value-of select="PHASE"/>
</fo:block>
</fo:table-cell>
                  <fo:table-cell text-align="left" border-style="solid" border-color="white">
<fo:block>
<xsl:value-of select="EMPLOYEE"/>
</fo:block>
</fo:table-cell>
                  <fo:table-cell text-align="left" border-style="solid" border-color="white">
<fo:block>
<xsl:value-of select="ROLE"/>
</fo:block>
</fo:table-cell>
                  <fo:table-cell text-align="left" border-style="solid" border-color="white">
<fo:block>
<xsl:value-of select="TYPE"/>
</fo:block>
</fo:table-cell>
                  <fo:table-cell text-align="center" border-style="solid" border-color="white">
<fo:block>
<xsl:value-of select="ACTIVITY_DATE"/>
</fo:block>
</fo:table-cell>
                  <fo:table-cell text-align="center" border-style="solid" border-color="white">
<fo:block>
<xsl:value-of select="START_TIME"/>
</fo:block>
</fo:table-cell>
                  <fo:table-cell text-align="center" border-style="solid" border-color="white">
<fo:block>
<xsl:value-of select="END_TIME"/>
</fo:block>
</fo:table-cell>
                </fo:table-row>    </xsl:for-each>
  </fo:table-body>
</fo:table>
<fo:block space-after.optimum="5pt"></fo:block>

          <fo:block id="end-of-document">
            <fo:marker marker-class-name="term">
              <fo:instream-foreign-object>
                <svg width="15cm" height="1cm" xml:space="preserve" xmlns="http://www.w3.org/2000/svg">
                  <rect style="fill:white;stroke:white" x="0" y="0" width="15cm" height="1cm"/>
                </svg>
              </fo:instream-foreign-object>
            </fo:marker>
          </fo:block>
        </fo:flow>
        <!-- closes the flow element-->
      </fo:page-sequence>
      <!-- closes the page-sequence -->
    </fo:root>
  </xsl:template>
</xsl:stylesheet>
