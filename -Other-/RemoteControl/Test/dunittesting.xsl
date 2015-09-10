<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="html"/>

  <xsl:template match="/">
    <html xmlns="http://www.w3.org/1999/xhtml">
      <head>
        <meta name="GENERATOR" content="XSL DUnitTesting.xsl"/>
        <meta http-equiv="Content-Type" content="text/html"/>
<!--
        <title>Test report</title>
-->
        <style>
          <xsl:comment>
            body {  
              color: #070707; 
              background-color: #ccc; 
              margin: 10px; 
              padding: 0; 
            }

            div#header { 
              display: inline; 
              width=75%; 
              background-color=white; 
            }

            div#content {
              margin: 0px; 
              border-top: 4px solid #666; 
              border-left: 4px solid #666; 
              border-right: 2px solid #666; 
              border-bottom: 2px solid #666; 
              background: #fffff7; width=75%
            }

            div#result {
              margin: 0px; 
              border-top: 4px solid #666; 
              border-left: 4px solid #666; 
              border-right: 2px solid #666; 
              border-bottom: 2px solid #666; 
              background: #9999FF; 
              width=75%
            }

            h1 {
              display: inline; 
              background: #666; 
              margin: 10pt; 
              padding: 1pt; 
              color: #ffffff;
            }

            .tableheader {
              font-size: large; 
              background-color: #9F9F9F;
            }

            .testpass {
              background-color: #00FF00;
            }

            .testerror {
              background-color: #FF0000;
            }

            .testfailure {
              background-color: #FF00FF;
            }

            .test {
              background-color: #EEEEEE;
            }

            .testsuite {
              font-size: large; 
              background-color: #DDDDDD;
            }

            .statname {
              background-color: #DDDDDD;
            }

            .statvalue {
              background-color: #FFFFFF;
            }

            .statistics {
              font-size: large;
              background-color: #DDDDDD;
            }
          </xsl:comment>
        </style>
      </head>
      <body>
        <xsl:apply-templates/>
      </body>
    </html>
  </xsl:template>

  <xsl:template match="TestRun">
    <div id="header">
      <h1>DUnitTesting - Test report</h1>
    </div>
    <div id="content">
      <blockquote>
        <table cellpadding="0" border="0" cellspacing="1" width="100%">
      	  <xsl:apply-templates/>
        </table>
      </blockquote>
    </div>
  </xsl:template>

  <xsl:template match="TestRun/TestSuite[1]">
    <tr class="tableheader">
      <td colspan="3"><xsl:value-of select="@name"/></td>
      <xsl:choose>
        <xsl:when test="@result='PASS'">
          <td colspan="2" class="testpass"><xsl:value-of select="@result"/></td>
        </xsl:when>
        <xsl:when test="@result='FAILS'">
          <td colspan="2" class="testfailure"><xsl:value-of select="@result"/></td>
        </xsl:when>
        <xsl:when test="@result='ERROR'">
          <td colspan="2" class="testerror"><xsl:value-of select="@result"/></td>
        </xsl:when>
      </xsl:choose>
    </tr>
    <tr>
    </tr>
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="TestSuite">
    <tr class="testsuite">
      <td colspan="6"><xsl:value-of select="@name"/></td>
    </tr>
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="Test">
    <tr class="test">
      <xsl:choose>
        <xsl:when test="@result='PASS'">
          <td colspan="4"><xsl:value-of select="@name"/></td>
          <td class="testpass"><xsl:value-of select="@result"/></td>
          <td><xsl:value-of select="@duration"/></td>
        </xsl:when>
        <xsl:when test="@result='FAILS'">
          <td><xsl:value-of select="@name"/></td>
          <xsl:apply-templates/>
          <td class="testfailure"><xsl:value-of select="@result"/></td>
          <td><xsl:value-of select="@duration"/></td>
        </xsl:when>
        <xsl:when test="@result='ERROR'">
          <td><xsl:value-of select="@name"/></td>
          <xsl:apply-templates/>
          <td class="testerror"><xsl:value-of select="@result"/></td>
          <td><xsl:value-of select="@duration"/></td>
        </xsl:when>
      </xsl:choose>
    </tr>
  </xsl:template>

  <xsl:template match="FailureType">
    <td><xsl:apply-templates/></td>
  </xsl:template>

  <xsl:template match="Location">
    <td><xsl:apply-templates/></td>
  </xsl:template>

  <xsl:template match="Message">
    <td><xsl:apply-templates/></td>
  </xsl:template>
  
  <xsl:template match="Statistics">
     <blockquote>
       <table width="300px">
         <tr>
           <td colspan="2" class="statistics">Statistics</td>
         </tr>
         <xsl:apply-templates/>
       </table>
    </blockquote>
  </xsl:template>

  <xsl:template match="Stat">
     <tr>
       <td class="statname" width="50px"><xsl:value-of select="@name" /></td>
       <td class="statvalue" width="200px"><xsl:value-of select="@result" /></td>
     </tr>
  </xsl:template>
</xsl:stylesheet>
