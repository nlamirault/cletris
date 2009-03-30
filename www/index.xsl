<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="html"
	      indent="yes"
	      doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN"
	      doctype-system="http://www.w3.org/TR/html4/loose.dtd"/>

  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="/">
    <html>
      <head>
	<title>
	  <xsl:value-of select="/page/@title"/>
	</title>
	<link rel="stylesheet" type="text/css" href="index.css"/>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
      </head>
      <body style="width: 100%">
	<xsl:call-template name="header"/>
	<xsl:apply-templates/>
      </body>
    </html>
  </xsl:template>

  <xsl:template match="page">
    <div id="homepage" class="main">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="blau">
    <span style="color: black">
      <xsl:apply-templates/>
    </span>
  </xsl:template>
  
  <xsl:template name="header">
    <div id="header">
      <table cellspacing="0" cellpadding="0" width="100%">
	<tr>
	  <td width="10"/>
	  <td valign="center">
	    <b><xsl:value-of select="/page/@title"/></b>
	  </td>
	</tr>
      </table>
    </div>
  </xsl:template>
</xsl:stylesheet>
