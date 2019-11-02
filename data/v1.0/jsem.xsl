<?xml version="1.0" encoding="UTF-8" ?>

<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="html"/>
  
  <!-- document outline --> 
  <xsl:template match="/">
    <html>
      <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
        <title>FraCas Project</title>
        <style>
          body {
            font-family: Lucida Sans, Arial, Helvetica, sans-serif;
            font-size: 12px;
          }
          td {
            font-family: Lucida Sans, Arial, Helvetica, sans-serif;
            font-size: 12px;
          }
          td.prob-id {
            width: 100px;
          }
		  
		  td.prob-language {
            width: 100px;
          }
		  
          td.prob-jsem-ans {
            width: 150px;
          }
          td.prob-type {
            width: 200px;
          }
          td.prob-phenomena {
            width: 500px;
          }
          div.comment {
            background: #cccccc;
            padding: 10px;
            margin-top: 10px;
            margin-bottom: 10px;
          }
          h1 {
            background: #ffffcc;
            padding: 10px;
            margin-top: 10px;
            margin-bottom: 10px;
          }
          h2 {
            background: #ffffcc;
            padding: 10px;
            margin-top: 10px;
            margin-bottom: 10px;
          }
          h3 {
            background: #ffffcc;
            padding: 10px;
            margin-top: 10px;
            margin-bottom: 10px;
          }
          div.problem {
            background: powderblue;
            padding: 10px;
            margin-top: 10px;
            margin-bottom: 10px;
          }
        </style>
      </head>
      <body>
        <xsl:apply-templates/>
      </body>
    </html>
  </xsl:template>
  

  <!-- problem -->
  <xsl:template match="problem">
    <div class="problem">
      <table>
        <tr>

          <!-- id -->
          <td  width="100">
            jsem-id:<xsl:value-of select="@jsem_id"/>
          </td>
		  
<!--		   <td class="prob-language">
            lang: <xsl:value-of select="@language"/>
          </td> -->


          <!-- answer -->
          <xsl:if test="@answer">
            <td width="150">
              answer: 
              <xsl:choose>
                <xsl:when test="@answer='yes'">
                  <font color="green">
                    <xsl:value-of select="@answer"/>
                  </font>
                </xsl:when>
                <xsl:when test="@answer='no'">
                  <font color="red">
                    <xsl:value-of select="@answer"/>
                  </font>
                </xsl:when>
                <xsl:when test="@answer='undef'">
                  <font color="fuchsia">
                    <xsl:value-of select="@answer"/>
                  </font>
                </xsl:when>
                <xsl:otherwise>
                  <font color="black">
                    <xsl:value-of select="@answer"/>
                  </font>
                </xsl:otherwise>
              </xsl:choose>
              <xsl:if test="@jsem_nonstandard='true'">
                **
              </xsl:if>
            </td>
          </xsl:if>

	  <!-- inference type -->
	   <td  width="200">
            inference type: <font color="darkcyan"><xsl:value-of select="@inference_type"/></font>
          </td>

          <!-- phenomena -->
          <td  width="500">
            phenomena: <font color="darkcyan"><xsl:value-of select="@phenomena"/>
                  </font>
          </td>
        </tr>
      </table>

      <table>
        <xsl:apply-templates select="link"/>
      </table>


      <table width="100%">
        <xsl:apply-templates select="p"/>
       <!-- <xsl:apply-templates select="q"/> -->
        <xsl:apply-templates select="h"/>
       <!-- <xsl:apply-templates select="a"/>  -->       
       <!--  <xsl:apply-templates select="why"/> -->
        <xsl:apply-templates select="note"/>
      </table>

    </div>
  </xsl:template>

<!-- link -->
        <xsl:template match="link">
	<tr>
	<td width="100"/>
	<td width="150">
            linked to: <font color="darkcyan"><xsl:value-of select="@resource"/>-<xsl:value-of select="@link_id"/></font>
          </td>
	<td width="200">
            literal translation?: <font color="darkcyan"><xsl:value-of select="@translation"/></font>
          </td>
	<td width="500">
            same phenomena?: <font color="darkcyan"><xsl:value-of select="@same_phenomena"/></font>
          </td>
	</tr>
  </xsl:template>


  <!-- premise -->
  <xsl:template match="p">
    <tr>
      <td width="60" valign="top">
        P<xsl:value-of select="@idx"/>
      </td>
    </tr>
	<tr>
	  <td valign="top"> script </td>
	  <td colspan="2">
        <xsl:value-of select="script"/>
      </td>
	</tr>
<!--	<tr>
	<td valign="top"> translit </td>
	  <td colspan="2">
        <xsl:value-of select="translit"/>
      </td>
	</tr>
	<tr>
	<td valign="top"> morph </td>	
      <td colspan="2">
        <xsl:value-of select="morph"/>
      </td>
	</tr> -->
	<tr>
	<td valign="top"> English </td>	
	  <td colspan="2">
        <xsl:value-of select="english"/>
      </td>
    </tr>
  </xsl:template>

<!--  question -->
  <xsl:template match="q">
    <tr>
      <td valign="top"> Q </td>
      <td colspan="2">
      </td>
    </tr>
		<tr>
	  <td valign="top"> script </td>
	  <td colspan="2">
        <xsl:value-of select="script"/>
      </td>
	</tr>
<!--	<tr>
	<td valign="top"> translit </td>
	  <td colspan="2">
        <xsl:value-of select="translit"/>
      </td>
	</tr>
	<tr>
	<td valign="top"> morph </td>	
      <td colspan="2">
        <xsl:value-of select="morph"/>
      </td>
	</tr> -->
	<tr>
	<td valign="top"> English </td>	
	  <td colspan="2">
        <xsl:value-of select="english"/>
      </td>
    </tr>
  </xsl:template>
  
  <!-- hypothesis -->
  <xsl:template match="h">
    <tr>
      <td valign="top"> H </td>
      <td colspan="2">
      </td>
    </tr>
		<tr>
	  <td valign="top"> script </td>
	  <td colspan="2">
        <xsl:value-of select="script"/>
      </td>
	</tr>
<!--	<tr>
	<td valign="top"> translit </td>
	  <td colspan="2">
        <xsl:value-of select="translit"/>
      </td>
	</tr>
	<tr>
	<td valign="top"> morph </td>	
      <td colspan="2">
        <xsl:value-of select="morph"/>
      </td>
	</tr> -->
	<tr>
	<td valign="top"> English </td>	
	  <td colspan="2">
        <xsl:value-of select="english"/>
      </td>
    </tr>
  </xsl:template>

  <!-- note -->
  <xsl:template match="note">
    <tr>
      <td valign="top"> Note </td>
      <td colspan="2">
        <i> <xsl:copy-of select="text()|node()" /> </i>
      </td>
    </tr>
  </xsl:template>
  
</xsl:stylesheet> 