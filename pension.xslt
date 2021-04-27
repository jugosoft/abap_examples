<xsl:transform xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 xmlns:sap="http://www.sap.com/sapxsl"
 xmlns:asx="http://www.sap.com/abapxml"  version="1.0">
  <xsl:strip-space elements="*"/>
  <xsl:template match="/">
    <ЭДПФР> 
      <ЗППВ>
        <xsl:for-each select="/asx:abap/asx:values/ROOT/item">
          <Гражданин>
            <УТ:ФИО>
              <УТ:Фамилия>
                <xsl:value-of select="NAME1"/>
              </УТ:Фамилия>
              <УТ:Имя>
                <xsl:value-of select="NAME2"/>
              </УТ:Имя>
              <УТ:Отчество>
                <xsl:value-of select="NAME3"/>
              </УТ:Отчество>
            </УТ:ФИО>
            <УТ:ДатаРождения>
              <xsl:value-of select="DATE_OF_BIRTH"/>
            </УТ:ДатаРождения>
            <УТ:СНИЛС>
              <xsl:value-of select="SNILS"/>
            </УТ:СНИЛС>
          </Гражданин>
        </xsl:for-each>
      </ЗППВ>
    </ЭДПФР>
  </xsl:template>
</xsl:transform>