<xsl:transform xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:sap="http://www.sap.com/sapxsl" xmlns:asx="http://www.sap.com/abapxml" version="1.0">

  <xsl:output encoding="utf-8" indent="yes" method="xml" version="1.0"/>

  <xsl:strip-space elements="*"/>
 
  <xsl:template match="/">
    <ЭДПФР xmlns="http://пф.рф/ВВ/ПУ/ЗППВ/2018-10-19" xmlns:УТ2="http://пф.рф/УТ/2017-08-21" xmlns:АФ4="http://пф.рф/АФ/2017-08-21">
      <ЗППВ>
        <Орган>
          <xsl:value-of select="/asx:abap/asx:values/ROOT/ZPPV/ORGAN"/>
        </Орган>
        <НаименованиеОрганизации>
          <xsl:value-of select="/asx:abap/asx:values/ROOT/ZPPV/ORGNM"/>
        </НаименованиеОрганизации>
        <Дата>
          <xsl:value-of select="/asx:abap/asx:values/ROOT/ZPPV/DATUM"/>
        </Дата>
        <Номер>
          <xsl:value-of select="/asx:abap/asx:values/ROOT/ZPPV/NUMBR"/>
        </Номер>

        <xsl:for-each select="/asx:abap/asx:values/ROOT/ZPPV/PERSON/item">
          <Гражданин>
            <УТ2:ФИО>
              <УТ2:Фамилия>
                <xsl:value-of select="NAME2"/>
              </УТ2:Фамилия>
              <УТ2:Имя>
                <xsl:value-of select="NAME1"/>
              </УТ2:Имя>
              <УТ2:Отчество>
                <xsl:value-of select="NAME3"/>
              </УТ2:Отчество>
            </УТ2:ФИО>
            <УТ2:ДатаРождения>
              <xsl:value-of select="DATE_OF_BIRTH"/>
            </УТ2:ДатаРождения>
            <УТ2:СНИЛС>
              <xsl:value-of select="SNILS"/>
            </УТ2:СНИЛС>
          </Гражданин>
        </xsl:for-each>

        <Подписант>
          <УТ2:Фамилия>
            <xsl:value-of select="/asx:abap/asx:values/ROOT/ZPPV/PODPISANT/NAME1"/>
          </УТ2:Фамилия>
          <УТ2:Имя>
            <xsl:value-of select="/asx:abap/asx:values/ROOT/ZPPV/PODPISANT/NAME2"/>
          </УТ2:Имя>
          <УТ2:Отчество>
            <xsl:value-of select="/asx:abap/asx:values/ROOT/ZPPV/PODPISANT/NAME3"/>
          </УТ2:Отчество>
        </Подписант>
      </ЗППВ>
      <СлужебнаяИнформация>
        <АФ4:ДатаВремя>
          <xsl:value-of select="/asx:abap/asx:values/ROOT/SERVICE_INFORMATION/DATUM2"/>
        </АФ4:ДатаВремя>
        <АФ4:GUID>
          <xsl:value-of select="/asx:abap/asx:values/ROOT/SERVICE_INFORMATION/GUID"/>
        </АФ4:GUID>
      </СлужебнаяИнформация>
    </ЭДПФР>
  </xsl:template>


</xsl:transform>