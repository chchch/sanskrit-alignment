<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
                xmlns:x="http://www.w3.org/1999/xhtml"
                exclude-result-prefixes="x">
<xsl:output method="text" omit-xml-declaration="yes"/>
<xsl:template match="x:lg">
<xsl:value-of select="@n"/>
</xsl:template>
</xsl:stylesheet>
