<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
        xmlns:x="http://www.tei-c.org/ns/1.0"
        exclude-result-prefixes="x">
<xsl:output method="text" omit-xml-declaration="yes"/>
    <xsl:strip-space elements="*"/>
    <xsl:template match="/">
        <xsl:apply-templates/>
    </xsl:template>
    <xsl:template match="para[content-style][not(text())]">
        <xsl:value-of select="normalize-space(.)"/>
    </xsl:template>
    <xsl:template match="x:teiCorpus">
        <xsl:apply-templates/>
    </xsl:template>
    <xsl:template match="x:TEI">
        <xsl:value-of select="@n"/><xsl:text>,</xsl:text>
        <xsl:apply-templates/>
        <xsl:text>&#xa;</xsl:text>
    </xsl:template>
    <xsl:template match="x:text">
        <xsl:apply-templates/>
    </xsl:template>
    <xsl:template match="x:w">
        <xsl:text>"</xsl:text><xsl:apply-templates/><xsl:text>"</xsl:text>
        <!--xsl:if test="not(position() = last())"-->
            <xsl:text>,</xsl:text>
        <!--/xsl:if-->
    </xsl:template>
    <!--xsl:template match="node()|@*">
        <xsl:copy>
            <xsl:apply-templates select="node()|@*"/>
        </xsl:copy>
    </xsl:template-->
</xsl:stylesheet>
