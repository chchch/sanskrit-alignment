<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
        xmlns:x="http://www.tei-c.org/ns/1.0"
        exclude-result-prefixes="x">
    <xsl:output indent="yes"/>
    <xsl:template match="x:span">
        <xsl:apply-templates/>
    </xsl:template>
    <xsl:template match="para[content-style][not(text())]">
        <xsl:value-of select="normalize-space(.)"/>
    </xsl:template>
    <xsl:template match="x:teiHeader">
        <xsl:text>&#10;&#32;&#32;&#32;&#32;</xsl:text>
            <xsl:copy><xsl:apply-templates select="node()|@*"/></xsl:copy>
    </xsl:template>
    <xsl:template match="x:xenoData">
        <xsl:text>&#10;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;</xsl:text>
            <xsl:copy><xsl:apply-templates select="node()|@*"/></xsl:copy>
        <xsl:text>&#10;&#32;&#32;&#32;&#32;</xsl:text>
    </xsl:template>
    <xsl:template match="x:TEI">
        <xsl:text>&#10;&#32;&#32;&#32;&#32;</xsl:text>
            <xsl:copy><xsl:apply-templates select="node()|@*"/></xsl:copy>
    </xsl:template>
    <xsl:template match="x:text">
        <xsl:text>&#10;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;</xsl:text>
            <xsl:copy><xsl:apply-templates select="node()|@*"/></xsl:copy>
        <xsl:text>&#10;&#32;&#32;&#32;&#32;</xsl:text>
    </xsl:template>
    <xsl:template match="x:w">
        <xsl:copy><xsl:apply-templates select="node()|@*"/></xsl:copy>
        <!--xsl:if test="not(position() = last())">
            <xsl:text>&#32;</xsl:text>
        </xsl:if-->
    </xsl:template>
    <xsl:template match="node()|@*">
        <xsl:copy>
            <xsl:apply-templates select="node()|@*"/>
        </xsl:copy>
    </xsl:template>
</xsl:stylesheet>
