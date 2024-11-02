<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
                xmlns:x="http://www.tei-c.org/ns/1.0"
                exclude-result-prefixes="x">
<xsl:output method="html"/>

<xsl:strip-space elements="x:cl"/>

<xsl:template match="x:teiHeader"/>

<xsl:template match="x:teiCorpus">
    <xsl:element name="table">
        <xsl:apply-templates/>
    </xsl:element>
</xsl:template>
<xsl:template match="x:TEI">
    <xsl:apply-templates/>
</xsl:template>
<xsl:template match="x:text">
    <xsl:element name="tr">
        <xsl:attribute name="data-n"><xsl:value-of select="../@n"/></xsl:attribute>
        <xsl:if test="../@corresp">
            <xsl:attribute name="data-treename"><xsl:value-of select="../@corresp"/></xsl:attribute>
            <xsl:attribute name="data-nodename"><xsl:value-of select="../@select"/></xsl:attribute>
        </xsl:if>
        <xsl:element name="th">
            <xsl:attribute name="scope">row</xsl:attribute>
            <xsl:attribute name="draggable">true</xsl:attribute>
            <xsl:value-of select="../@n"/>
        </xsl:element>
        <xsl:apply-templates/>
    </xsl:element>
</xsl:template>
<xsl:template match="x:w">
    <xsl:element name="td">
        <xsl:choose>
            <xsl:when test="parent::x:cl">
                <xsl:choose>
                    <xsl:when test="position() = '1'">
                        <xsl:attribute name="class">lemma group-start</xsl:attribute>
                    </xsl:when>
                    <xsl:when test="position() = last()">
                        <xsl:attribute name="class">lemma group-end</xsl:attribute>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:attribute name="class">lemma group-internal</xsl:attribute>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:when>
            <xsl:otherwise>
                <xsl:attribute name="class">lemma</xsl:attribute>
            </xsl:otherwise>
        </xsl:choose>
        <xsl:if test="@lemma">
            <xsl:attribute name="data-normal"><xsl:value-of select="@lemma"/></xsl:attribute>
        </xsl:if>
        <xsl:if test="@emended">
            <xsl:attribute name="data-emended">true</xsl:attribute>
        </xsl:if>
        <xsl:attribute name="data-n">
            <xsl:value-of select="@n"/>
        </xsl:attribute>
        <xsl:if test="@insignificant='true'">
            <xsl:attribute name="data-insignificant">true</xsl:attribute>
        </xsl:if>
        <xsl:if test="@binary='true'">
            <xsl:attribute name="data-binary">true</xsl:attribute>
        </xsl:if>
        <xsl:apply-templates/>
    </xsl:element>
</xsl:template>
</xsl:stylesheet>
