const lemmaXSLT = 
`<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
                xmlns:x="http://www.tei-c.org/ns/1.0"
                exclude-result-prefixes="x">
<xsl:output method="html"/>
<xsl:template match="x:w">
    <xsl:element name="span">
        <xsl:attribute name="data-n"><xsl:value-of select="@n"/></xsl:attribute>
        <xsl:if test="@lemma">
            <xsl:attribute name="data-normal"><xsl:value-of select="@lemma"/></xsl:attribute>
        </xsl:if>
        <xsl:if test="@emended">
            <xsl:attribute name="data-emended"><xsl:value-of select="@emended"/></xsl:attribute>
        </xsl:if>
       <xsl:choose>
           <xsl:when test="not(normalize-space(.))">
                <xsl:attribute name="class">lemma invisible</xsl:attribute>
                <xsl:text>&#x00A0;</xsl:text>
            </xsl:when>
            <xsl:otherwise>
                <xsl:attribute name="class">lemma</xsl:attribute>
            </xsl:otherwise>
        </xsl:choose>
        <xsl:apply-templates/>
    </xsl:element>
    <xsl:text>&#173;</xsl:text>
</xsl:template>
<xsl:template match="x:cl">
    <xsl:element name="span">
        <xsl:attribute name="class">group</xsl:attribute>
        <xsl:apply-templates/>
    </xsl:element>
</xsl:template>
<xsl:template match="x:pb">
    <xsl:element name="hr">
        <xsl:attribute name="data-n"><xsl:value-of select="@n"/></xsl:attribute>
    </xsl:element>
    <xsl:element name="img">
        <xsl:attribute name="class">editbutton</xsl:attribute>
        <xsl:attribute name="data-n"><xsl:value-of select="@n"/></xsl:attribute>
        <xsl:attribute name="src">edit_icon.svg</xsl:attribute>
    </xsl:element>
    <h3 class="pb" lang="en"><xsl:value-of select="@n"/></h3>
</xsl:template>

<xsl:template match="x:lb">
<br />
<span class="lb" lang="en"><xsl:value-of select="@n"/></span>
</xsl:template>

<xsl:template match="x:fw">
<div class="fw">(<i lang="en"><xsl:value-of select="@type"/>, <xsl:value-of select="@place"/></i>): <xsl:apply-templates/></div>
</xsl:template>

<xsl:template match="x:div[@type='story']">
    <span class="story" lang="en">(story no. <xsl:value-of select="@num"/>, <xsl:value-of select="@city"/>, <xsl:value-of select="@king"/>)</span>
    <xsl:apply-templates/>
</xsl:template>

<xsl:template match="x:div[@type='verse']">
    <span class="verse"><xsl:apply-templates /></span>
</xsl:template>

<xsl:template match="x:del">
    <xsl:variable name="rend" select="@rend"/>
    <xsl:element name="del">
        <xsl:attribute name="data-title">
            <xsl:text>deleted</xsl:text>
            <xsl:if test="string($rend)">
                <xsl:text> (</xsl:text>
                <xsl:value-of select="$rend"/>
                <xsl:text>)</xsl:text>
           </xsl:if>
        </xsl:attribute>
        <xsl:apply-templates/>
    </xsl:element>
</xsl:template>

<xsl:template match="x:add">
    <xsl:variable name="place" select="@place"/>
    <xsl:variable name="rend" select="@rend"/>
    <xsl:element name="ins">
        <xsl:attribute name="data-title">
            <xsl:text>added</xsl:text>
            <xsl:if test="string($rend) or string($place)">
                <xsl:text> (</xsl:text>
                <xsl:value-of select="$rend"/>
                <xsl:if test="string($rend) and string($place)">
                    <xsl:text>, </xsl:text>
                </xsl:if>
                <xsl:value-of select="$place"/>
                <xsl:text>)</xsl:text>
            </xsl:if>
        </xsl:attribute>
        <xsl:apply-templates/>
    </xsl:element>
</xsl:template>

<xsl:template match="x:subst">
    <span class="subst"><xsl:apply-templates /></span>
</xsl:template>

<xsl:template match="x:g">
    <xsl:element name="span">
        <xsl:attribute name="data-title"><xsl:value-of select="@rend"/></xsl:attribute>
        <xsl:attribute name="class">gaiji</xsl:attribute>
        <xsl:apply-templates/>
    </xsl:element>
</xsl:template>

<xsl:template match="x:lg">
  <xsl:element name="span">
    <xsl:attribute name="class">lg</xsl:attribute>
    <xsl:apply-templates/>
  </xsl:element>
</xsl:template>

<xsl:template match="x:l">
  <xsl:element name="span">
    <xsl:attribute name="class">l</xsl:attribute>
    <xsl:apply-templates/>
  </xsl:element>
</xsl:template>
</xsl:stylesheet>`;

const oldmatrixXSLT = 
`<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
                xmlns:x="http://www.tei-c.org/ns/1.0"
                exclude-result-prefixes="x">
<xsl:output method="html"/>
<xsl:template match="x:teiCorpus">
    <div class="teicorpus"><xsl:apply-templates/></div>
</xsl:template>
</xsl:stylesheet>
`;
const oldXSLT = 
`<xsl:template match="x:teiCorpus">
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

</xsl:stylesheet>`;

const prettyXSLT =
`<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
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
</xsl:stylesheet>`;

const csvXSLT =
`<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
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
</xsl:stylesheet>`;

const matrixXSLT = 
`<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
                xmlns:x="http://www.tei-c.org/ns/1.0"
                exclude-result-prefixes="x">
<xsl:output method="html"/>
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
</xsl:stylesheet>`;

export { lemmaXSLT, prettyXSLT, matrixXSLT, csvXSLT };
