window.treeXSLT = 
`<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
                xmlns:x="http://www.w3.org/1999/xhtml"
                exclude-result-prefixes="x">
<xsl:output method="html" omit-xml-declaration="yes"/>

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
    <xsl:attribute name="class">tree-verse</xsl:attribute>
    <xsl:attribute name="lang">en</xsl:attribute>
    <xsl:text>[verse </xsl:text>
    <xsl:value-of select="@n"/>
    <xsl:text>]</xsl:text>
  </xsl:element>
</xsl:template>

<xsl:template match="x:l">
  <xsl:element name="span">
    <xsl:attribute name="class">l</xsl:attribute>
    <xsl:apply-templates/>
  </xsl:element>
</xsl:template>
</xsl:stylesheet>`;

const lgXSLT =
`<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
                xmlns:x="http://www.w3.org/1999/xhtml"
                exclude-result-prefixes="x">
<xsl:output method="text" omit-xml-declaration="yes"/>
<xsl:template match="x:lg">
<xsl:value-of select="@n"/>
</xsl:template>
</xsl:stylesheet>
`;
