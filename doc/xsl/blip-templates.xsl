<?xml version = "1.0"?>
<xsl:stylesheet version="1.0" 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:param name="release"/>
  <xsl:output indent="yes" method="html"/>

  <xsl:template match="subsection">
    <h4 class="subsection">
      <xsl:value-of select="@id"/>
    </h4>
    <xsl:apply-templates select="*"/>
  </xsl:template>

  <xsl:template match="release">
    <a>
      <xsl:attribute name="href">
        <xsl:text>download/</xsl:text>
        <xsl:value-of select="$release"/>
        <xsl:value-of select="@type"/>
      </xsl:attribute>
      <xsl:value-of select="$release"/>
    </a>
  </xsl:template>

  <xsl:template match="lastmod">
    <div class="metadata">
      <p>
        <xsl:copy>
          <xsl:copy-of select="@*"/>
          <xsl:apply-templates/>
        </xsl:copy>
      </p>
    </div>
  </xsl:template>

  <xsl:template match="module">
    <div class="module">
      <div class="module_name">
        <xsl:text>Module:</xsl:text>
        <xsl:apply-templates select="." mode="moduleref"/>
        <xsl:text> (in </xsl:text>
        <xsl:apply-templates select="." mode="packageref"/>
        <xsl:text>)</xsl:text>
      </div>
      <div class="status">
        <xsl:copy-of select="status"/>
      </div>
      <div class="desc">
        <xsl:apply-templates select="desc"/>
      </div>

      <xsl:if test="submodules">
        <p>
          This module makes use of:
        </p>
        <xsl:apply-templates select="submodules/module"/>
      </xsl:if>

      <xsl:if test="bridges">
        <p>
          This data module can use the following inter-model bridges
        </p>
        <ul>
          <xsl:for-each select="bridges/bridge">
            <li>
              <xsl:apply-templates select="." mode="moduleref"/>
            </li>
          </xsl:for-each>
        </ul>
      </xsl:if>
    </div>
  </xsl:template>

  <xsl:template match="moduleref">
    <a>
      <xsl:attribute name="href">
        <xsl:text>http://berkeleybop.org/blipdoc/doc/users/cjm/cvs/blipkit/packages/blip/</xsl:text>
        <xsl:value-of select="ancestor-or-self::module/@package"/>
        <xsl:text>/</xsl:text>
        <xsl:value-of select="@to"/>
        <xsl:text>.pro</xsl:text>
      </xsl:attribute>
      <xsl:value-of select="@to"/>
    </a>
  </xsl:template>

  <xsl:template match="packageref">
    <a>
      <xsl:attribute name="href">
        <xsl:text>http://berkeleybop.org/blipdoc/doc/users/cjm/cvs/blipkit/packages/blip/</xsl:text>
        <xsl:value-of select="@to"/>
        <xsl:text>/</xsl:text>
      </xsl:attribute>
      <xsl:value-of select="@to"/>
    </a>
  </xsl:template>

  <xsl:template match="*" mode="packageref">
    <a>
      <xsl:attribute name="href">
        <xsl:text>http://berkeleybop.org/blipdoc/doc/users/cjm/cvs/blipkit/packages/blip/</xsl:text>
        <xsl:value-of select="@package"/>
        <xsl:text>/</xsl:text>
      </xsl:attribute>
      <xsl:value-of select="@package"/>
    </a>
  </xsl:template>

  <xsl:template match="pred">
    <a>
      <xsl:attribute name="href">
        <xsl:text>http://berkeleybop.org/blipdoc/doc_for?object=/</xsl:text>
        <xsl:value-of select="@module"/>
        <xsl:text>:</xsl:text>
        <xsl:value-of select="."/>
      </xsl:attribute>
      <xsl:value-of select="."/>
    </a>
  </xsl:template>

  <xsl:template match="use">
    <xsl:text>use_module(bio(</xsl:text>
    <a>
      <xsl:attribute name="href">
        <xsl:text>http://berkeleybop.org/blipdoc/doc/users/cjm/cvs/blipkit/packages/blip/</xsl:text>
        <xsl:value-of select="@mod"/>
        <xsl:text>.pro</xsl:text>
      </xsl:attribute>
      <xsl:value-of select="@mod"/>
    </a>
    <xsl:text>)).</xsl:text>
  </xsl:template>

  <xsl:template match="*" mode="moduleref">
    <a>
      <xsl:attribute name="href">
        <xsl:text>http://berkeleybop.org/blipdoc/doc/users/cjm/cvs/blipkit/packages/blip/</xsl:text>
        <xsl:value-of select="ancestor-or-self::module/@package"/>
        <xsl:text>/</xsl:text>
        <xsl:value-of select="@id"/>
        <xsl:text>.pro</xsl:text>
      </xsl:attribute>
      <xsl:value-of select="@id"/>
    </a>
    <xsl:text> [</xsl:text>
    <a>
      <xsl:attribute name="href">
        <xsl:text>http://berkeleybop.org/blipdoc/doc/users/cjm/cvs/blipkit/packages/blip/</xsl:text>
        <xsl:value-of select="ancestor-or-self::module/@package"/>
        <xsl:text>/</xsl:text>
        <xsl:value-of select="@id"/>
        <xsl:text>.pro?source=true</xsl:text>
      </xsl:attribute>
      <xsl:text>src</xsl:text>
    </a>
    <xsl:text>]</xsl:text>
  </xsl:template>

  <xsl:template match="kw">
    <div class="code">
      <xsl:copy-of select="."/>
    </div>
  </xsl:template>

  <xsl:template match="code">
    <div class="code">
      <xsl:if test="desc">
        <p class="desc">
          <xsl:copy-of select="desc"/>
        </p>
      </xsl:if>
      <xsl:apply-templates select="pre"/>
    </div>
  </xsl:template>

  <xsl:template match="output">
    <div class="output">
      <xsl:if test="desc">
        <p class="desc">
          <xsl:copy-of select="desc"/>
        </p>
      </xsl:if>
      <xsl:apply-templates select="pre"/>
    </div>
  </xsl:template>

  <xsl:template match="news">
    <div class="news">
      <xsl:for-each select="newsitem">
        <div class="date">
          <xsl:value-of select="@date"/>
        </div>
        <xsl:apply-templates select="."/>
      </xsl:for-each>
    </div>
  </xsl:template>

  <xsl:template match="downloadset">
    <div class="downloadset">
      <xsl:for-each select="datafile">
        <div class="datafile">
          <span class="description">
            <xsl:value-of select="."/>
          </span>
          <div class="formats">
            <xsl:text>format: [</xsl:text>
            <xsl:variable name="filename" select="@name"/>
            <xsl:for-each select="../format">
              <xsl:text> &lt;</xsl:text>
              <a href="{../@path}/{$filename}.{@type}">
                <xsl:value-of select="@type"/>
              </a>
              <xsl:text>&gt; </xsl:text>
            </xsl:for-each>
            <xsl:text>]</xsl:text>
          </div>
        </div>
      </xsl:for-each>
    </div>
  </xsl:template>

  <xsl:template match="pageref">
    <a>
      <xsl:attribute name="href">
        <xsl:value-of select="@page"/>
        <xsl:text>.html</xsl:text>
      </xsl:attribute>
      <xsl:value-of select="@page"/>
    </a>
  </xsl:template>

  <xsl:template match="url">
    <a>
      <xsl:attribute name="href">
        <xsl:choose>
          <xsl:when test="@to='SQUID'">http://selab.wustl.edu/cgi-bin/selab.pl?mode=software#squid</xsl:when>
          <xsl:when test="@to='SWI-Prolog'">http://www.swi-prolog.org</xsl:when>
          <xsl:when test="@to='blipdoc'">http://berkeleybop.org/blipdoc</xsl:when>
          <xsl:when test="@to='PrologDocNG'">http://prologdocng.sf.net</xsl:when>
          <xsl:when test="@to='LGPL'">http://www.gnu.org/copyleft/lesser.html</xsl:when>
          <xsl:when test="@to='NCBO'">http://www.ncbo.us</xsl:when>
          <xsl:when test="@to='BioPortal'">http://www.ncbo.us</xsl:when>
          <xsl:when test="@to='ZFIN'">http://zfin.org</xsl:when>
          <xsl:when test="@to='FlyBase'">http://www.flybase.org</xsl:when>
          <xsl:when test="@to='GO'">http://www.geneontology.org</xsl:when>
          <xsl:when test="@to='OBO'">http://obo.sourceforge.net</xsl:when>
          <xsl:when test="@to='SequenceOntology'">http://song.sourceforge.net</xsl:when>
          <xsl:when test="@to='amigo'">http://www.godatabase.org</xsl:when>
          <xsl:when test="@to='amigo-OBD'">http://yuri.lbl.gov/amigo/obd</xsl:when>
          <xsl:when test="@to='BioPerl'">http://www.bioperl.org</xsl:when>
          <xsl:when test="@to='go-perl'">http://www.godatabase.org/dev</xsl:when>
          <xsl:when test="@to='obo-format'">http://www.godatabase.org/dev/doc/obo_format_spec.html</xsl:when>
          <xsl:when test="@to='Chaos-XML'">http://www.fruitfly.org/chaos-xml</xsl:when>
          <xsl:when test="@to='blip'">http://www.blipkit.org</xsl:when>
          <xsl:when test="@to='obol'">http://www.fruitfly.org/~cjm/obol</xsl:when>
          <xsl:when test="@to='obo-download'">http://www.fruitfly.org/~cjm/obo-download</xsl:when>
          <xsl:when test="@to='OBD'">http://www.fruitfly.org/~cjm/obd</xsl:when>
          <xsl:when test="@to='OWL'">http://www.w3.org/TR/owl-features</xsl:when>
          <xsl:when test="@to='developer'">mailto:cjm_AT_fruitfly_DOT_org</xsl:when>

          <xsl:when test="@to='SemWeb'">http://www.swi-prolog.org/packages/semweb.html</xsl:when>
          <xsl:when test="@to='ODBC'">http://www.swi-prolog.org/packages/odbc.html</xsl:when>
          <xsl:when test="@to='Open-Bio'">http://www.open-bio.org/wiki/SourceCode</xsl:when>
          <xsl:when test="@to='svnweb'">http://code.open-bio.org/svnweb/index.cgi/blipkit/browse/trunk</xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="@to"/>
            <xsl:message terminate="no">
              <xsl:text>Bad URL ID: </xsl:text>
              <xsl:value-of select="@to"/>
            </xsl:message>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:attribute>
      <xsl:value-of select="@to"/>
    </a>
  </xsl:template>
  <!-- passthrough html -->
  <xsl:template match="*">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates/>
    </xsl:copy>
  </xsl:template>


</xsl:stylesheet>
