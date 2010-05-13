<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE stylesheet [
  <!ENTITY oboInOwl "http://www.geneontology.org/formats/oboInOwl#">
  <!ENTITY oboDir "http://purl.org/obo/">
  <!ENTITY md "http://purl.org/obo/owl/metadata#">
  <!ENTITY stats "http://purl.org/obo/owl/metadata_stats#">
  <!ENTITY xref "http://purl.org/obo/owl/">
  <!ENTITY xsd "http://www.w3.org/2001/XMLSchema#">
  <!ENTITY foaf "http://xmlns.com/foaf/0.1/">
  <!ENTITY dc "http://purl.org/dc/elements/1.1/">
  <!ENTITY doap "http://usefulinc.com/ns/doap#">
  ]>
 <xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" 
  xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#" 
  xmlns:owl="http://www.w3.org/2002/07/owl#"
  xmlns:oboInOwl="&oboInOwl;"
  xmlns:oboDir="&oboDir;"
  xmlns:md="&oboDir;metadata#"
  xmlns:stats="&oboDir;metadata_stats#"
  xmlns:foaf="&foaf;"
  xmlns:dc="&dc;"
  xmlns:doap="&doap;"
  version="1.0">
  
  <xsl:output indent="yes" method="xml"/>

  <xsl:template match="/">
    <rdf:RDF
      xmlns="&oboDir;metadata#"
      xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
      xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
      xmlns:owl="http://www.w3.org/2002/07/owl#"
      xmlns:xsd="&xsd;"
      xmlns:oboInOwl="&oboInOwl;"
      xmlns:oboDir="&oboDir;"
      xmlns:md="&oboDir;metadata#"
      xmlns:stats="&oboDir;metadata_stats#"
      xmlns:dc="&dc;"
      xml:base="&oboDir;metadata#"
      >
      <xsl:apply-templates select="obo_metadata"/>
    </rdf:RDF>
  </xsl:template>

  <xsl:template match="obo_metadata">
    <md:ExportSet>
      <rdfs:label>
        <xsl:text>OBO Ontology Exports and Metadata</xsl:text>
      </rdfs:label>
      <md:timeStarted rdf:datatype="&xsd;string">
        <xsl:value-of select="time_started"/>
      </md:timeStarted>
      <md:timeCompleted rdf:datatype="&xsd;string">
        <xsl:value-of select="time_started"/>
      </md:timeCompleted>
      <foaf:maker rdf:resource="http://www.berkeleybop.org/content/people/cjm/card#me"/>
      <xsl:apply-templates select="ont"/>
    </md:ExportSet>
  </xsl:template>
  <xsl:template match="ont">
    <md:hasResource>
      <xsl:variable name="type">
        <xsl:choose>
          <xsl:when test="type='mappings'">
            <xsl:text>md:MappingResource</xsl:text>
          </xsl:when>
          <xsl:when test="type='metadata'">
            <xsl:text>md:MetadataResource</xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:text>owl:Ontology</xsl:text>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <md:InformationEntity>
        <xsl:attribute name="rdf:about">
          <xsl:text>&oboDir;information_entity/</xsl:text>
          <xsl:value-of select="@id"/>
        </xsl:attribute>
        <md:ontology rdf:resource="&oboDir;owl/{namespace}"/>
        <md:contentType rdf:resource="{$type}"/>
        <foaf:homepage rdf:resource="{substring-after(home,'|')}"/>
        <foaf:mbox rdf:resource="mailto:{substring-after(contact,'|')}"/>
        <dc:title>
          <xsl:value-of select="title"/>
        </dc:title>
        <rdfs:label>
          <xsl:value-of select="title"/>
        </rdfs:label>
        <dc:creator>
          <doap:Project rdf:about="&oboDir;projects/{namespace}"/>
        </dc:creator>
        <dc:description>
          <xsl:value-of select="description"/>
        </dc:description>
        <xsl:for-each select="*">
          <xsl:choose>
            <xsl:when test="name(.)='export'">
              <xsl:if test="not(@problem)">
                <md:export>
                  <foaf:Document rdf:about="&oboDir;{@path}">
                    <rdfs:label>
                      <xsl:value-of select="../title"/>
                      <xsl:text> exported as </xsl:text>
                      <xsl:value-of select="@format"/>
                    </rdfs:label>
                    <dc:format rdf:resource="&md;{@format}"/>
                    <dc:source rdf:resource="{../url}"/>
                    <xsl:for-each select="@*">
                      <xsl:element name="{name(.)}">
                        <xsl:value-of select="."/>
                      </xsl:element>
                    </xsl:for-each>
                  </foaf:Document>
                </md:export>
              </xsl:if>
            </xsl:when>
            <xsl:when test="name(.)='stats'">
              <xsl:for-each select="@*">
                <xsl:element name="{name(.)}">
                  <xsl:if test="string(number(.))!='NaN'">
                    <xsl:attribute name="rdf:datatype">
                      <xsl:choose>
                        <xsl:when test="contains(.,'.')">
                          <xsl:text>&xsd;float</xsl:text>
                        </xsl:when>
                        <xsl:otherwise>
                          <xsl:text>&xsd;int</xsl:text>
                        </xsl:otherwise>
                      </xsl:choose>
                    </xsl:attribute>
                  </xsl:if>
                  <xsl:value-of select="normalize-space(.)"/>
                </xsl:element>
              </xsl:for-each>
            </xsl:when>
            <xsl:otherwise>
              <xsl:element name="{name(.)}">
                <xsl:value-of select="."/>
              </xsl:element>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:for-each>
      </md:InformationEntity>
    </md:hasResource>
  </xsl:template>
</xsl:stylesheet>

