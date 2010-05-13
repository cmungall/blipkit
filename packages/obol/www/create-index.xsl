<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

  <xsl:output method="html"/>

  <xsl:variable name="all_extends" select="//extends[not(.=preceding::extends) and not (contains(.,'_xp'))]"/>
  <xsl:variable name="all_uses" select="//uses[not(.=preceding::uses)]"/>

  <xsl:template match="/">
    <html>
      <head profile="http://www.berkeleybop.org/ontologies/obo-all/ontology_index.rdf">
        <title>Obol</title> 
        <link href="stylesheet.css" rel="stylesheet" type="text/css" media="screen"/>
        <link rel="meta" type="application/rdf+xml" href="obo-all/ontology_index.rdf" />
        <link rel="meta" type="application/rdf+xml" title="FOAF" href="http://www.berkeleybop.org/content/people/cjm/chris-mungall-foaf.rdf"/>
        <meta http-equiv="content-type" content="text/html; charset=iso-8859-1"/>
        <meta name="description" content="The ontologies available from the Open Biomedical Ontologies project."/>
        <meta name="keywords" content="ontology, ontologies, owl, obo, biological, biomedical, terminology, rdf, download, open-source"/>
        <meta name="dc.creator" content="Chris Mungall"/>
        <style type="text/css">
          <xsl:comment>
.metadata p
{
        font-size: 75%;
}
.item
{
        font-size: 125%;
}
li
{	
	padding: 0;
	margin: 0;
}
.formatlist li
{
        font-size: 80%;
        margin-left: 8px;
}
.caveats li
{
            list-style: disc;
            margin: 8px;
}
.warning
{
            color: #f00;
            margin-left: 24px;
}

#dhtmltooltip
{
        font-size: 80%;
            position: absolute;
            width: 150px;
            border: 2px solid black;
            padding: 2px;
            background-color: lightyellow;
            visibility: hidden;
            z-index: 100;
            /*Remove below line to remove shadow. Below line should always appear last within this CSS*/
            filter: progid:DXImageTransform.Microsoft.Shadow(color=gray,direction=135);
}
          </xsl:comment>
        </style>

      </head>
      <body>
        <div id="dhtmltooltip"></div>

        <script type="text/javascript">
          <xsl:text>
<![CDATA[
/***********************************************
* Cool DHTML tooltip script- © Dynamic Drive DHTML code library (www.dynamicdrive.com)
* This notice MUST stay intact for legal use
* Visit Dynamic Drive at http://www.dynamicdrive.com/ for full source code
***********************************************/

var offsetxpoint=-60 //Customize x offset of tooltip
var offsetypoint=20 //Customize y offset of tooltip
var ie=document.all
var ns6=document.getElementById && !document.all
var enabletip=false
if (ie||ns6)
var tipobj=document.all? document.all["dhtmltooltip"] : document.getElementById? document.getElementById("dhtmltooltip") : ""

function ietruebody(){
return (document.compatMode && document.compatMode!="BackCompat")? document.documentElement : document.body
}

function ddrivetip(thetext, thecolor, thewidth){
if (ns6||ie){
if (typeof thewidth!="undefined") tipobj.style.width=thewidth+"px"
if (typeof thecolor!="undefined" && thecolor!="") tipobj.style.backgroundColor=thecolor
tipobj.innerHTML=thetext
enabletip=true
return false
}
}

function positiontip(e){
if (enabletip){
var curX=(ns6)?e.pageX : event.clientX+ietruebody().scrollLeft;
var curY=(ns6)?e.pageY : event.clientY+ietruebody().scrollTop;
//Find out how close the mouse is to the corner of the window
var rightedge=ie&&!window.opera? ietruebody().clientWidth-event.clientX-offsetxpoint : window.innerWidth-e.clientX-offsetxpoint-20
var bottomedge=ie&&!window.opera? ietruebody().clientHeight-event.clientY-offsetypoint : window.innerHeight-e.clientY-offsetypoint-20

var leftedge=(offsetxpoint < 0)? offsetxpoint*(-1) : -1000

//if the horizontal distance isn't enough to accomodate the width of the context menu
if (rightedge<tipobj.offsetWidth)
//move the horizontal position of the menu to the left by it's width
tipobj.style.left=ie? ietruebody().scrollLeft+event.clientX-tipobj.offsetWidth+"px" : window.pageXOffset+e.clientX-tipobj.offsetWidth+"px"
else if (curX<leftedge)
tipobj.style.left="5px"
else
//position the horizontal position of the menu where the mouse is positioned
tipobj.style.left=curX+offsetxpoint+"px"

//same concept with the vertical position
if (bottomedge<tipobj.offsetHeight)
tipobj.style.top=ie? ietruebody().scrollTop+event.clientY-tipobj.offsetHeight-offsetypoint+"px" : window.pageYOffset+e.clientY-tipobj.offsetHeight-offsetypoint+"px"
else
tipobj.style.top=curY+offsetypoint+"px"
tipobj.style.visibility="visible"
}
}

function hideddrivetip(){
if (ns6||ie){
enabletip=false
tipobj.style.visibility="hidden"
tipobj.style.left="-1000px"
tipobj.style.backgroundColor=''
tipobj.style.width=''
}
}

document.onmousemove=positiontip
  ]]>
          </xsl:text>
        </script>
        <div class="content">
          <h1>Obol</h1>
          <p>
            Obol is the name of a piece of software for automatically
            generating <a
            href="http://www.bioontology.org/wiki/index.php/XP:Main_Page">cross-product
            definitions</a> (aka genus-differentia definitions) from
          the names of terms/classes in <a
          href="http://obofoundry.org">OBO ontologies</a>. It relies
          on (a) a consistent grammatical style employed in naming
          terms or exact synonyms (b) Consistent naming across ontologies
          </p>

          <p>
            Obol generates results in .obo format, which can also be
            converted to owl. The results are in the form of
            genus-differentia definitions, which can then be fed into
            a reasoner to check for consistency between and within
            ontologies, or can be used for cross-ontology queries
          </p>
          <p>
            Obol will also generate synonyms for terms, based on the
            synonyms of the cross-product component terms. It will
            also (to a lesser extent) generate textual definitions,
            based on the xp definition, and the standard definition
            forms used in GO
          </p>

          <h3>Page contents</h3>
          <ul>
            <li><a href="#summary">Result Summary</a></li>
            <li><a href="#grid">Cross-product Grid</a></li>
            <li><a href="#results">Results Details</a></li>
            <li><a href="#using">Using the results in OBO-Edit/Protege/SWOOP</a></li>
          </ul>
          <a name="summary"/>
          <h2>Result Summary</h2>

          <p>
            These results are for automated Obol runs - they are
            unvetted. For vetted/curated cross-products, please see
            the <a
            href="http://www.berkeleybop.org/ontologies#logical_definitions">logical
            definitions</a> section of the <a
            href="http://www.berkeleybop.org/ontologies">Berkeley OBO
            Download Matrix</a>.
        </p>

          <ul>
            <xsl:apply-templates mode="contents" select="obo_metadata/ont">
              <xsl:sort select="@id"/>
            </xsl:apply-templates>
          </ul>

          <a name="grid"/>
          <h2>Grid View</h2>
          <p>
            Each row shows an OBO ontology that has been
            enhanced/extended with cross-products. Each column shows
            an OBO ontology that is required in some cross-product
            file. Each cell shows a cross-product file; a file can be
            repeated across columns, but not across rows - it only
            "enhances" one OBO ontology. Click on the file for more info.
          </p>
          <table class="grid" border="1">
            <tr>
              <th></th>
              <th></th>
              <th class="outer" colspan="50">{requires}</th>
            </tr>
            <tr>
              <th class="outer" rowspan="50">{extends}</th>
              <th></th>
              <xsl:for-each select="$all_uses">
                <xsl:sort select="."/>
                <th>
                  <xsl:apply-templates select="." mode="ont-link"/>
                </th>
              </xsl:for-each>
            </tr>
            <xsl:for-each select="$all_extends">
              <xsl:sort select="."/>

              <xsl:variable name="e">
                <xsl:value-of select="."/>
              </xsl:variable>
              <tr>
                <th>
                  <xsl:apply-templates select="." mode="ont-link"/>
                </th>
                <xsl:for-each select="$all_uses">
                  <xsl:sort select="."/>

                  <xsl:variable name="u">
                    <xsl:value-of select="."/>
                  </xsl:variable>
                  <td>
                    <xsl:text> </xsl:text>
                    <xsl:for-each select="//obo_metadata/ont">
                      <xsl:for-each select="extends">
                        <xsl:if test=".=$e">
                          <xsl:if test="$e=$u">
                            <a href="#{../@id}">
                              <xsl:value-of select="../@id"/>
                            </a>
                            <br/>
                          </xsl:if>
                          <xsl:for-each select="../uses">
                            <!-- everything uses itself -->
                            <xsl:if test=".=$u">
                              <a href="#{../@id}">
                                <xsl:value-of select="../@id"/>
                              </a>
                              <br/>
                            </xsl:if>
                          </xsl:for-each>
                        </xsl:if>
                      </xsl:for-each>
                    </xsl:for-each>
                  </td>
                </xsl:for-each>
              </tr>
            </xsl:for-each>
          </table>

          <a name="details"/>
          <h2>Details</h2>
          <xsl:apply-templates select="obo_metadata/ont">
            <xsl:sort select="@id"/>
          </xsl:apply-templates>

          
          
          <div class="metadata">
            Export started on:
            <xsl:value-of select="obo_metadata/time_started"/>
            Completed on:
            <xsl:value-of select="obo_metadata/time_completed"/>
          </div>

          <a name="using"/>
          <h2>Using the results</h2>
          <p>
            Obol produces genus-differentia definitions using the .obo <b>intersection_of</b> tag. For example, the term <i>oocyte growth</i> would be parsed to the following:

          <pre>
[Term]
id: GO:0001555 ! oocyte growth
intersection_of: GO:0040007 ! growth
intersection_of: OBO_REL:results_in_increase_in_mass_of CL:0000023 ! oocyte
          </pre>

          This can be read as:
          <div class="note">
            <b>oocyte growth</b> is a <b>growth</b> that <i>results_in_increase_in_mass_of</i> an <b>oocyte</b>
          </div>

          You can browse the .obo files directly, or load them into
          oboedit. If you do this, it is recommended you load the
          <b>.imports.obo</b> file - this has processing instructions
          for loading all the dependent cross-product ontologies

          </p>
          <p>
            You can also use an OWL client, such as Protege or SWOOP. In this case, load the .owl file. Again, you will probably want to use the <b>.imports.owl</b> file
          </p>
          
          <a name="formats"/>
          <h2>Formats</h2>
          <p>
            The following formats are generated. See below for
            caveats. All indexed ontologies can be downloaded
            en-masse for any one particular format, see download
            section below. The format metadata spec can be found <a
            rel="meta" type="application/rdf+xml"
            href="http://purl.org/obo/metadata#">Here</a>
          </p>
          <ul class="formatlist">
            <li>
              <div class="item">obo</div> -- The <a href="http://www.geneontology.org/GO.format.obo-1_2.shtml">Obo text</a> format. If this is not the source format, this is generated using flat2obo (part of obo-edit)
            </li>
            <li>
              <div class="item">obo_xml</div> -- The <a href="http://www.godatabase.org/dev/xml/dtd/obo-xml.dtd">Obo XML</a> format. Generated using <a href="http://www.godatabase.org/dev/go-perl/doc/go-perl-doc.html">go-perl</a>
            </li>
            <li>
              <div class="item">owl</div> -- See <a href="http://www.w3.org/TR/owl-features">W3 OWL page</a> for details. Generated from obo-xml using <a href="http://www.godatabase.org/dev/xml/xsl/oboxml_to_owl.xsl">oboxml_to_owl.xsl</a>. See <a href="http://xrl.us/oboinowl">OboInOwl</a> for details of mapping. All obo files converted to owl are made available using the <a href="http://purl.org/obo/">http://purl.org/obo/</a> URI scheme; for example <a href="http://purl.org/obo/GO">http://purl.org/obo/GO</a>
            </li>
            <li>
              <div class="item">chadoxml</div> -- See <a href="http://www.gmod.org/chado_xml_doc">Chado-XML</a> pages. This format can be loaded into a Chado-schema database using a generic loader such as <a href="http://search.cpan.org/~cmungall/DBIx-DBStag">DBStag</a>.
            </li>
            <li>
              <div class="item">prolog</div> -- Prolog
              database. Can be reasoned over using a prolog
              engine. See <a
              href="http://www.berkeleybop.org/obol">obol</a> and <a href="http://www.blipkit.org">blip</a>.
            </li>
            <li>
              <div class="item">owl-classified-by-pellet</div> -- result of running the OWL through the peller classifier. Potentially includes missing subclasses
            </li>
            <li>
              <div class="item">obo-classified-by-oboedit</div> -- result of running the obo through the oboedit reasoner. Potentially includes missing is_a and relationship links. For best results, see the logical definitions files.
            </li>
            <li>
              <div class="item">obo-classified-by-blip</div> -- missing subclasses and explanations of why they should be added
            </li>
          </ul>
          <h2>Presentations, articles and Papers</h2>

          <h3>Obol: integrating language and meaning in bio-ontologies</h3>
          <p>
            Appears in: <a href="http://www3.interscience.wiley.com/cgi-bin/jissue/109860809"><b>Comparative and Functional Genomics</b>; <i>Volume 5, Issue 6-7, 2004.  Pages 509-520</i></a>
          
          <ul>
            <li>
              Format:
              [
              <a href="doc/Mungall_CFG_2004.pdf">PDF</a>
              ]
            </li>
          </ul>
        </p>
        <p>
          This is a fairly CS-oriented discussion of the theory behind
          Obol. The paper also includes the initial Obol results.
        </p>

        <h3>Managing complexity in the GO</h3>
        <p>
          NOTE: the powerpoint that was previously posted here (prior to
          2005/10/22) had lots of missing slides - the correct version
          is below
        </p>
        <p>
          This presentation outlines the rationale for Obol by examining
          both the rise in complexity in GO and inconsistencies between
          the OBO cell type ontology and the implicit cell type ontology
          in GO
          <ul>
            <li>
              Format:
              [ <a href="doc/go-managing-complexity.ppt">PowerPoint</a>
            |
            <a href="doc/go-managing-complexity.pdf">PDF</a>
            ]
          </li>
        </ul>
      </p>
      <h3>Obol at Bio-Ontologies 2004</h3>
      <p>
        This presentation covers the theory behind how Obol works and
        presents some initial results, fleshed out in more detail in
        the paper below
        <ul>
          <li>
            Format:
            [
            <a href="doc/obol-glasgow.ppt">PowerPoint</a>
            |
            <a href="doc/obol-glasgow.pdf">PDF</a>
            ]
          </li>
        </ul>
      </p>
      <h3>Articles</h3>
      <p>
        <a href="http://www.the-scientist.com/2005/9/12/26/1">Your
        database is talking. Is anyone listening?</a> This article,
        which appeared in <i>The Scientist</i>, mentions Obol.
      </p>
      
        </div>
        <div style="display:none">
          <xsl:copy-of select="document('obo-all/ontology_index.rdf')" />
        </div>
        <!--
             TODO: figure out how to do this elegantly..
             -->
      </body>
    </html>
  </xsl:template>

  <xsl:template mode="copy-all" match="@*|node()">
    <xsl:copy><xsl:copy-of select="@*|node()"/><xsl:apply-templates mode="copy-all"/></xsl:copy>
  </xsl:template>

  <xsl:template mode="ont-link" match="@*|node()">
    <a>
      <xsl:attribute name="href">
        <xsl:text>http://obo.sourceforge.net/cgi-bin/detail.cgi?</xsl:text>
        <xsl:value-of select="."/>
      </xsl:attribute>
      <xsl:value-of select="."/>
    </a>
  </xsl:template>

  <xsl:template mode="contents" match="ont">
    <li>
      <a href="#{@id}">
        <xsl:value-of select="@id"/>
      </a>
    </li>
  </xsl:template>

  <xsl:template match="ont">
    <a name="{@id}"/>
    <div class="detail">
      <h3>
        <xsl:value-of select="@id"/>
        <xsl:text> : </xsl:text>
        <xsl:value-of select="title"/>
      </h3>
      <table class="info">
        
        <tr>
          <td>
            Description
          </td>
          <td class="data">
            <xsl:value-of select="description"/>
          </td>
        </tr>
        
        <tr>
          <td>
            Source
          </td>
          <td class="url">
            <a href="{url}">
              <xsl:value-of select="url"/>
            </a>
          </td>
        </tr>
        
        <tr>
          <td>
            OBO-Edit URL
          </td>
          <td class="url">
            <xsl:variable name="uri">
              <xsl:text>http://www.berkeleybop.org/obol/</xsl:text>
              <xsl:value-of select="export[@format='imports.obo']/@path"/>
            </xsl:variable>
            <a href="{$uri}">
              <xsl:value-of select="$uri"/>
            </a>
          </td>
        </tr>
        
        <tr>
          <td>
            OWL URL
          </td>
          <td class="url">
            <xsl:variable name="uri">
              <xsl:text>http://www.berkeleybop.org/obol/</xsl:text>
              <xsl:value-of select="export[@format='imports.owl']/@path"/>
            </xsl:variable>
            <a href="{$uri}">
              <xsl:value-of select="$uri"/>
            </a>
          </td>
        </tr>
        
        <tr>
          <td>
            XP-Defs
          </td>
          <td class="data">
            <xsl:value-of select="stats/@number_of_logical_definitions"/>
          </td>
        </tr>
        
        <tr>
          <td>
            Browse
          </td>
          <td class="data">
            <a href="{export[@format='xp.html']/@path}">
              <xsl:text>Static HTML Summary</xsl:text>
            </a>
          </td>
        </tr>
        
        <tr>
          <td>
            Extends
          </td>
          <td class="data">
            <ul>
              <xsl:for-each select="extends">
                <li>
                  <xsl:apply-templates select="." mode="ont-link"/>
                </li>
              </xsl:for-each>
            </ul>
          </td>
        </tr>

        <tr>
          <td>
            Uses
          </td>
          <td class="data">
            <ul>
              <xsl:for-each select="uses">
                <li>
                  <xsl:apply-templates select="." mode="ont-link"/>
                </li>
              </xsl:for-each>
            </ul>
          </td>

        </tr>

        <tr>
          <td>
            Download
          </td>
          <td class="data">
            <ul>
              <xsl:for-each select="export">
                <xsl:if test="not(@problem) and number(@size)>0 and not(@format='go_ont') 
                  and not(@format='tbl')
                  and not(@format='rdf')
                  and not(@format='imports-local.obo')
                  and not(@format='imports-local.owl')
                  and not(@format='validation_report')
                  and not(@format='pellet_report')
                  ">
                  <li>
                    <a class="tip">
                      <xsl:attribute name="onmouseover">
                        <xsl:text>ddrivetip('</xsl:text>
                        <xsl:text>Build time:</xsl:text>
                        <xsl:value-of select="@time_taken_to_generate"/>
                        <xsl:text>s</xsl:text>
                        <xsl:text> File size:</xsl:text>
                        <xsl:value-of select="@size"/>
                        <xsl:text> bytes</xsl:text>
                        <xsl:text>','yellow', 300)</xsl:text>
                      </xsl:attribute>
                      <xsl:attribute name="onmouseout">
                        <xsl:text>hideddrivetip()</xsl:text>
                      </xsl:attribute>
                      <xsl:if test="@format='owl'">
                        <xsl:attribute name="rel">
                          <xsl:text>meta</xsl:text>
                        </xsl:attribute>
                        <xsl:attribute name="type">
                          <xsl:text>application/rdf+xml</xsl:text>
                        </xsl:attribute>
                      </xsl:if>
                      <xsl:attribute name="tag">
                        <xsl:text>ontology</xsl:text>
                      </xsl:attribute>
                      <xsl:attribute name="href">
                        <xsl:value-of select="@path"/>
                      </xsl:attribute>
                      <xsl:value-of select="@format"/>
                    </a>
                    <xsl:text>  [</xsl:text>
                    <xsl:value-of select="floor(@size div 1024)"/>
                    <xsl:text> kb]</xsl:text>
                  </li>
                </xsl:if>
              </xsl:for-each>
            </ul>
          </td>
        </tr>
      </table>
    </div>
  </xsl:template>

</xsl:stylesheet>

