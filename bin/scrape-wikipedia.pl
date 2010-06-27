#!/usr/bin/perl

# a first attempt at a script to screen scrape Rfam wikipedia articles
# and store them in a DB table.
# jt6 20070524 WTSI.

use strict;
use warnings;
use Encode;

# some of these modules are only installed in our custom perl
# installation... For production we'll need either to install them
# again somewhere more general, or get ISG to install them in the core
# perl installation

use LWP;         # for making web requests
use JSON;        # NOT CORE: parsing the responses from the wp api
use URI::Escape; # NOT CORE: tidying up the URIs that we send to wp
#use DateTime;    #for getting time/formatting for cron job 

use HTML::Parser;

#binmode(STDIN, ':encoding(UTF-8)');
#binmode(STDOUT, ':encoding(UTF-8)');


$| = 1;

#-------------------------------------------------------------------------------
# configuration

# the root for all wikipedia URLs
my $WP_ROOT = 'http://en.wikipedia.org';

# the root for the Rfam family entries in WP
my $WP = "$WP_ROOT/wiki/";

# the root for the wikipedia API
my $WP_API = 'http://en.wikipedia.org/w/api.php';

#set datetime for cron job
#my $dt = DateTime->now;
#$dt->set_time_zone('UTC');
#$dt->subtract (days => 1);
#my $lastday=$dt->ymd;

# counter for changed pages;
my $changes=0;
my $unchanged=0;
my $checked=0;

#-------------------------------------------------------------------------------
# set up the HTML parser. Most of the code that uses the parser is
# taken straight from the examples in the module source package. And
# it's a bit hairy.

# set up to filter the tags and attributes

# attributes to remove
my @ignore_attr =
    qw(bgcolor background color face link alink vlink text
       onblur onchange onclick ondblclick onfocus onkeydown onkeyup onload
       onmousedown onmousemove onmouseout onmouseover onmouseup
       onreset onselect onunload
      );

# make it easier to look up attributes
my %ignore_attr = map { $_ => 1} @ignore_attr;

# tags to remove
my @ignore_tags = qw(font big small i);

# elements to remove... the distinction between these and tags being
# lost on me entirely
my @ignore_elements = qw( script style );

# this is a string that we'll populate with the HTML that drops
# through the first set of filters
my $OUTPUT  = '';

# build a new parser, set up the handlers and the lists of
# tags/elements to ignore
my $p = HTML::Parser->new(
						  start_h         => [ \&editTag, 'tokenpos, text, tag, attr' ],
						  default_h       => [ sub{ $OUTPUT .= shift }, 'text' ],
						  process_h       => [ '', '' ],
						  comment_h       => [ '', '' ],
						  ignore_tags     => \@ignore_tags,
						  ignore_elements => \@ignore_elements,
						 );
#$p->utf8_mode(0);

#----------------------------------------

# construct another parser that we'll use to fix up relative URLs

# first, build a list of HTML tags that might have an href
# attribute. This is directly from the example in the HTML::Parser
# distribution
my %link_attr;
{
  no warnings;

  # To simplify things, reformat the %HTML::Tagset::linkElements
  # hash so that it is always a hash of hashes.
  require HTML::Tagset;
  while (my($k,$v) = each %HTML::Tagset::linkElements) {
	if (ref($v)) {
	  $v = { map {$_ => 1} @$v };
	}
	else {
	  $v = { $v => 1};
	}
	$link_attr{$k} = $v;
  }
}

# somewhere to dump the output of the second filter
my $OUTPUT2 = '';

# a second parser...
my $a = HTML::Parser->new(
						  default_h => [ sub { foreach ( @_ ) { $OUTPUT2 .= $_ } }, 'text' ],
						  start_h   => [ \&editLink, 'tagname, tokenpos, text' ]
						 );

#$a->utf8_mode(1);

#-------------------------------------------------------------------------------
# set up the user agent

# create a user agent
my $ua = LWP::UserAgent->new;

# tweak the agent identity to make it look like a nice, friendly
# Mozilla browser
$ua->agent( 'Mozilla/5.001 (windows; U; NT4.0; en-us) Gecko/25250101' );

# set up the proxy
#$ua->proxy( [ 'http' ], $PROXY_URL );


#-------------------------------------------------------------------------------
# main

# get the list of pages to check

#keys ($ar)= auto_wiki AND $desc=page title
my $found = 0;
foreach my $ar ( @ARGV ) {
    if ($ar eq '-h' || $ar eq '--help') {
	print usage();
	exit 0;
    }
    my $desc = $ar;
    my $title = $ar;
  
    #sleep 2;
  ++$checked;
  # work around slashes in the title...
  my $wpid = '';
    foreach ( split "/", $title ) {
	$wpid .= uri_escape( $_, q|^A-Za-z_-| ) . '/';
  }
  # bin the 
  chop $wpid;

  # test for the existence of a page
  print STDERR "(ii) checking for existence of a WP article for $title  \"$wpid\"\n";
  unless( entryFound( $wpid ) ) {
	print STDERR "(EE) ERROR: no such entry for \"$desc\" (wp ID \"$wpid\")\n\n";
	next;
  }
    ++$found;


  #report on these revisions
  #may be multiple revisions per page since last check.
  #ONLY do the rest if the page has changed;
  if (1) {
          ++$changes;

      # scrape the HTML for this entry
      print STDERR "(ii) getting annotations\n";
      my $content;
      unless( $content = getContent( $wpid ) ) {
	  print STDERR "(WW) WARNING: couldn't retrieve content for \"$desc\"\n";
	  next;
        }

       # edit the HTML
      my $editedContent;
	  
      unless( $editedContent = editContent( $content ) ) {
	  print STDERR "(WW) WARNING: couldn't edit HTML for \"$desc\"\n";
	  next;
      }

      #update the wikitext table
          print STDERR "(ii) updating the text in wikitext table for auto_wiki \"$ar\", \"$title\" \n\n";
          #print $editedContent;
          #print encode("utf8",$editedContent);
          print encode("UTF-8",$editedContent);
          #print decode("utf8",$editedContent);
    
  }

}


if ($changes > 0) {
    print STDERR "$changes changed : $unchanged unchanged : $checked checked\n";
    print STDERR "\n";
}else{
    print STDERR "No pages changed : $checked checked\n"
    } 

if (!$found) {
    exit 1;
}

exit 0;

#-------------------------------------------------------------------------------
#- methods ---------------------------------------------------------------------
#-------------------------------------------------------------------------------
# get the changes for this page

# # use the wp API to check if a given entry changed




#-------------------------------------------------------------------------------
# retrieve the HTML for a given entry

sub getContent {
  my $wpid = shift;

  # build the URL
  my $url = $WP . $wpid;

  # build a new request object and retrieve a response from it
  my $req = HTTP::Request->new( GET => $url );
  my $res = $ua->request( $req );

  # see if the request worked...
  my $content;
  if( $res->is_success ) {
    ( $content ) = $res->content =~ m/\<\!\-\- content \-\-\>(.*)\<\!\-\- \/content \-\-\>/s;
  } else {
	print STDERR "(WW) WARNING: couldn't retrieve content for \"$wpid\": "
	             . $res->status_line . "\n";
  }

  return decode("UTF-8",$content);
}

#-------------------------------------------------------------------------------
# use the wp API to check if a given entry actually exists in wp

sub entryFound {
  my $desc = shift;

  my $url = $WP_API . "?action=query&prop=info&format=json&titles=$desc";

  my $req = HTTP::Request->new( GET => $url );
  my $res = $ua->request( $req );

  my $rv = 0;
  if( $res->is_success ) {

	# the API is returning the result of our query as a snippet of
	# JSON, looking something like this:
	#
	# {
	#   "query": {
	#              "normalized": [
	#                              {
	#                                "from": "RNase_MRP",
	#                                "to":   "RNase MRP"
	#                              }
	#                            ],
	#              "pages":      {
	#                              "11243833": {
	#                                            "pageid":    11243833,
	#                                            "ns":        0,
	#                                            "title":     "RNase MRP",
	#                                            "touched":   "2007-05-25T04:27:55Z",
	#                                            "lastrevid": 131269941,
	#                                            "counter":   0,
	#                                            "length":    1730
	#                                          }
	#                            }
	#            }
	# }
	#
	# parse it and get the resulting data structure using the JSON module
	my $result = from_json( $res->content );

	# get the list of internal wp page IDs from the JSON
	my @ids = keys %{ $result->{query}->{pages} };

	# we're expecting only a single page, so check that we can get the
	# first key from the hash containing the pages. If the page
	# doesn't exist we will get a different data structure for which
	# this process won't work
	$rv = shift @ids > 0 || 0;

  } else {
	print STDERR "(WW) WARNING: couldn't retrieve content for \"$desc\": "
                 . $res->status_line . "\n";
  }

  return $rv;
}


#-------------------------------------------------------------------------------
# Filter and edit the retrieved WP content

sub editContent {
  my $content = shift;

  # empty the output string and parse the retrieved content
  $OUTPUT = '';

  # the first pass filters out unwanted tags and attributes
  $p->parse( $content );

  # reset the parser so that we can use it again
  $p->eof;

  $OUTPUT2 = '';

  # the second pass fixes relative WP URLs and makes them absolute
  $a->parse( $OUTPUT );
  $a->eof;

  return $OUTPUT2;
}

#-------------------------------------------------------------------------------
# a handler for the HTML::Parser module. This is where we have logic
# to remove various bits and pieces of the HTML

sub editTag {
  my( $pos, $text, $tag, $attr ) = @_;

  # flag to show whether we're printing this tag or not
  my $printing = 1;

  # first, tidy the attributes
  if( @$pos >= 4 ) {

	my( $k_offset, $k_len, $v_offset, $v_len ) = @{$pos}[-4 .. -1];
	my $next_attr = $v_offset ? $v_offset + $v_len : $k_offset + $k_len;

	my $edited;
	while( @$pos >= 4 ) {

	  ( $k_offset, $k_len, $v_offset, $v_len ) = splice @$pos, -4;

	  if( $ignore_attr{ lc substr( $text, $k_offset, $k_len ) } ) {
		substr( $text, $k_offset, $next_attr - $k_offset ) = '';
		$edited++;
	  }

	  $next_attr = $k_offset;
	}

	# if we killed all attributes, kill any extra whitespace too
	$text =~ s/^(<\w+)\s+>$/$1>/ if $edited;
  }

  # print the line ?
  $OUTPUT .= $text if $printing;
	
}

#-------------------------------------------------------------------------------
# edit any tag that could have an attribute "href" and substitute a
# leading slash for the root of the WP URLs. Again, straight from the
# HTML::Parser example

sub editLink {
  my($tagname, $pos, $text) = @_;
  if (my $link_attr = $link_attr{$tagname}) {
	while (4 <= @$pos) {
	  # use attribute sets from right to left
	  # to avoid invalidating the offsets
	  # when replacing the values
	  my($k_offset, $k_len, $v_offset, $v_len) =
		splice(@$pos, -4);
	  my $attrname = lc(substr($text, $k_offset, $k_len));
	  next unless $link_attr->{$attrname};
	  next unless $v_offset; # 0 v_offset means no value
	  my $v = substr($text, $v_offset, $v_len);
	  $v =~ s/^([\'\"])(.*)\1$/$2/;
	  my $new_v = _edit($v, $attrname, $tagname);
	  next if $new_v eq $v;
	  $new_v =~ s/\"/&quot;/g;  # since we quote with ""
	  substr($text, $v_offset, $v_len) = qq("$new_v");
	}
  }
  $OUTPUT2 .= $text;
}

#-------------------------------------------------------------------------------
# a method to make the substitution in the attribute value

sub _edit {
  local $_ = shift;
  my( $attr, $tag ) = @_; 
  no strict;
  s|^(/.*)|$WP_ROOT$1|g;
  $_;
}

#-------------------------------------------------------------------------------

sub usage {
    print <<EOM
scrape-wikipedia.pl PAGENAME

e.g.
scrape-wikipedia.pl Hepatocyte

EOM
}
