#!/usr/bin/perl -w
use strict;

# In "BlipApp.pm"...
package BlipApp;
use base 'CGI::Application';
use FileHandle;
use IPC::Open2;


my $PATH_TO_BLIP = "/users/cjm/cvs/blipkit/bin/";
my $ERR_FILE = "/tmp/blip.err";
my $CMD_FILE = "/tmp/blip.cmd";

# data cache: recommended writable by wwwrun and admin
my $DATA_CACHE = "/data/blip_cache"; # todo - make configurable

my %reserved =
    (img=>1,
     cmd=>1);

# ( setup() can even be skipped for common cases. See docs below. )
sub setup {
    my $c = shift;
    $c->start_mode('blip');
    $c->mode_param('mode');
    $c->run_modes(
        'blip' => 'run_blip',
        'rest' => 'run_blip_rest',
        'mode2' => 'do_more_stuff',
        'mode3' => 'do_something_else'
        );
}

sub run_blip {
    my $self=shift;
    my $q = $self->query;

    #my $cmd = "$PATH_TO_BLIP/blip.local -r obop/cell ontol-subset -n astrocyte -to png";
    # blip sub-command; typically ontol-rest
    my $subcmd = $q->path_info;
    $subcmd =~ s/^\///;
    my $pp = '';
    if ($subcmd =~ /([\w\-]+)\/(.*)/) {
        $subcmd = $1;
        $pp = $2;
    }
    #my $cmd = "$PATH_TO_BLIP/blip.local $subcmd ";
    my $cmd =  $subcmd ? "$subcmd " : "";

    if ($pp) {
        $cmd .= " $pp";
    }
    if ($DATA_CACHE) {
	$cmd .= " -set_data_cache $DATA_CACHE";
    }

    my $is_help;
    my @params = $q->param;
    foreach my $p (@params) {
        next if $reserved{$p};
        # TODO: different way of passing params; e.g. file
        foreach my $v ($q->param($p)) {
	    if ($p eq 'keywords') {
		$cmd .= "-$v ";
		if ($v eq 'help') {
		    $is_help=1;
		}
	    }
	    else { 
		$cmd .= "-$p $v ";
	    }
        }
    }
    print STDERR "Running: $cmd\n";

    open(CW,">$CMD_FILE");
    print CW "$cmd\n";
    my $ok = open(P,"cat $CMD_FILE | $PATH_TO_BLIP/blip.local read 2> $ERR_FILE |");
    my @lines = <P>;
    $ok &&= close(P);
    my $payload = join('',@lines);
    if (!$ok || !$payload) {
        open(F,$ERR_FILE);
        $payload .= join('',<F>);
        close(F);
    }
    if ($subcmd eq 'ontol-rest' || $self->query->param('img')) {
        return "$payload";
    }
    else {
	if ($is_help) {
	    $payload = $self->parse_help($payload);
	}
        return "<pre>$payload</pre>";
    }
}

sub run_blip_rest {
    my $self=shift;
    my $q = $self->query;

    my $path = $q->path_info;

    my @params = $q->param;
    my $qs = '';
    foreach my $p (@params) {
        next if $reserved{$p};
        $qs .= '-param ';
        foreach my $v ($q->param($p)) {
	    if ($p eq 'keywords') {
                $qs .= "'$p'";
	    }
	    else { 
		$qs .= "'$p'='$v'";
	    }
        }
        $qs .= ' ';
    }

    open(CW,">$CMD_FILE");
    print CW "ontol-rest $path $qs\n";
    my $ok = open(P,"cat $CMD_FILE | $PATH_TO_BLIP/blip.local read 2> $ERR_FILE |");
    my @lines = <P>;
    $ok &&= close(P);
    my $payload = join('',@lines);
    if (!$ok || !$payload) {
        open(F,$ERR_FILE);
        $payload .= join('',<F>);
        close(F);
    }
    return "$payload";
}

sub parse_help {
    my $self = shift;
    my $txt = shift;
    my @lines = split(/\n/,$txt);
    my $in = '';
    my $s = '';
    foreach (@lines) {
	if (/^Commands:/) {
	    $in = 'c';
	}
	elsif (/^Examples:/) {
	    $in = 'e';
	}
	elsif ($in eq 'c' && /^\S+/) {
	    $_ = sprintf('<a href=\'blip.cgi/%s?help\'>%s</a>',$_,$_);
	}
	elsif ($in eq 'e' && /^\s+blip\s+(.*)/) {
	    my $optstr = $1;
	    my $cmd;
	    my $qs = '';
	    my @opts = split(' ',$optstr);
	    my $i=0; 
	    while ($i<@opts) {
		if ($opts[$i] =~ /^-(.*)/) {
		    $qs .= $1;
		    if ($opts[$i+1] !~ /^-/) {
			$i++;
			if (0 && $opts[$i] =~ /^\"(.*)/) {
			    $qs .= "=".$1;
                            if ($qs =~ /\"$/) {
                                chop $qs;
                            }
                            else {
                                $i++;
                                while ($i < @opts && $opts[$i] !~ /(.*)\"$/) {
                                    $qs .= " ".$1;
                                    $i++;
                                }
                                $qs .= " ".$opts[$i];
                            }
			}
			else {
			    $qs .= "=".$opts[$i];
			}
		    }
		}
		else {
		    if (!$cmd) {
			$cmd = $opts[$i];
		    }
		    else {
			$qs .= "ARG=$opts[$i]";
		    }
		}
		$qs .= '&';
		$i++;
	    }
	    #$_ = sprintf('<a href="blip.cgi/%s?%s">%s</a>',$cmd,$self->query->escapeHTML($qs),$_);
	    $_ = sprintf('<a href=\'blip.cgi/%s?%s\'>%s</a>',$cmd,$qs,$_);
	}
	else {
	}
	$s .= "$_\n";
    }
    return $s;
}

1;

my $webapp = BlipApp->new();
if ($webapp->query->path_info =~ /ontol\-rest/) {
    $webapp->header_type('none');
}

if ($webapp->query->param('img')) {
    $webapp->header_add( -type => 'image/png' );
}
$webapp->run();

exit 0;

