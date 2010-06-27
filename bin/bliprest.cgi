#!/usr/bin/perl -w
use strict;

# In "BlipApp.pm"...
package BlipApp;
use base 'CGI::Application';
use FileHandle;
use IPC::Open2;
use Encode;

#binmode(STDOUT, ':encoding(UTF-8)');

my $PATH_TO_BLIP = "/users/cjm/cvs/blipkit/bin/";
$ENV{PATH} = "$ENV{PATH}:$PATH_TO_BLIP";
my $ERR_FILE = "/tmp/blip.err";
my $CMD_FILE = "/tmp/blip.cmd";

# data cache: recommended writable by wwwrun and admin
my $DATA_CACHE = "/local/blip_cache"; # todo - make configurable

my %reserved =
    (img=>1,
     cmd=>1);

# ( setup() can even be skipped for common cases. See docs below. )
sub setup {
    my $c = shift;
    $c->start_mode('rest');
    $c->run_modes(
        'rest' => 'run_blip_rest',
        'mode2' => 'do_more_stuff',
        'mode3' => 'do_something_else'
        );
}


sub run_blip_rest {
    my $self=shift;
    my $q = $self->query;

    my $path = $q->path_info;

    my @params = $q->param;
    my $qs = '';
    foreach my $p (@params) {
        next if $reserved{$p};
        foreach my $v ($q->param($p)) {
            $qs .= ' -param ';
	    if ($p eq 'keywords') {
                $qs .= "$p";
	    }
	    else { 
                #$v =~ s/\s/\+/g; # TODO - full
		#$qs .= "$p=$v";
		$qs .= "\"$p=$v\"";
	    }
        }
    }

    my $wait = 1;
    while ($wait) {
        my @processes = split(/\n/,`ps auxwww | grep blip.local`);
        if (@processes < 4) {
            $wait = 0;
        }
        elsif (@processes < 10) {
            print STDERR "bliprest: Too many active processes: $#processes\n";
            sleep(1);
        }
        else {
	    # @processes >= 10
            return "Content-type: text/html\n\n<h2>Try again later - too many processes</h2>";
        }
    }

    my $ULIMIT = 300;
    if ($q->param('ulimit')) {
        $ULIMIT = $q->param('ulimit');
        print STDERR "ulimit: $ULIMIT\n";
    }
    my $cmdf = $CMD_FILE."$$";
    my $errf = $ERR_FILE."$$";
    open(CW,">$cmdf");
    print STDERR "ontol_rest '$path' $qs\n";
    print CW "ontol-rest -set_data_cache $DATA_CACHE -debug load -debug ontol_rest '$path' $qs\n";
    my $ok = open(P,"ulimit -t $ULIMIT; cat $cmdf | $PATH_TO_BLIP/blip.local read 2> $errf |");
    my @lines = <P>;
    $ok &&= close(P);
    my $payload = join('',@lines);
    if (!$ok || !$payload) {
        open(F,$errf);
        $payload .= "Content-type: text/html\n\n<h2>Encountered an error</h2>\n<pre>".join('',<F>)."</pre>";
        close(F);
    }
    print STDERR `cat $errf`; # TODO - too much info?
    unlink($cmdf);
    unlink($errf);
    #return encode("UTF-8",$payload);
    return decode("UTF-8",$payload);
    #return "$payload";
}


1;

my $webapp = BlipApp->new();
$webapp->header_type('none');
$webapp->run();

exit 0;

