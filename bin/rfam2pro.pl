#!/usr/bin/perl
use strict;

use Bio::AlignIO;

foreach my $f (@ARGV) {
    parse($f);
}

# STOCKHOLM (multiple concatenated files)
# Rfam
sub parse {
    my $f = shift;
    my $str  = Bio::AlignIO->new(
        '-file'	=> $f,
        '-format'	=> 'stockholm');

    
    while (my $aln = $str->next_aln()) {
        my $family = $aln->accession;
        my $desc = $aln->description;
        my ($ann) = $aln->annotation->get_Annotations('alignment_comment');
        my $ann_text = $ann ? $ann->text : '';
        my $meta = $aln->consensus_meta;
        my ($name) = $meta->meta_names;
        my $meta_str = $meta->named_meta('SS_cons');

        foreach my $seq ( $aln->each_seq() ) {
            my $seqstr = $seq->seq;
            my $seqid = $seq->display_id;
            #printf "%s\n%s\n", $seq->display_id, $seq->seq;
            #printf "%s %s\n", $meta_str;
            my @stack = ();
            my $last;
            my $n=0;
            for (my $i=0; $i<length($meta_str); $i++) {
                my $base = $seqid.'_'.$n;
                my $basetype = substr($seqstr,$i,1);
                next if $basetype eq '.';
                $n++;
                my $m = substr($meta_str,$i,1);
                if ($m eq '<') {
                    push(@stack, $base);
                }
                elsif ($m eq '>') {
                    my $pb = pop(@stack);
                    fact(pairs_with_CWW=>[$pb,$base]);
                }
                else {
                    # unpaired
                }
                fact(lc($basetype)=>[$base]);
                fact(part_of=>[$base,$seqid]);
                if ($last) {
                    fact(five_prime_to=>[$last,$base]);
                }
                $last = $base;
            }
        }
    }
}

exit 0;

sub prologquote {
    my $s = shift;
    my $force = shift;
    if (ref($s)) {
        if (ref($s) ne 'HASH') {
            sprintf("[%s]",
                    join(',',map{prologquote($_)} @$s));
        }
        else {
            my @keys = keys %$s;
            if (@keys == 1) {
                my $functor = $keys[0];
                my $args = $s->{$functor};
                sprintf("$functor(%s)",
                        join(', ', map {prologquote($_)} @$args));
            }
            else {
                warn "@keys != 1 - ignoring";
            }
        }
    }
    else {
        $s = '' unless defined $s;
        if ($s =~ /^[\-]?[0-9]+$/ && !$force) {
            return $s;
        }
        $s =~ s/\'/\'\'/g;
        "'$s'";
    }
}

sub nl {
    print("\n");
}

sub fact {
    my $pred = shift;
    my @args = @{shift||[]};
    my $cmt = shift;
    printf("$pred(%s).",
           join(', ', map {prologquote($_)} @args));
    nl;
}


1;
