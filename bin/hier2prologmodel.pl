#!/usr/bin/perl
use strict;

my $lasttab = 0;
my %is_ext = ();
my %is_reif = ();
my %parenth = ();
my %argh = ();
my %arity = ();
my @pos = ();
my @preds = ();
my %cmt = ();
while(<>) {
    chomp;
    next if m/^\s*$/;
    my $cmt;
    my @args = ();
    if (/(.*)\#\s*(.*)/) {
	$_ = $1;
	$cmt = $2;
    }
    if (/^(\s*)(\S+)\s*(.*)/) {
	my $tab = length($1);
	my $pred = $2;
	my $rest = $3;
	my $arity = 1;
	my @args = ();
	if ($rest =~ /^(\d+)/) {
	    $arity = $1;
	}
	else {
	    @args = split(' ',$rest);
	    $arity = scalar(@args) if $arity > 0;
	}
	if ($pred =~ /^\*(.*)/) {
	    $pred = $1;
	    $is_ext{$pred} = 1;
	}
	if ($pred =~ /^\?(.*)/) {
	    $pred = $1;
	    $is_reif{$pred} = 1;
	}
	$arity{$pred} = $arity;
	$pos[$tab] = $pred;
	$parenth{$pred} = $pos[$tab-1];
	push(@preds,$pred);
	$cmt{$pred} = $cmt;
	$argh{$pred} = \@args;
    }
}

my %childh = ();
foreach my $pred (keys %parenth) {
    my $p =$parenth{$pred};
    push(@{$childh{$p}}, $pred);
}

foreach my $pred (@preds) {
    my $wpred = safe($pred);
    printf("    $wpred/$arity{$pred},\n");
}

foreach my $pred (@preds) {
    my $wpred = safe($pred);
    my $argstrc = join(', ',map {"?$_"} @{$argh{$pred}});
    my $argstr = join(', ',map{safe($_)} @{$argh{$pred}});

    print "%% $wpred($argstrc)\n";
    my $cmt = $cmt{$pred};
    if ($cmt) {
	print "% $cmt\n";
    }
    if ($is_ext{$pred}) {
	print "% (extensional predicate - can be asserted)\n";
    }

    my $v = varstr($arity{$pred});
    if (!$is_ext{$pred} &&
	($childh{$pred} || 
	 $is_ext{$parenth{$pred}}
	)) {
	printf("\% \@see %s\n",join(', ',map { safe($_)."/".$arity{$_}} @{$childh{$pred}}))
	    if @{$childh{$pred} || []};
	foreach my $cp (@{$childh{$pred}}) {
	    if ($is_reif{$pred}) {
		my $vc = varstr($arity{$cp});
		printf("$wpred(%s($vc)) :- %s($vc).\n",safe($cp),safe($cp));
	    }
	    else {
		printf("$wpred($v) :- %s($v).\n",safe($cp));
	    }
	}
    }
    else {
	my $arity = $arity{$pred};
	printf(":- ext($wpred/%d).\n",$arity);
	printf("relation('$pred',%d).\n",$arity);
	for (my $i=0; $i<$arity; $i++) {
	    printf("attribute(%d,'$pred','%s',string).\n",$i+1,$argh{$pred}->[$i]);
	}
    }
    my $argstrsub = join(', ',map{$_ =~ s/.*://;safe($_)} @{$argh{$pred}});
    if ($is_ext{$parenth{$pred}}) {
	printf("$wpred($v) :- %s($v),subsumed_by([$v],[$argstrsub]).\n",safe($parenth{$pred}));	    
    }
    printf("valid_axiom($wpred($v)) :- subsumed_by([$v],[$argstrsub]).\n",$arity{$pred});
    print "\n";
} 
exit 0;

sub safe {
    my $s = shift;
    if ($s =~ /^[A-Z]+$/) {
	return lc($s);
    }
    if ($s =~ /^[A-Z]/) {
	#$s = "'$s'";
	$s =~ s/^([A-Z])/lc($1)/e;
    }
    # e.g. PropertyExpressions:list(PropertyExpression)
    if ($s =~ /\([A-Z]/) {
	$s =~ s/(\([A-Z])/lc($1)/e;
    }
    $s;
}

sub varstr {
    my $arity = shift;
    my @v = ();
    my $n = 'A';
    for (my $i=0;$i<$arity;$i++) {
	push(@v, $n);
	$n++;
    }
    return join(', ',@v);
}
