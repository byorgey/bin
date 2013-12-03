#!/usr/bin/perl -w

$box = lc(shift @ARGV);
$boxfile = '';

if ( $box eq '' ) {
    $boxfile = 'mbox';
} else {
    open mailboxFILE,'<',"/home1/b/byorgey/.mailboxesrc" || die "Can't open mailbox file";

    while ( $line = <mailboxFILE> ) {
	($name,$file) = ($line =~ /(.*) (.*)/);
	if ( lc($name) =~ /^$box/ ) {
	    $boxfile = $file;
	}
    }
}

if ( $boxfile eq '' ) {
    $boxfile = $box;
}

system "mx $boxfile";
