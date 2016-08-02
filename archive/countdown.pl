#!/usr/bin/perl

use Date::Parse;

$thetime = str2time($ARGV[0]);
$remaining = $thetime - time();

# $pct = $remaining/18028800;
# $pct = int(100*$pct);

$negflag = 1;
if ($remaining < 0) {
  $remaining *= -1;
  $negflag = -1;
}

$seconds = $remaining % 60;
$remaining = int($remaining / 60);
$minutes = $remaining % 60;
$remaining = int($remaining / 60);
$hours = $remaining % 24;
$remaining = int($remaining / 24);
$days = $remaining;

if ($hours < 10) { $hours = '0' . $hours; }
if ($minutes < 10) { $minutes = '0' . $minutes; }
if ($seconds < 10) { $seconds = '0' . $seconds; }

$days *= $negflag;

print "$days;$hours:$minutes.$seconds";
