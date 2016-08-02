#!/usr/bin/perl

use Date::Parse;

$ptime = str2time('Sun 26 Dec 04 22:00:00 CST');
$wtime = str2time('Sat 23 Jul 05 14:00:00 CST');
$remaining = $wtime - $ptime;

print "$remaining\n";

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

print "$days;$hours:$minutes.$seconds";
