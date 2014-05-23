#!/usr/bin/perl

use Geo::Coordinates::UTM;

$ellipsoid = 23; #WGS-84
#$latitude = 49.10;
#$longitude = -110.18;
$latitude = 70.10;
$longitude = -156.18;

($zone,$easting,$northing)=latlon_to_utm($ellipsoid,$latitude,$longitude);

print "$zone,  $easting   $northing\n";
