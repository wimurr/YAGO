/*

SWI-Prolog code to compute distance between two locations in kilometers, given their
latitude and longitude in degrees.

*/


/*

[Adapted from the Perl code on this web page at http://www8.nau.edu/cvm/latlon_formula.html
Original text below.]

Formula and code for calculating distance based on two lat/lon locations

The following is the formula I use in perl to do the calculations.  Perl expects all of the angles to be in radians.

return &acos(cos($a1)*cos($b1)*cos($a2)*cos($b2) + cos($a1)*sin($b1)*cos($a2)*sin($b2) + sin($a1)*sin($a2)) * $r;
Where:

$a1 = lat1 in radians
$b1 = lon1 in radians
$a2 = lat2 in radians
$b2 = lon2 in radians
$r = radius of the earth in whatever units you want

The values I use for radius of the earth are:

3963.1 statute miles
3443.9 nautical miles
6378 km

To convert the decimal degrees to radians use the following perl.

# define an accurate value for PI

$pi = atan2(1,1) * 4;

#
# make sure the sign of the angle is correct for the direction
# West an South are negative angles
#

$degrees = $degrees * -1 if $direction =~ /[WwSs]/;
$radians = $degrees*($pi/180);
To convert degree minutes and seconds to decimal degrees use the following perl formula.

$dec_deg = $deg + ($min + $sec/60)/60;
Finally, there is no acos function in perl so here is the function I use.  I don't remember where I got the math for this.

# subroutine acos
#
# input: an angle in radians
#
# output: returns the arc cosine of the angle
#
# description: this is needed because perl does not provide an arc cosine function
sub acos {
   my($x) = @_;
   my $ret = atan2(sqrt(1 - $x**2), $x);
   return $ret;
}
 
*/

/*

Prolog translation:

  distance = arccos(cos(a1)*cos(b1)*cos(a2)*cos(b2) + cos(a1)*sin(b1)*cos(a2)*sin(b2) + sin(a1)*sin(a2)) * r

Where:

a1 = lat1 in radians
b1 = lon1 in radians
a2 = lat2 in radians
b2 = lon2 in radians
r = radius of the earth in whatever units you want (we will use 6378 km)

find_distance_from_radians(Lat1,Long1,Lat2,Long2,Distance) :- 
  Product1 is cos(Lat1)*cos(Long1)*cos(Lat2)*cos(Long2),
  Product2 is cos(Lat1)*sin(Long1)*cos(Lat2)*sin(Long2),
  Product3 is sin(Lat1)*sin(Lat2),
  Sum is Product1 + Product2 + Product3,
  print('acos('),
  print(Product1),
  print('+'),
  print(Product2),
  print('+'),
  print(Product3),
  print(') = '),
  Arc_cosine is acos(Product1 + Product2 + Product3)
  Distance is Arc_cosine * 6371.0.

find_distance_from_degrees(Deg_Lat1,Deg_Long1,Deg_Lat2,Deg_Long2,Distance) :- 
    convert_degrees_to_radians(Deg_Lat1,Lat1),
    convert_degrees_to_radians(Deg_Long1,Long1),
    convert_degrees_to_radians(Deg_Lat2,Lat2),
    convert_degrees_to_radians(Deg_Long2,Long2),
    find_distance_from_radians(Lat1,Long1,Lat2,Long2,Distance).

convert_degrees_to_radians(Degrees,Radians) :- Radians is Degrees * (pi/180.0).

To convert the decimal degrees to radians use the following:
radians = degrees*(pi/180);

degrees_to_radians(+degrees, -radians)
degrees_to_radians(degrees, radians) :- radians = degrees*(pi/180.0)


*/

convert_degrees_to_radians(Degrees,Radians) :- Radians is Degrees * (pi/180.0).

find_distance_from_angular_degrees(Deg_Lat1,Deg_Long1,Deg_Lat2,Deg_Long2,Distance) :- 
    convert_degrees_to_radians(Deg_Lat1,Lat1),
    convert_degrees_to_radians(Deg_Long1,Long1),
    convert_degrees_to_radians(Deg_Lat2,Lat2),
    convert_degrees_to_radians(Deg_Long2,Long2),
    find_distance_from_angular_radians(Lat1,Long1,Lat2,Long2,Distance).

find_distance_from_angular_radians(Lat1,Long1,Lat2,Long2,Distance) :- 
  Product1 is cos(Lat1)*cos(Long1)*cos(Lat2)*cos(Long2),
  Product2 is cos(Lat1)*sin(Long1)*cos(Lat2)*sin(Long2),
  Product3 is sin(Lat1)*sin(Lat2),
  print('acos('),
  print(Product1),
  print('+'),
  print(Product2),
  print('+'),
  print(Product3),
  print(') = '),
  Arc_cosine is acos(Product1 + Product2 + Product3),
  Distance is Arc_cosine * 6371.0,
  print(Distance),nl.

/* find_location/3 finds the latitude and longitude of a location
by leveraging GeoNames triples such as these:

<geoentity_Denver_2169039>	rdfs:label	"Denver"@eng .
<geoentity_Denver_2169039>	<hasLatitude>	"-37.26667"^^<degrees> .
<geoentity_Denver_2169039>	<hasLongitude>	"144.3"^^<degrees> .

Unfortunately, lookup/2 can return the wrong 'Denver' so find_city/3
is better for cities.

*/

find_location(Name,Latitude,Longitude) :-
        lookup(Name,Resource),
        rdf(Resource,yago:'hasLatitude',literal(Latitude)),
        rdf(Resource,yago:'hasLongitude',literal(Longitude)).                                          
                                             

/* find_city/4 finds the latitude and longitude of a city
by leveraging GeoNames triples such as these:

<Denver%2C_Colorado>	owl:sameAs	<http://sws.geonames.org/5419384> .

since, unfortunately, there are many "Denver"-named cities,
so we have to be more specific as find_location/3 will otherwise
find the wrong coordinates.

<geoentity_Denver_5419384>	rdfs:label	"Denver"@eng .
<geoentity_Denver_5419384>	<hasLatitude>	"39.73915"^^<degrees> .
<geoentity_Denver_5419384>	<hasLongitude>	"-104.9847"^^<degrees> .
<geoentity_Denver_5419384>	<hasGeonamesEntityId>	"5419384" 

25 ?- find_city('Denver','Colorado',Lat,Long).
Lat = type(yago:degrees, '39.73915'),
Long = type(yago:degrees, '-104.9847').

*/

find_city(City_Name,State_Name,Latitude,Longitude) :- find_city_resource(City_Name,State_Name,Resource),
                                                      link_resource_to_location(Resource,Latitude,Longitude).

/*

yago('Seattle',Seattle),link_resource_to_location(Seattle,X,Y).
yago('Denver',Denver),link_resource_to_location(Denver,X,Y).

*/

link_resource_to_location(Resource,Latitude,Longitude) :- link_yago_to_geonames(Resource,GeoNames),
                                                          get_geonames_entity_ID(GeoNames,EntityID),
                                                          get_geoentity_resource(EntityID,GeoEntity),
                                                          rdf(GeoEntity,yago:'hasLatitude',literal(type(_, Latitude))),
                                                          rdf(GeoEntity,yago:'hasLongitude',literal(type(_, Longitude))).                                                                                                

/*
   To get the entity ID for geonames from a resource such as http://sws.geonames.org/5419384,
drop the first part.

Example: get_geonames_entity_ID('http://sws.geonames.org/5419384','5419384').
*/

get_geonames_entity_ID(GeoNames,EntityID) :- atom_concat('http://sws.geonames.org/',EntityID,GeoNames).

/* 
  To get geoentity given the entity ID for geonames find the geoentity with the ID given.

   Given "5419384" -->

   <geoentity_Denver_5419384>	<hasGeonamesEntityId>	'5419384'

Example: get_geoentity_resource('5419384',yago:'geoentity_Denver_5419384').

*/

get_geoentity_resource(EntityID,GeoEntity) :- rdf(GeoEntity,yago:'hasGeonamesEntityId',literal(EntityID)).

/*
find_city_resource/3 returns the YAGO resource for a (city,state) combination.
*/

find_city_resource(City_Name,State_Name,Resource) :-
        atom_concat(City_Name,'%2C_',City_and_Comma),
        atom_concat(City_and_Comma,State_Name,City_and_State),
        yago(City_and_State,Resource).

/*
find_from_redirect/3 returns the YAGO resource for a (city,state) combination
if there is a redirect link listed for it from its label.
*/

find_from_redirect(City_Name,State_Name,Resource) :-
        atom_concat(City_Name,', ',City_and_Comma),
        atom_concat(City_and_Comma,State_Name,City_and_State),
        rdf(Resource,yago:redirectedFrom,literal(lang(_,City_and_State))).

/*
find_from_label/3 returns the YAGO resource for a (city,state) combination
if it can be identified by its label.
*/

find_from_label(City_Name,State_Name,Resource) :-
        atom_concat(City_Name,', ',City_and_Comma),
        atom_concat(City_and_Comma,State_Name,City_and_State),
        convert(City_and_State,Resource).

/*

link_yago_to_geonames(resource1,resource2) links a YAGO resource to
its corresponding GeoNames Entity.

yago('Seattle',Seattle),link_yago_to_geonames(Seattle,X).

*/

link_yago_to_geonames(YAGO,GeoNames) :- rdf(YAGO,owl:'sameAs',GeoNames).

/* nearby/2 and nearby/3 provide a rough estimate of whether two
places are near by to each other or not. We default to a distance
of 30 km if the distance is not measured, just as YAGO does. */

% X and Y are both (lat,long) pairs below.
# nearby/2(+X,+Y)
# nearby/3(+X,+Y,+Distance)
nearby(X,Y) :- how_far_apart_are_locations(X,Y,Distance),Distance < 30.0.
nearby(X,Y,Max_Separation) :- how_far_apart_are_locations((X,Y,Distance),Distance < Max_Separation.

has_loc('Denver',39.76185,-104.881105).
has_loc('Centennial',39.590568, -104.869118).
has_loc('Durango',37.273267, -107.871692).
has_loc('Seattle',47.609722, -122.333056).

% X and Y are both (lat,long) pairs below.
how_far_apart_are_locations(Name1,Name2,Distance) :-
        has_loc(Name1,Lat1,Long1),
        has_loc(Name2,Lat2,Long2),
        find_distance_from_angular_degrees(Lat1,Long1,Lat2,Long2,Distance),
        format('The distance from ~s and ~s is ~f kilometers.~n').

/*
  how_far/3 finds the distance between two YAGO resources.

  Given two resources (e.g., cities) how_far finds how apart they are in kilometers, or
  fails if it cannot get the latitude, longitude of one of the resources.

  yago('London',London),yago('Paris',Paris),how_far(London,Paris,Distance).

  yago('Seattle',Seattle),yago('Denver',Denver),how_far(Seattle,Denver,Distance).

*/

how_far(Resource1,Resource2,Distance) :-
        link_resource_to_location(Resource1,Latitude1,Longitude1),
        link_resource_to_location(Resource2,Latitude2,Longitude2),
        find_distance_from_angular_degrees(Latitude1,Longitude1,
                                           Latitude2,Longitude2,
                                           Distance).
/*
  Find out how far apart two cities are in kilometers.

51 ?- how_far_apart_are_these_two_cities('Denver','Colorado','Seattle','Washington',Distance).
Denver, Colorado has location 39.73915 degrees latitude, -104.9847 degrees longitude.
Seattle, Washington has location 47.60621 degrees latitude, -122.33207 degrees longitude.
'acos('0.07169415154078075+0.42317583642008605+0.47213634221909456') = '1641.116019549868
Denver, Colorado is 1641.116020 kilometers from Seattle, Washington,
Distance = 1641.116019549868.

how_far_apart_are_these_two_cities('Denver','Colorado','Durango','Colorado',Distance).

52 ?- how_far_apart_are_these_two_cities('Denver','Colorado','Durango','Colorado',Distance).
Denver, Colorado has location 39.73915 degrees latitude, -104.9847 degrees longitude.
Durango, Colorado has location 37.27528 degrees latitude, -107.88007 degrees longitude.
'acos('0.04857485789050523+0.562534618407175+0.38718493883067107') = '372.15256531523903
Denver, Colorado is 372.152565 kilometers from Durango, Colorado,
Distance = 372.15256531523903.

52 ?- how_far_apart_are_these_two_cities('Denver','Colorado','Aurora','Colorado',Distance).
Denver, Colorado has location 39.73915 degrees latitude, -104.9847 degrees longitude.
Durango, Colorado has location 37.27528 degrees latitude, -107.88007 degrees longitude.
'acos('0.04857485789050523+0.562534618407175+0.38718493883067107') = '372.15256531523903
Denver, Colorado is 372.152565 kilometers from Durango, Colorado,
Distance = 372.15256531523903.

64 ?- how_far_apart_are_these_two_cities('Denver','Colorado','Aurora','Colorado',Distance).
Denver, Colorado has location 39.73915 degrees latitude, -104.9847 degrees longitude.
Aurora, Colorado has location 39.72943 degrees latitude, -104.83192 degrees longitude.
'acos('0.039142504996637635+0.5522427342398387+0.40861264390504215') = '13.108970678615224
Denver, Colorado is 13.108971 kilometers from Aurora, Colorado,
Distance = 13.108970678615224.

Note:

  how_far_apart_are_these_two_cities('Denver','Colorado','Centennial','Colorado',Distance).

fails as there does not appear to be a lat, long for Centennial.

*/

how_far_apart_are_these_two_cities(City_Name1,State_Name1,City_Name2,State_Name2,Distance) :-
        find_city(City_Name1,State_Name1,Latitude1,Longitude1),
        format("~s, ~s has location ~s degrees latitude, ~s degrees longitude.~n",[City_Name1,State_Name1,Latitude1,Longitude1]),
        find_city(City_Name2,State_Name2,Latitude2,Longitude2),
        format("~s, ~s has location ~s degrees latitude, ~s degrees longitude.~n",[City_Name2,State_Name2,Latitude2,Longitude2]),
        atom_number(Latitude1,Latitude1N),
        atom_number(Longitude1,Longitude1N),
        atom_number(Latitude2,Latitude2N),
        atom_number(Longitude2,Longitude2N),        
        find_distance_from_angular_degrees(Latitude1N,Longitude1N,Latitude2N,Longitude2N,Distance),
        format("~s, ~s is ~f kilometers from ~s, ~s,",[City_Name1,State_Name1,City_Name2,State_Name2]).

/*

Example traces:

  Note that these measures are less than kilometer driving distances found by Google
since we assume straight line driving, without diversions for mountains, etc.

27 ?- how_far("Denver","Centennial",D).
'acos('0.039038861161259854+0.553342680744569+0.407613976772893') = '19.073298494882696
"The distance from ""Denver"" to ""Centennial"" is "19.073298494882696" km."
D = 19.073298494882696 

  Google says 22.4 km.

29 ?- how_far("Denver","Durango",D).
'acos('0.04821018604536946+0.5626620979051988+0.3873515399510835') = '379.7780992703726
"The distance from ""Denver"" to ""Durango"" is "379.7780992703726" km."
D = 379.7780992703726 
false.

  Google says 542.1 km.

30 ?- how_far("Denver","Seattle",D).
'acos('0.07118343272569441+0.4232074280373961+0.47238773354134184') = '1646.801745744498
"The distance from ""Denver"" to ""Seattle"" is "1646.801745744498" km."
D = 1646.801745744498 

  Google says 2142.5 km.


35 ?- nearby('Denver','Centennial').
'acos('0.039038861161259854+0.553342680744569+0.407613976772893') = '19.073298494882696
'The distance from ''Denver'' to ''Centennial'' is '19.073298494882696' km.'
true.


36 ?- nearby('Denver','Durango').
'acos('0.04821018604536946+0.5626620979051988+0.3873515399510835') = '379.7780992703726
'The distance from ''Denver'' to ''Durango'' is '379.7780992703726' km.'
false.



*/

/* places_in/2 finds places located within a larger place.

yago('Denver',Place),places_in(Place,Places).

yago('Colorado',Place),places_in(Place,Places).

*/

places_in(Large_Place,Places) :- setof(Place,rdf(Place,yago:'isLocatedIn',Large_Place),Places).

/* 

yago('Colorado',State),cities_in(State,Cities).

*/

cities_in(Large_Place,Cities) :- setof(City,city_in_place(City,Large_Place), Cities).

city_in_place(City,Place) :- rdf(City,yago:'isLocatedIn',Place),
                             rdf(City,rdf:'type',yago:'wordnet_administrative_district_108491826').

city_nearby(City1,Place,City2) :-
        rdf(City1,yago:'isLocatedIn',Place),
        rdf(City1,rdf:'type',yago:'wordnet_administrative_district_108491826'),
        rdf(City2,yago:'isLocatedIn',Place),
        not(City1 = City2),
        rdf(City2,rdf:'type',yago:'wordnet_administrative_district_108491826'),
        nearby(City1,City2,30.0).

/* cities_nearby/3 given a city and the state it is in will list other cities
near by it in the same state.

yago('Denver',Denver),yago('Colorado',CO),cities_nearby(Denver,CO,Nearby)

*/

cities_nearby(City,State,Nearby_Cities) :- setof(Other_City,city_nearby(City,State,Other_City), Nearby_Cities).

:- write('Code for computing distance from geographic locations loaded.'),nl.
