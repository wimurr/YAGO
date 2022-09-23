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
  % print('acos('),
  % print(Product1),
  % print('+'),
  % print(Product2),
  % print('+'),
  % print(Product3),
  % print(') = '),
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
        ensure_is_number(Deg_Lat1,Deg_Lat1N),
        ensure_is_number(Deg_Long1,Deg_Long1N),
        ensure_is_number(Deg_Lat2,Deg_Lat2N),
        ensure_is_number(Deg_Long2,Deg_Long2N),                
        find_distance_from_angular_degrees_ensured_numbers(Deg_Lat1N,Deg_Long1N,Deg_Lat2N,Deg_Long2N,Distance).

find_distance_from_angular_degrees_ensured_numbers(Deg_Lat1,Deg_Long1,Deg_Lat2,Deg_Long2,Distance) :- 
    convert_degrees_to_radians(Deg_Lat1,Lat1),
    convert_degrees_to_radians(Deg_Long1,Long1),
    convert_degrees_to_radians(Deg_Lat2,Lat2),
    convert_degrees_to_radians(Deg_Long2,Long2),
    find_distance_from_angular_radians(Lat1,Long1,Lat2,Long2,Distance).

find_distance_from_angular_radians(Lat1,Long1,Lat2,Long2,Distance) :- 
  Product1 is cos(Lat1)*cos(Long1)*cos(Lat2)*cos(Long2),
  Product2 is cos(Lat1)*sin(Long1)*cos(Lat2)*sin(Long2),
  Product3 is sin(Lat1)*sin(Lat2),
  Arc_cosine is acos(Product1 + Product2 + Product3),
  Distance is Arc_cosine * 6371.0.

/* find_location/3 finds the latitude and longitude of a 
place name.
*/

find_location(Name,Lat,Long) :-
        best_resource_for_name(Name,Resource),
        find_resource_lat_long(Resource,Lat,Long).

:- rdf_meta find_resource_lat_long(r,-,-).

find_resource_lat_long(Resource,Lat,Long) :-
        rdf(Resource,yago:'hasLatitude',Lat^^yago:'degrees'),
        rdf(Resource,yago:'hasLongitude',Long^^yago:'degrees'),
        !.

/*
find_city_resource/3 returns the YAGO resource for a (city,state) combination
if it can be identified by its label. E.g., find_from_label('Seattle','Washington',X)
returns yago:'Seattle, Washington' if there is such a resource.

Many times only a single label is required (e.g., 'Denver', 'Seattle', 'Fort Collins'
and we can just call find_location directly.

Other times, two names are needed. E.g., if we just use 'Durango' we get the
Mexican state, instead we need 'Durango, Colorado' to get that city.

*/

find_city_resource(City_Name,State_Name,Resource) :-
        atomic_list_concat([City_Name,', ',State_Name],City_and_State),
        get_yago_resource(City_and_State,Resource).

/*
Older code, only required if we need to go through geoname entity IDs to lat, long
values, but that seems unnecessary. 

link_yago_to_geonames(resource1,resource2) links a YAGO resource to
its corresponding GeoNames Entity.

yago('Seattle',Seattle),link_yago_to_geonames(Seattle,X).

link_yago_to_geonames(YAGO,GeoNames) :- rdf(YAGO,owl:'sameAs',GeoNames).

has_loc('Denver',39.76185,-104.881105).
has_loc('Centennial',39.590568, -104.869118).
has_loc('Durango',37.273267, -107.871692).
has_loc('Seattle',47.609722, -122.333056).

  how_far/3 finds the distance between two YAGO resources.

  Given two resources (e.g., cities) how_far finds how apart they are in kilometers, or
  fails if it cannot get the latitude, longitude of one of the resources.

  yago('London',London),yago('Paris',Paris),how_far(London,Paris,Distance).

  yago('Seattle',Seattle),yago('Denver',Denver),how_far(Seattle,Denver,Distance).

how_far(Resource1,Resource2,Distance) :-
        link_resource_to_location(Resource1,Latitude1,Longitude1),
        link_resource_to_location(Resource2,Latitude2,Longitude2),
        find_distance_from_angular_degrees(Latitude1,Longitude1,
                                           Latitude2,Longitude2,
                                           Distance).

*/


/* nearby/2 and nearby/3 provide a rough estimate of whether two
places are near by to each other or not. We default to a distance
of 30 km if the distance is not measured, just as YAGO does. */

% X and Y are both (lat,long) pairs below.
% nearby/2(+X,+Y)
% nearby/3(+X,+Y,+Distance)

% In nearby_places we are given names.
nearby_places(X,Y) :- how_far_apart_are_places(X,Y,Distance),Distance < 30.
nearby_places(X,Y,Max_Separation) :- how_far_apart_are_places(X,Y,Distance),Distance < Max_Separation.

:- rdf_meta nearby_resources(r,r).
:- rdf_meta nearby_resources(r,r,+).

% In nearby_resources we are given resources.
nearby_resources(X,Y) :- how_far_apart_are_resources(X,Y,Distance),Distance < 30.
nearby_resources(X,Y,Max_Separation) :- how_far_apart_are_resources(X,Y,Distance),Distance < Max_Separation.

% Given place names, return how apart they are in kilometers.
how_far_apart_are_places(Name1,Name2,Distance) :-
        find_location(Name1,Lat1,Long1),
        find_location(Name2,Lat2,Long2),
        find_distance_from_angular_degrees(Lat1,Long1,Lat2,Long2,Distance),
        format('The distance from ~s and ~s is ~f kilometers.~n').

% Given resources, return how apart they are in kilometers.
how_far_apart_are_resources(Resource1,Resource2,Distance) :-
        find_resource_lat_long(Resource1,Lat1,Long1),
        find_resource_lat_long(Resource2,Lat2,Long2),
        find_distance_from_angular_degrees(Lat1,Long1,Lat2,Long2,Distance).

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

how_far_apart_are_these_places(Name1,Name2,Distance) :-
        find_location(Name1,Latitude1,Longitude1),
        find_location(Name2,Latitude2,Longitude2),        
        format(" ~w has location ~w degrees latitude, ~w degrees longitude.~n",[Name1,Latitude1,Longitude1]),
        format(" ~w has location ~w degrees latitude, ~w degrees longitude.~n",[Name2,Latitude2,Longitude2]),        
        atom_number(Latitude1,Latitude1N),
        atom_number(Longitude1,Longitude1N),
        atom_number(Latitude2,Latitude2N),
        atom_number(Longitude2,Longitude2N),        
        find_distance_from_angular_degrees(Latitude1N,Longitude1N,Latitude2N,Longitude2N,Distance),
        format("~w is ~f kilometers distance in air miles from  ~w.~n",[Name1,Distance,Name2]).

/*

Example traces:

  Note that these measures are less than kilometer driving distances found by Google
since we assume straight line driving, without diversions for mountains, etc.

27 ?- how_far("Denver","Centennial",D).
'acos('0.039038861161259854+0.553342680744569+0.407613976772893') = '19.073298494882696
"The distance from ""Denver"" to ""Centennial"" is "19.073298494882696" km."
D = 19.073298494882696 

  Google says 22.4 km.

29 ?- how_far("Denver","Durango, Colorado",D).
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


36 ?- nearby('Denver','Durango, Colorado').
'acos('0.04821018604536946+0.5626620979051988+0.3873515399510835') = '379.7780992703726
'The distance from ''Denver'' to ''Durango'' is '379.7780992703726' km.'
false.



*/

/* places_in/2 finds places located within a larger place.

yago('Denver',Place),places_in(Place,Places).

yago('Colorado',Place),places_in(Place,Places).

*/

:- rdf_meta places_in(r,-).

places_in(Large_Place,Places) :-
        setof(Place,rdf(Place,yago:'isLocatedIn',Large_Place),Places).

/* 
show_cities_in_state('Colorado').
*/

show_cities_in_state(State_Name) :-
        best_resource_for_name(State_Name,State),
        cities_in(State,Cities),
        length(Cities,N),
        format("Found ~d cities in ~w:~n",[N,State_Name]),
        print_all(Cities).

:- rdf_meta cities_in(r,-).

cities_in(Large_Place,Cities) :-
        setof(City,city_in_place(City,Large_Place), Cities).

:- rdf_meta city_in_place(r,r).

city_in_place(City,Place) :-
        rdf(City,rdf:'type',yago:'wordnet_city_108524735'),
        rdf(City,yago:'isLocatedIn',Place).

city_in_place(City,Place) :-
        rdf(City,rdf:'type',yago:'wordnet_administrative_district_108491826'),
        rdf(City,yago:'isLocatedIn',Place).

/* cities_nearby/3 given a city and the state it is in will list other cities
near by it in the same state, within Radius kilometers (air miles).

cities_nearby(yago:'Denver',yago:'Colorado',Nearby,40).

*/

:- rdf_meta cities_nearby(r,r,-).

cities_nearby(City,State,Nearby_Cities,Radius) :-
        setof(Other_City,(city_in_place(Other_City,State),nearby_resources(City,Other_City,Radius)), Nearby_Cities).

show_cities_nearby(City_Name,State_Name,Radius) :-
        best_resource_for_name(City_Name,City),
        best_resource_for_name(State_Name,State),
        cities_in(State,Cities_In_State),
        length(Cities_In_State,N),
        format("Found ~d cities in ~w:~n",[N,State_Name]),
        print_all(Cities_In_State),
        findall(Other_City,(member(Other_City,Cities_In_State),nearby_resources(City,Other_City,Radius)),Other_Cities),
        length(Other_Cities,K),
        format("Found ~d cities in ~w  within ~d kilometers of ~w:~n",[K,State_Name,Radius,City_Name]),
        print_resources(Other_Cities),
        !.

:- write('Code for computing distance from geographic locations loaded.'),nl.
